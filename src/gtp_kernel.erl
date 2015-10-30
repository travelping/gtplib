%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% Copyright 2015, Travelping GmbH <info@travelping.com>

-module(gtp_kernel).

-behavior(gen_server).

%% API
-export([dev_create/4, create_pdp_context/6, delete_pdp_context/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("kernel/include/file.hrl").
-include_lib("gen_netlink/include/netlink.hrl").

-record(state, {ns, gtp_nl, rt_nl, rt_nl_ns, gtp_genl_family, gtp_ifidx}).

%%====================================================================
%% API
%%====================================================================

-spec dev_create(Device     :: binary() | list(),
		 FD0        :: non_neg_integer(),
		 FD1u       :: non_neg_integer(),
		 Opts       :: [term()]) -> ok | {error, _}.

dev_create(Device, FD0, FD1u, Opts) ->
    gen_server:start_link(?MODULE, [Device, FD0, FD1u, Opts], []).

create_pdp_context(Server, Version, SGSN, MS, LocalTEI, RemoteTEI) ->
    gen_server:call(Server, {create_pdp_context, Version, SGSN, MS, LocalTEI, RemoteTEI}).

delete_pdp_context(Server, Version, SGSN, MS, LocalTEI, RemoteTEI) ->
    gen_server:call(Server, {delete_pdp_context, Version, SGSN, MS, LocalTEI, RemoteTEI}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Device, FD0, FD1u, Opts]) ->
    {ok, FDesc} = get_ns_fd(Opts),
    #file_descriptor{module = prim_file,
		     data   = {_Port, NsFd}} = FDesc,

    {ok, RtNl} = socket(netlink, raw, ?NETLINK_ROUTE, []),
    ok = gen_socket:bind(RtNl, netlink:sockaddr_nl(netlink, 0, -1)),

    {ok, RtNlNs} = socket(netlink, raw, ?NETLINK_ROUTE, Opts),
    ok = gen_socket:bind(RtNlNs, netlink:sockaddr_nl(netlink, 0, -1)),
    ok = netlink:setsockopt(RtNlNs, sol_netlink, netlink_add_membership, rtnlgrp_link),

    CreateGTPLinkInfo = [{fd0, FD0}, {fd1, FD1u}, {hashsize, 131072}],
    CreateGTPData = netlink:linkinfo_enc(inet, "gtp", CreateGTPLinkInfo),
    CreateGTPMsg = {inet,arphrd_none, 0, [up], [up],
		    [{net_ns_fd, NsFd},
		     {ifname,    Device},
		     {linkinfo,[{kind, "gtp"},
				{data, CreateGTPData}]}]},
    CreateGTPReq = #rtnetlink{type  = newlink,
			      flags = [create,excl,ack,request],
			      seq   = erlang:unique_integer([positive]),
			      pid   = 0,
			      msg   = CreateGTPMsg},
    lager:debug("CreateGTPReq: ~p", [CreateGTPReq]),

    ok = nl_simple_request(RtNl, ?NETLINK_ROUTE, CreateGTPReq),

    {ok, GtpIfIdx} = wait_for_interface(RtNlNs, Device),

    Routes = proplists:get_value(routes, Opts, []),
    lists:foreach(fun(R) -> add_route(RtNlNs, GtpIfIdx, R) end, Routes),

    {ok, GtpGenlFam} = get_family("gtp"),
    {ok, GtpNl} = socket(netlink, raw, ?NETLINK_GENERIC, []),
    ok = gen_socket:bind(GtpNl, netlink:sockaddr_nl(netlink, 0, 0)),

    {ok, #state{ns = NsFd, gtp_nl = GtpNl, rt_nl = RtNl, rt_nl_ns = RtNlNs, gtp_genl_family = GtpGenlFam, gtp_ifidx = GtpIfIdx}}.

handle_call({create_pdp_context, Version, SGSN, MS, LocalTID, RemoteTID},
	    _From, #state{ns = NsFd, gtp_nl = GtpNl, gtp_genl_family = GtpGenlFam,
			  gtp_ifidx = GtpIfIdx} = State) ->
    lager:debug("create_pdp_context: ~w, ~w, ~w, ~w, ~w", [Version, SGSN, MS, LocalTID, RemoteTID]),

    GtpReqAttrs = [{version,      Version},
		   {net_ns_fd,    NsFd},
		   {link,         GtpIfIdx},
		   {sgsn_address, SGSN},
		   {ms_address,   MS},
		   {i_tid,        LocalTID},                  %% TODO: GTPv0 TID and FLOW
		   {o_tid,        RemoteTID}],
    GtpReq = {new, 0, 0, GtpReqAttrs},
    Req = #netlink{type  = gtp,
		   flags = [?NLM_F_EXCL, ack, request],
		   seq   = erlang:unique_integer([positive]),
		   pid   = 0,
		   msg   = GtpReq},
    lager:debug("create_pdp_context: ~p", [Req]),
    ok = nl_simple_request(GtpNl, GtpGenlFam, Req),

    {reply, ok, State};

handle_call({delete_pdp_context, Version, SGSN, MS, LocalTID, _RemoteTID},
	    _From, #state{ns = NsFd, gtp_nl = GtpNl, gtp_genl_family = GtpGenlFam,
			  gtp_ifidx = GtpIfIdx} = State) ->
    lager:debug("delete_pdp_context: ~w, ~w, ~w, ~w, ~w", [Version, SGSN, MS, LocalTID, _RemoteTID]),

    GtpReqAttrs = [{version,      Version},
		   {net_ns_fd,    NsFd},
		   {link,         GtpIfIdx},
		   {sgsn_address, SGSN},
		   {ms_address,   MS},
		   {i_tid,        LocalTID}],                  %% TODO: GTPv0 TID and FLOW
    GtpReq = {delete, 0, 0, GtpReqAttrs},
    Req = #netlink{type  = gtp,
		   flags = [?NLM_F_EXCL, ack, request],
		   seq   = erlang:unique_integer([positive]),
		   pid   = 0,
		   msg   = GtpReq},
    lager:debug("delete_pdp_context: ~p", [Req]),
    ok = nl_simple_request(GtpNl, GtpGenlFam, Req),

    {reply, ok, State};

handle_call(Request, _From, State) ->
    lager:warning("handle_call: ~p", [lager:pr(Request, ?MODULE)]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:debug("handle_cast: ~p", [lager:pr(Msg, ?MODULE)]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:debug("handle_info: ~p", [lager:pr(Info, ?MODULE)]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-define(SELF_NET_NS, "/proc/self/ns/net").
-define(SIOCGIFINDEX, 16#8933).

get_ns_fd(Opts) ->
    try
	{netns, NetNs} = lists:keyfind(netns, 1, Opts),
	{ok, _} = file:open(filename:join("/var/run/netns", NetNs), [raw, read])
    catch
	_:_ ->
	    {ok, _} = file:open(?SELF_NET_NS, [raw, read])
    end.

%% get_ifindex(Name, Opts) when is_list(Name) ->
%%     get_ifindex(iolist_to_binary(Name), Opts);
%% get_ifindex(Name, Opts) ->
%%     {ok, S} = raw_socket(local, dgram, default, Opts),
%%     {ok, <<_:16/binary, Index:32/native-integer, _/binary>>} = gen_socket:ioctl(S, ?SIOCGIFINDEX, <<Name/binary,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>),
%%     gen_socket:close(S),
%%     Index.

%% raw_socket(Family, Type, Protocol, Opts) ->
%%     case proplists:get_value(netns, Opts) of
%%         undefined ->
%%             gen_socket:raw_socket(Family, Type, Protocol);
%%         NetNs ->
%%             gen_socket:raw_socketat(NetNs, Family, Type, Protocol)
%%     end.

socket(Family, Type, Protocol, Opts) ->
    case proplists:get_value(netns, Opts) of
        undefined ->
            gen_socket:socket(Family, Type, Protocol);
        NetNs ->
            gen_socket:socketat(NetNs, Family, Type, Protocol)
    end.

get_family(Family) ->
    {ok, S} = gen_socket:socket(netlink, raw, ?NETLINK_GENERIC),

    Get = {getfamily, 1, 0, [{family_id, generic}, {family_name, Family}]},
    Seq = erlang:unique_integer([positive]),
    Req = #netlink{type  = ctrl,
		   flags = [ack,request],
		   seq   = Seq,
		   pid   = 0,
		   msg = Get},
    Repl = do_request(S, ?NETLINK_GENERIC, Req, fun nl/2, []),
    gen_socket:close(S),
    lager:debug("Request: ~p", [Req]),
    lager:debug("Request: ~p", [Repl]),

    case Repl of
	[#netlink{type = ctrl, seq = Seq, msg = {newfamily, _, _, Attrs}}|_] ->
	    {_, FamilyId} = lists:keyfind(family_id, 1, Attrs),
	    {ok, FamilyId};
	Other ->
	    lager:error("unknown genl family: ~p", [Other]),
	    {error, unknown}
    end.

wait_for_interface(Socket, Device, []) ->
    wait_for_interface(Socket, Device);
wait_for_interface(Socket, Device,
		   [#rtnetlink{type = newlink,
			       msg = {_, _, Index, _, _, Attrs}}|T]) ->
    case lists:keyfind(ifname, 1, Attrs) of
	{_, Device} ->
	    {ok, Index};
	_Other ->
	    wait_for_interface(Socket, Device, T)
    end;
wait_for_interface(Socket, Device, [_|T]) ->
    wait_for_interface(Socket, Device, T).

wait_for_interface(Socket, Device) ->
    ok = gen_socket:input_event(Socket, true),
    receive
	{Socket, input_ready} ->
	    case gen_socket:recvfrom(Socket, 128 * 1024) of
		{ok, _Sender, Data} ->
		    wait_for_interface(Socket, Device, netlink:nl_rt_dec(Data));
		Other ->
		    lager:error("~p~n", [Other]),
		    Other
	    end
    after 5000 ->
	    {error, timeout}
    end.

add_route(Socket, IfIdx, {{_,_,_,_} = IP, Len}) ->
    Seq = erlang:unique_integer([positive]),
    Msg = {inet, Len, 0, 0, main, static, universe, unicast, [],
	   [{dst,IP}, {oif,IfIdx}]},
    Req = #rtnetlink{type  = newroute,
		     flags = [create,ack,request],
		     seq   = Seq,
		     pid   = 0,
		     msg   = Msg},
    nl_simple_request(Socket, ?NETLINK_ROUTE, Req).

nl_simple_request(Socket, Protocol, #rtnetlink{seq = Seq} = Req)  ->
    case do_request(Socket, Protocol, Req, fun nl/2, []) of
	[#rtnetlink{type = error, seq = Seq, msg = {Code, _}}|_] when Code == 0 ->
	    ok;
	[#rtnetlink{type = error, seq = Seq, msg = {Code, _}}|_] ->
	    {error, Code};
	[Other|_] ->
	    Other
    end;
nl_simple_request(Socket, Protocol, #netlink{seq = Seq} = Req)  ->
    case do_request(Socket, Protocol, Req, fun nl/2, []) of
	[#netlink{type = error, seq = Seq, msg = {Code, _}}|_] when Code == 0 ->
	    ok;
	[#netlink{type = error, seq = Seq, msg = {Code, _}}|_] ->
	    {error, Code};
	[Other|_] ->
	    Other
    end.

do_request(Socket, Protocol, Req, Cb, CbState) ->
    BinReq = netlink:nl_enc(Protocol, Req),
    gen_socket:send(Socket, BinReq),
    process_answer(Socket, Protocol, Cb, CbState).

process_answer(Socket, Protocol, Cb, CbState0) ->
    case gen_socket:recv(Socket, 16 * 1024 * 1024) of
        {ok, Data} ->
            Msg = netlink:nl_dec(Protocol, Data),
            case process_nl(false, Msg, Cb, CbState0) of
                {continue, CbState1} ->
                    process_answer(Socket, Protocol, Cb, CbState1);
                CbState1 ->
                    CbState1
            end;
        Other ->
            io:format("Other: ~p~n", [Other]),
            Other
    end.

process_nl(true, [], _Cb, CbState) ->
    {continue, CbState};
process_nl(_, [], _Cb, CbState) ->
    CbState;
process_nl(_Multi, [#netlink{type = done}], _Cb, CbState) ->
    CbState;
process_nl(_Multi, [Head|Rest], Cb, CbState0) ->
    CbState1 = Cb(Head, CbState0),
    Flags = element(3, Head),
    process_nl(lists:member(multi, Flags), Rest, Cb, CbState1).

nl(Msg, Acc) ->
    [Msg|Acc].
