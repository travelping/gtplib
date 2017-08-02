%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% Copyright 2015, Travelping GmbH <info@travelping.com>

-module(gtp_packet).

-export([encode/1, encode_ies/1,
	 decode/1, decode/2, decode_ies/1, decode_ies/2,
	 msg_description/1, msg_description_v2/1]).
-compile(export_all).
-compile([{parse_transform, cut},
	  bin_opt_info]).

-include("gtp_packet.hrl").

-define(V2_INDICATION_FLAGS, ['DAF', 'DTF', 'HI', 'DFI', 'OI', 'ISRSI', 'ISRAI', 'SGWCI',
			      'SQCI', 'UIMSI', 'CFSI', 'CRSI', 'P', 'PT', 'SI', 'MSV',
			      'RetLoc', 'PBIC', 'SRNI', 'S6AF', 'S4AF', 'MBMDT', 'ISRAU', 'CCRSI',
			      'CPRAI', 'ARRL', 'PPOF', 'PPON/PPEI', 'PPSI', 'CSFBI', 'CLII', 'CPSR',
			      'Spare', 'Spare', 'Spare', 'Spare', 'PSCI', 'PCRI', 'AOSI', 'AOPI']).

%%====================================================================
%% API
%%====================================================================

decode(Data) ->
    decode(Data, #{ies => map}).

decode(Data, Opts) ->
    Msg = decode_header(Data),
    decode_ies(Msg, Opts).

decode_ies(Msg) ->
    decode_ies(Msg, #{ies => map}).

decode_ies(#gtp{ie = IEs} = Msg, #{ies := map})
  when is_map(IEs) ->
    Msg;
decode_ies(#gtp{ie = IEs} = Msg, #{ies := Format} = Opts)
  when not is_binary(IEs) orelse (Format /= map andalso Format /= binary) ->
    error(badargs, [Msg, Opts]);
decode_ies(#gtp{version = v1, type = Type, ie = IEs} = Msg, #{ies := map}) ->
    Msg#gtp{ie = decode_v1(Type, IEs)};
decode_ies(#gtp{version = v2, ie = IEs} = Msg, #{ies := map}) ->
    Msg#gtp{ie = decode_v2(IEs)};
decode_ies(Msg, _) ->
    Msg.

encode(#gtp{version = v1, type = Type, tei = TEI, seq_no = SeqNo,
	    n_pdu = NPDU, ext_hdr = ExtHdr, ie = IEs}) ->
    Flags = encode_gtp_v1_hdr_flags(SeqNo, NPDU, ExtHdr),
    HdrOpt = encode_gtp_v1_opt_hdr(SeqNo, NPDU, ExtHdr),
    Data = encode_v1(Type, IEs),
    <<Flags/binary, (message_type_v1(Type)):8, (size(HdrOpt) + size(Data)):16, TEI:32, HdrOpt/binary, Data/binary>>;


encode(#gtp{version = v2, type = Type, tei = TEI, seq_no = SeqNo, ie = IEs}) ->
    encode_v2_msg(message_type_v2(Type), 0, TEI, SeqNo, encode_v2(IEs)).

encode_ies(#gtp{version = v1, type = Type, ie = IEs} = Msg) ->
    Msg#gtp{ie = encode_v1(Type, IEs)};
encode_ies(#gtp{version = v2, ie = IEs} = Msg) ->
    Msg#gtp{ie = encode_v2(IEs)}.

%%====================================================================
%% Helpers
%%====================================================================

decode_header(<<1:3, 1:1, _:1, E:1, S:1, PN:1, Type:8, Length:16, TEI:32/integer,
		SeqNo0:16, NPDU0:8, ExtHdrType:8, Data0/binary>>)
  when E == 1; S == 1; PN == 1 ->
    DataLen = Length - 4,
    <<Data1:DataLen/bytes, _Next/binary>> = Data0,
    SeqNo = case S of
		1 -> SeqNo0;
		_ -> undefined
	    end,
    NPDU = case PN of
	       1 -> NPDU0;
	       _ -> undefined
	   end,
    {IEs, ExtHdr} = case E of
			 1 -> decode_exthdr(ExtHdrType, Data1, []);
			 _ -> {Data1, []}
		     end,
    #gtp{version = v1, type = message_type_v1(Type), tei = TEI, seq_no = SeqNo,
	 n_pdu = NPDU, ext_hdr = ExtHdr, ie = IEs};

decode_header(<<1:3, 1:1, _:1, 0:1, 0:1, 0:1, Type:8, Length:16, TEI:32/integer,
		Data0/binary>>) ->
    <<IEs:Length/bytes, _Next/binary>> = Data0,
    #gtp{version = v1, type = message_type_v1(Type), tei = TEI, ie = IEs};

decode_header(Data = <<2:3, 0:1, _T:1, _Spare0:3, _/binary>>) ->
    decode_v2_msg(Data);
decode_header(Data = <<2:3, 1:1, _T:1, _Spare0:3, _/binary>>) ->
    decode_v2_msg(Data, []).

decode_v2_msg(Data = <<2:3, _P:1, _T:1, _Spare0:3, _Type:8, Length:16, _/binary>>, Acc) ->
    MsgLen = Length + 4,
    <<Msg:MsgLen/bytes, Next/binary>> = Data,
    decode_v2_msg(Next, [decode(Msg) | Acc]);
decode_v2_msg(Data, Acc) ->
    {lists:reverse(Acc), Data}.

decode_v2_msg(<<2:3, _:1, 1:1, _Spare0:3, Type:8, Length:16,
		TEI:32/integer, SeqNo:24, _Spare1:8, Data0/binary>>) ->
    DataLen = Length - 8,
    <<IEs:DataLen/bytes, _Next/binary>> = Data0,
    #gtp{version = v2, type = message_type_v2(Type), tei = TEI, seq_no = SeqNo, ie = IEs};
decode_v2_msg(<<2:3, _:1, 0:1, _Spare0:3, Type:8, Length:16,
		SeqNo:24, _Spare1:8, Data0/binary>>) ->
    DataLen = Length - 4,
    <<IEs:DataLen/bytes, _Next/binary>> = Data0,
    #gtp{version = v2, type = message_type_v2(Type), tei = undefined, seq_no = SeqNo, ie = IEs}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-define(PPP_IPCP,        16#8021).      %% IP Control Protocol
-define(PPP_LCP,         16#c021).      %% Link Control Protocol
-define(PPP_PAP,         16#c023).      %% Password Authentication Protocol
-define(PPP_CHAP,        16#c223).      %% Cryptographic Handshake Auth. Protocol

pad_length(Width, Length) ->
    (Width - Length rem Width) rem Width.

%%
%% pad binary to specific length
%%   -> http://www.erlang.org/pipermail/erlang-questions/2008-December/040709.html
%%
pad_to(Width, Binary) ->
    case pad_length(Width, size(Binary)) of
        0 -> Binary;
        N -> <<Binary/binary, 0:(N*8)>>
    end.

put_ie(IE, IEs) ->
    Key = {element(1, IE), element(2, IE)},
    UpdateFun = fun(V) when is_list(V) -> [IE | V];
		   (undefined)         -> IE;
		   (V)                 -> [IE, V]
		end,
    maps:update_with(Key, UpdateFun, IE, IEs).

bool2int(false) -> 0;
bool2int(true)  -> 1.

encode_flag(Flag, Flags) ->
    bool2int(proplists:get_bool(Flag, Flags)).

is_bin(Bin) -> bool2int(is_binary(Bin)).

maybe_bin(0, _, Bin, _, IE) ->
    {IE, Bin};
maybe_bin(1, Len, Bin, Pos, IE) ->
    <<V:Len/bytes, Rest/binary>> = Bin,
    {setelement(Pos, IE, V), Rest}.

maybe_bin(Bin, IE) when is_binary(Bin) ->
    <<IE/binary, Bin/binary>>;
maybe_bin(_, IE) ->
    IE.

decode_exthdr(0, Data, Hdrs) ->
    {Data, Hdrs};
decode_exthdr(Type, <<Length, Rest/binary>>, Hdrs) ->
    HdrLen = Length * 4 - 2,
    <<HdrData:HdrLen/bytes, NextType:8, Data/binary>> = Rest,
    Hdr = decode_exthdr_type(Type, HdrData),
    decode_exthdr(NextType, Data, [Hdr|Hdrs]).

decode_exthdr_type(2#00100000, <<Class:8, _/binary>>) ->
    %% Service Class Indicator
    {service_class, Class};
decode_exthdr_type(2#01000000, <<Port:16, _/binary>>) ->
    %% UDP Port
    {udp_port, Port};
decode_exthdr_type(2#10000001, Container) ->
    %% RAN Container
    {ran_container, Container};
decode_exthdr_type(2#10000010, <<_:6, PDU:18, _/binary>>) ->
    %% Long PDCP PDU Number
    {long_pdcp_pdu_number, PDU};
decode_exthdr_type(2#11000000, <<PDU:16, _/binary>>) ->
    %% PDCP PDU Number
    {pdcp_pdu_number, PDU};
decode_exthdr_type(Type, Data) ->
    {Type, Data}.

decode_tbcd(Bin) ->
    decode_tbcd(Bin, <<>>).

tbcd_to_string(10)  -> $*;
tbcd_to_string(11)  -> $#;
tbcd_to_string(12)  -> $a;
tbcd_to_string(13)  -> $b;
tbcd_to_string(14)  -> $c;
tbcd_to_string(BCD) -> BCD + $0.

decode_tbcd(<<>>, BCD) ->
    BCD;
decode_tbcd(<<_:4, 15:4, _/binary>>, BCD) ->
    BCD;
decode_tbcd(<<15:4, Lo:4, _/binary>>, BCD) ->
    <<BCD/binary, (tbcd_to_string(Lo))>>;
decode_tbcd(<<Hi:4, Lo:4, Next/binary>>, BCD) ->
    decode_tbcd(Next, <<BCD/binary, (tbcd_to_string(Lo)), (tbcd_to_string(Hi))>>).

decode_v1_rai(Instance, <<MCCHi:8, MNC3:4, MCC3:4, MNCHi:8, LAC:16, RAC:8>>) ->
    #routeing_area_identity{
       instance = Instance,
       mcc = decode_tbcd(<<MCCHi:8, 15:4, MCC3:4>>),
       mnc = decode_tbcd(<<MNCHi:8, 15:4, MNC3:4>>),
       lac = LAC,
       rac = RAC}.

decode_v1_uli(Instance, <<Type:8, MCCHi:8, MNC3:4, MCC3:4, MNCHi:8, LAC:16, Info:16, _/binary>>) ->
    ULI = #user_location_information{
	     instance = Instance,
	     type = Type,
	     mcc = decode_tbcd(<<MCCHi:8, 15:4, MCC3:4>>),
	     mnc = decode_tbcd(<<MNCHi:8, 15:4, MNC3:4>>),
	     lac = LAC
	    },
    case Type of
	0 -> ULI#user_location_information{ci = Info};
	1 -> ULI#user_location_information{sac = Info};
	2 -> ULI#user_location_information{rac = Info bsr 8};
	_ -> ULI
    end.

decode_apn(APN) ->
    [ Part || <<Len:8, Part:Len/bytes>> <= APN ].

decode_isdn_address_string(<<>>) ->
    {isdn_address, 1, 1, 1, <<"000000000000000">>};
decode_isdn_address_string(<<Extension:1, Nature:3, Plan:4, Number/binary>>) ->
    {isdn_address, Extension, Nature, Plan, decode_tbcd(Number)}.

decode_protocol_ppp_opt(Id, Data) ->
    ppp_frame:decode(<<Id:16, Data/binary>>).

%% GSM 09.60 version 6.1.0 Release 1997
decode_protocol_config_opts(<<0:1, Protocol:7, Opts/binary>>) ->
    {{rel97, Protocol}, Opts};
decode_protocol_config_opts(<<1:1, _Spare:4, Protocol:3, Opts/binary>>) ->
    {Protocol, decode_protocol_opts(Protocol, Opts, [])}.

decode_protocol_opts(_Protocol, <<>>, Opts) ->
    lists:reverse(Opts);
decode_protocol_opts(Protocol, <<Id:16, Length:8, Data:Length/bytes, Next/binary>>, Opts)
  when Protocol == 0, Id >= 16#8000, Id < 16#FF00 ->
    Opt = decode_protocol_ppp_opt(Id, Data),
    decode_protocol_opts(Protocol, Next, [Opt | Opts]);
decode_protocol_opts(_Protocol, <<Id:16, Length:8, Data:Length/bytes, Next/binary>>, Opts) ->
    decode_protocol_opts(-1, Next, [{Id, Data} | Opts]).

decode_v1(g_pdu, Data) ->
    %% G-PDU
    Data;
decode_v1(_, Data) ->
    decode_v1(Data, -1, 0, #{ {recovery, 0} => undefined }).

v1_instance(CurrId, PrevId, PrevInst)
  when CurrId == PrevId ->
    PrevInst + 1;
v1_instance(_CurrId, _PrevId, _PrevInst) ->
    0.

decode_v2_indication_flags(<<>>, _, Acc) ->
    Acc;
decode_v2_indication_flags(_, [], Acc) ->
    Acc;
decode_v2_indication_flags(<<0:1, Next/bitstring>>, [_ | Flags], Acc) ->
    decode_v2_indication_flags(Next, Flags, Acc);
decode_v2_indication_flags(<<1:1, Next/bitstring>>, [F | Flags], Acc) ->
    decode_v2_indication_flags(Next, Flags, [F | Acc]).

decode_v2_indication_flags(Flags) ->
    decode_v2_indication_flags(pad_to(5, Flags), ?V2_INDICATION_FLAGS, []).

decode_v2_user_location_information(Instance,
				    <<_:2, FlagLAI:1, FlagECGI:1,
				      FlagTAI:1, FlagRAI:1, FlagSAI:1, FlagCGI:1,
				      Rest0/binary>>) ->
    IE0 = #v2_user_location_information{instance = Instance},
    {IE1, Rest1} = maybe_bin(FlagCGI,  7, Rest0, #v2_user_location_information.cgi,  IE0),
    {IE2, Rest2} = maybe_bin(FlagSAI,  7, Rest1, #v2_user_location_information.sai,  IE1),
    {IE3, Rest3} = maybe_bin(FlagRAI,  7, Rest2, #v2_user_location_information.rai,  IE2),
    {IE4, Rest4} = maybe_bin(FlagTAI,  5, Rest3, #v2_user_location_information.tai,  IE3),
    {IE5, Rest5} = maybe_bin(FlagECGI, 7, Rest4, #v2_user_location_information.ecgi, IE4),
    {IE6, _} = maybe_bin(FlagLAI,  5, Rest5, #v2_user_location_information.lai,  IE5),
    IE6.

decode_v2_fully_qualified_tunnel_endpoint_identifier(Instance,
						     <<FlagV4:1, FlagV6:1, InterfaceType:6,
						       Key:32, Rest0/binary>>) ->
    IE0 = #v2_fully_qualified_tunnel_endpoint_identifier{
	     instance = Instance,
	     interface_type = InterfaceType,
	     key = Key},
    {IE1, Rest1} = maybe_bin(FlagV4,  4, Rest0, #v2_fully_qualified_tunnel_endpoint_identifier.ipv4,  IE0),
    {IE2, _} = maybe_bin(FlagV6, 16, Rest1, #v2_fully_qualified_tunnel_endpoint_identifier.ipv6,  IE1),
    IE2.

decode_v2_mccmnc(Instance, <<MCCHi:8, MNC3:4, MCC3:4, MNCHi:8, _/binary>>) ->
    #v2_serving_network{
       instance = Instance,
       mcc = decode_tbcd(<<MCCHi:8, 15:4, MCC3:4>>),
       mnc = decode_tbcd(<<MNCHi:8, 15:4, MNC3:4>>)
      }.

decode_v2(Data) ->
    decode_v2(Data, #{ {v2_recovery, 0} => undefined }).

decode_v2(<<>>, IEs) ->
    IEs;
decode_v2(<<Type:8, Length:16/integer, _Spare:4, Instance:4, Data:Length/bytes, Next/binary>>, IEs) ->
    IE = decode_v2_element(Type, Instance, Data),
    decode_v2(Next, put_ie(IE, IEs));
decode_v2(<<Type:8, _Length:16/integer, _Spare:4, Instance:4, Data/binary>>, IEs) ->
    decode_v2(<<>>, put_ie({Type, Instance, Data}, IEs));
decode_v2(Data, IEs) ->
    decode_v2(<<>>, put_ie({undecoded, 0, Data}, IEs)).

decode_v2_grouped(Bin) ->
    decode_v2(Bin, #{}).

encode_ie_map(_Fun, _K, undefined, IEs) ->
    IEs;
encode_ie_map(Fun, _K, V, IEs) when is_list(V) ->
    lists:foldl(fun(IE, Acc) -> [Fun(IE)|Acc] end, IEs, V);
encode_ie_map(Fun, _K, V, IEs) ->
    [Fun(V)|IEs].

encode_v1_element(Id, Instance, Bin) when Id < 128 ->
    {{Id, Instance}, <<Id:8, Bin/binary>>};
encode_v1_element(Id, Instance, Bin) ->
    Size = byte_size(Bin),
    {{Id, Instance}, <<Id:8, Size:16, Bin/binary>>}.

encode_v1(_, IEs) when is_binary(IEs) ->
    IEs;
%% encode_v1(g_pdu, Data) when is_binary(Data) ->
%%     Data;
encode_v1(_, IEs) when is_list(IEs) ->
    Data = lists:keysort(1, [encode_v1_element(IE) || IE <- IEs]),
    << <<V/binary>> || {_Id, V} <- Data >>;
encode_v1(_, IEs) when is_map(IEs) ->
    Data = lists:keysort(1, maps:fold(encode_ie_map(fun encode_v1_element/1, _, _, _), [], IEs)),
    << <<V/binary>> || {_Id, V} <- Data >>.

encode_v2_element(Id, Instance, Bin) ->
    Size = byte_size(Bin),
    {{Id, Instance}, <<Id:8, Size:16, 0:4, Instance:4, Bin/binary>>}.

encode_v2(IEs) when is_binary(IEs) ->
    IEs;
encode_v2(IEs) when is_list(IEs) ->
    Data = [encode_v2_element(IE) || IE <- IEs],
    << <<V/binary>> || {_Id, V} <- Data >>;
encode_v2(IEs) when is_map(IEs) ->
    Data = lists:keysort(1, maps:fold(encode_ie_map(fun encode_v2_element/1, _, _, _), [], IEs)),
    << <<V/binary>> || {_Id, V} <- Data >>.

encode_v2_grouped(IEs) ->
    encode_v2(IEs).

encode_v2_msg(Type, P, TEI, SeqNo, IEs)
  when is_integer(TEI) ->
    <<2:3, P:1, 1:1, 0:3, Type:8, (size(IEs) + 8):16, TEI:32, SeqNo:24, 0:8, IEs/binary>>;
encode_v2_msg(Type, P, _TEI, SeqNo, IEs) ->
    <<2:3, P:1, 0:1, 0:3, Type:8, (size(IEs) + 4):16, SeqNo:24, 0:8, IEs/binary>>.

opt_int(I) when is_integer(I) -> I;
opt_int(_)                    -> 0.

hdr_flag(undefined) -> 0;
hdr_flag([])        -> 0;
hdr_flag(_)         -> 1.

encode_gtp_v1_hdr_flags(SeqNo, NPDU, ExtHdr) ->
    <<1:3, 1:1, 0:1, (hdr_flag(ExtHdr)):1, (hdr_flag(SeqNo)):1, (hdr_flag(NPDU)):1>>.

encode_gtp_v1_opt_hdr(SeqNo, NPDU, ExtHdr)
  when SeqNo /= undefined;
       NPDU /= undefined;
       (ExtHdr /= undefined andalso ExtHdr /= []) ->
    << (opt_int(SeqNo)):16, (opt_int(NPDU)):8, (encode_exthdr(ExtHdr, <<>>))/binary>>;
encode_gtp_v1_opt_hdr(_SeqNo, _NPDU, _ExtHdr) ->
    <<>>.

encode_exthdr([], Bin) ->
    <<Bin/binary, 0>>;
encode_exthdr([V|T], Bin) ->
    {HdrType, Data} = encode_exthdr_type(V),
    Hdr = case (pad_length(4, size(Data)) + 2) rem 4 of
	      0 -> Data;
	      N -> <<Data/binary, 0:(N*8)>>
	  end,
    encode_exthdr(T, <<Bin/binary, HdrType:8, ((size(Hdr) + 2) div 4):8, Hdr/binary>>).

encode_exthdr_type({service_class, Class}) ->
    %% Service Class Indicator
    {2#00100000, <<Class:8>>};
encode_exthdr_type({udp_port, Port}) ->
    %% UDP Port
    {2#01000000, <<Port:16>>};
encode_exthdr_type({ran_container, Container}) ->
    %% RAN Container
    {2#10000001, Container};
encode_exthdr_type({long_pdcp_pdu_number, PDU}) ->
    %% Long PDCP PDU Number
    {2#10000010, <<0:6, PDU:18>>};
encode_exthdr_type({pdcp_pdu_number, PDU}) ->
    %% PDCP PDU Number
    {2#11000000, <<PDU:16>>};
encode_exthdr_type({Type, Hdr} = V)
  when is_integer(Type), is_binary(Hdr) ->
    V.

encode_tbcd(Number) ->
    encode_tbcd(Number, <<>>).

string_to_tbcd($*) -> 10;
string_to_tbcd($#) -> 11;
string_to_tbcd($a) -> 12;
string_to_tbcd($b) -> 13;
string_to_tbcd($c) -> 14;
string_to_tbcd(15) -> 15;
string_to_tbcd(BCD) -> BCD - $0.

encode_tbcd(<<>>, BCD) ->
    BCD;
encode_tbcd(<<D:8>>, BCD) ->
    <<BCD/binary, 2#1111:4, (string_to_tbcd(D)):4>>;
encode_tbcd(<<H:8, L:8, Next/binary>>, BCD) ->
    encode_tbcd(Next, <<BCD/binary, (string_to_tbcd(L)):4, (string_to_tbcd(H)):4>>).

encode_v1_rai(#routeing_area_identity{
		 mcc = MCC,
		 mnc = MNC,
		 lac = LAC,
		 rac = RAC}) ->
    [MCC1, MCC2, MCC3 | _] = [ string_to_tbcd(X) || <<X:8>> <= MCC] ++ [15,15,15],
    [MNC1, MNC2, MNC3 | _] = [ string_to_tbcd(X) || <<X:8>> <= MNC] ++ [15,15,15],
    <<MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4, LAC:16, RAC:8>>.


encode_v1_uli(#user_location_information{
		 type = Type,
		 mcc = MCC,
		 mnc = MNC,
		 lac = LAC,
		 ci = CI,
		 sac = SAC,
		 rac = RAC}) ->
    [MCC1, MCC2, MCC3 | _] = [ string_to_tbcd(X) || <<X:8>> <= MCC] ++ [15,15,15],
    [MNC1, MNC2, MNC3 | _] = [ string_to_tbcd(X) || <<X:8>> <= MNC] ++ [15,15,15],
    Info = case Type of
	       0 -> CI;
	       1 -> SAC;
	       2 -> (RAC bsl 8) bor 255;
	       _ -> 16#ffff
	   end,
    <<Type:8, MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4, LAC:16, Info:16>>.

encode_apn(APN) ->
    << <<(size(Part)):8, Part/binary>> || Part <- APN >>.

encode_isdn_address_string({isdn_address, Extension, Nature, Plan, Number}) ->
    <<Extension:1, Nature:3, Plan:4, (encode_tbcd(Number))/binary>>.

encode_protocol_ppp_opt(Frame) ->
    <<Id:16, Data/binary>> = ppp_frame:encode(Frame),
    {Id, Data}.

%% GSM 09.60 version 6.1.0 Release 1997
encode_protocol_config_opts({{rel97, Protocol}, Opts})
  when is_binary(Opts) ->
    <<0:1, Protocol:7, Opts/binary>>;
encode_protocol_config_opts({Protocol, Opts}) ->
    encode_protocol_opts(Protocol, Opts, <<1:1, 0:4, Protocol:3>>).

encode_protocol_opts(_Protocol, [], Opts) ->
    Opts;
encode_protocol_opts(Protocol, [{Id, Data} | T], Opts)
  when Id < 16#8000; Id >= 16#FF00 ->
    encode_protocol_opts(-1, T, <<Opts/binary, Id:16, (size(Data)):8, Data/binary>>);
encode_protocol_opts(Protocol, [Opt | T], Opts)
  when Protocol == 0 ->
    {Id, Data} = encode_protocol_ppp_opt(Opt),
    encode_protocol_opts(Protocol, T, <<Opts/binary, Id:16, (size(Data)):8, Data/binary>>).

encode_v2_indication_flags(Flags) ->
    pad_to(5, << <<(bool2int(lists:member(F, Flags))):1>> || F <- ?V2_INDICATION_FLAGS>>).

encode_v2_user_location_information(
  #v2_user_location_information{cgi = CGI, sai = SAI, rai = RAI,
				tai = TAI, ecgi = ECGI, lai = LAI}) ->

    IE0 = <<0:2,
	    (is_bin(LAI)):1, (is_bin(ECGI)):1,
	    (is_bin(TAI)):1, (is_bin(RAI)):1,
	    (is_bin(SAI)):1, (is_bin(CGI)):1>>,
    IE1 = maybe_bin(CGI,  IE0),
    IE2 = maybe_bin(SAI,  IE1),
    IE3 = maybe_bin(RAI,  IE2),
    IE4 = maybe_bin(TAI,  IE3),
    IE5 = maybe_bin(ECGI, IE4),
    maybe_bin(LAI,  IE5).

encode_v2_fully_qualified_tunnel_endpoint_identifier(
  #v2_fully_qualified_tunnel_endpoint_identifier{
     interface_type = InterfaceType,
     key = Key,
     ipv4 = IPv4,
     ipv6 = IPv6}) ->
    IE0 = <<(is_bin(IPv4)):1, (is_bin(IPv6)):1, InterfaceType:6, Key:32>>,
    IE1 = maybe_bin(IPv4,  IE0),
    maybe_bin(IPv6,  IE1).

encode_v2_mccmnc(#v2_serving_network{mcc = MCC, mnc = MNC}) ->
    [MCC1, MCC2, MCC3 | _] = [ string_to_tbcd(X) || <<X:8>> <= MCC] ++ [15,15,15],
    [MNC1, MNC2, MNC3 | _] = [ string_to_tbcd(X) || <<X:8>> <= MNC] ++ [15,15,15],
    <<MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4>>.

-include("gtp_packet_v1_gen.hrl").
-include("gtp_packet_v2_gen.hrl").
