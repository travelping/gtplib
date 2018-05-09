%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% Copyright 2015, Travelping GmbH <info@travelping.com>

-module(gtp_packet).

-export([encode/1, encode_ies/1,
	 decode/1, decode/2, decode_ies/1, decode_ies/2,
	 msg_description/1, msg_description_v2/1,
	 pretty_print/1]).
-compile(export_all).
-compile([{parse_transform, cut},
	  bin_opt_info]).
-compile({inline,[decode_tbcd/1, decode_fqdn/1,
		  decode_v2_grouped/1]}).
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

%%%===================================================================
%%% Record formating
%%%===================================================================

-define(PRETTY_PRINT(F, R),
	F(R, N) ->
	       case record_info(size, R) - 1 of
		   N -> record_info(fields, R);
		   _ -> no
	       end).

pretty_print(Record) ->
    io_lib_pretty:print(Record, fun pretty_print/2).

pretty_print(gtp, N) ->
    N = record_info(size, gtp) - 1,
    record_info(fields, gtp);
pretty_print(Record, N) ->
    case pretty_print_v1(Record, N) of
	no ->
	    pretty_print_v2(Record, N);
	RDef ->
	    RDef
    end.

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
		IEs:Length/bytes, _Next/binary>>) ->
    #gtp{version = v1, type = message_type_v1(Type), tei = TEI, ie = IEs};

decode_header(<<2:3, 0:1, T:1, _Spare0:3, Type:8, Length:16,
		Data:Length/bytes, _Next/binary>>) ->
    decode_v2_msg(Data, T, Type);
decode_header(<<2:3, 1:1, T:1, _Spare0:3, Type:8, Length:16,
		Data:Length/bytes, Next/binary>>) ->
    {decode_v2_msg(Data, T, Type), decode_header(Next)}.

decode_v2_msg(<<TEI:32/integer, SeqNo:24, _Spare1:8, IEs/binary>>, 1, Type) ->
    #gtp{version = v2, type = message_type_v2(Type), tei = TEI, seq_no = SeqNo, ie = IEs};
decode_v2_msg(<<SeqNo:24, _Spare1:8, IEs/binary>>, 0, Type) ->
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

maybe_bin(<<Bin/binary>>, 0, _, _, IE) ->
    {IE, Bin};
maybe_bin(<<Bin/binary>>, 1, Len, Pos, IE) ->
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
    Hdr = decode_exthdr_type(HdrData, Type),
    decode_exthdr(NextType, Data, [Hdr|Hdrs]).

decode_exthdr_type(<<Class:8, _/binary>>, 2#00100000) ->
    %% Service Class Indicator
    {service_class, Class};
decode_exthdr_type(<<Port:16, _/binary>>, 2#01000000) ->
    %% UDP Port
    {udp_port, Port};
decode_exthdr_type(<<_:6, PDU:18, _/binary>>, 2#10000010) ->
    %% Long PDCP PDU Number
    {long_pdcp_pdu_number, PDU};
decode_exthdr_type(<<PDU:16, _/binary>>, 2#11000000) ->
    %% PDCP PDU Number
    {pdcp_pdu_number, PDU};
decode_exthdr_type(Container, 2#10000001) ->
    %% RAN Container
    {ran_container, Container};
decode_exthdr_type(Data, Type) ->
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

decode_v1_rai(<<MCCHi:8, MNC3:4, MCC3:4, MNCHi:8, LAC:16, RAC:8>>, Instance) ->
    #routeing_area_identity{
       instance = Instance,
       mcc = decode_tbcd(<<MCCHi:8, 15:4, MCC3:4>>),
       mnc = decode_tbcd(<<MNCHi:8, 15:4, MNC3:4>>),
       lac = LAC,
       rac = RAC}.

decode_v1_uli(<<Type:8, MCCHi:8, MNC3:4, MCC3:4, MNCHi:8, LAC:16, Info:16, _/binary>>, Instance) ->
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

decode_fqdn(FQDN) ->
    [ Part || <<Len:8, Part:Len/bytes>> <= FQDN ].

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
    {Protocol, decode_protocol_opts(Opts, Protocol, [])}.

decode_protocol_opts(<<>>, _Protocol, Opts) ->
    lists:reverse(Opts);
decode_protocol_opts(<<Id:16, Length:8, Data:Length/bytes, Next/binary>>, Protocol, Opts)
  when Protocol == 0, Id >= 16#8000, Id < 16#FF00 ->
    Opt = decode_protocol_ppp_opt(Id, Data),
    decode_protocol_opts(Next, Protocol, [Opt | Opts]);
decode_protocol_opts(<<Id:16, Length:8, Data:Length/bytes, Next/binary>>, _Protocol, Opts) ->
    decode_protocol_opts(Next, -1, [{Id, Data} | Opts]).

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
decode_v2_indication_flags(<<0:1, Next/bitstring>>, [_ | Flags], Acc) ->
    decode_v2_indication_flags(Next, Flags, Acc);
decode_v2_indication_flags(<<1:1, Next/bitstring>>, [F | Flags], Acc) ->
    decode_v2_indication_flags(Next, Flags, [F | Acc]).

decode_v2_indication_flags(<<Flags:5/bytes, _/binary>>) ->
    decode_v2_indication_flags(Flags, ?V2_INDICATION_FLAGS, []);
decode_v2_indication_flags(Flags) ->
    decode_v2_indication_flags(Flags, ?V2_INDICATION_FLAGS, []).

decode_v2_user_location_information(<<_:2, FlagLAI:1, FlagECGI:1,
				      FlagTAI:1, FlagRAI:1, FlagSAI:1, FlagCGI:1,
				      Rest0/binary>>, Instance) ->
    IE0 = #v2_user_location_information{instance = Instance},
    {IE1, Rest1} = maybe_bin(Rest0, FlagCGI,  7, #v2_user_location_information.cgi,  IE0),
    {IE2, Rest2} = maybe_bin(Rest1, FlagSAI,  7, #v2_user_location_information.sai,  IE1),
    {IE3, Rest3} = maybe_bin(Rest2, FlagRAI,  7, #v2_user_location_information.rai,  IE2),
    {IE4, Rest4} = maybe_bin(Rest3, FlagTAI,  5, #v2_user_location_information.tai,  IE3),
    {IE5, Rest5} = maybe_bin(Rest4, FlagECGI, 7, #v2_user_location_information.ecgi, IE4),
    {IE6, _} = maybe_bin(Rest5, FlagLAI,  5, #v2_user_location_information.lai,  IE5),
    IE6.

decode_v2_fully_qualified_tunnel_endpoint_identifier(<<FlagV4:1, FlagV6:1, InterfaceType:6,
						       Key:32, Rest0/binary>>, Instance) ->
    IE0 = #v2_fully_qualified_tunnel_endpoint_identifier{
	     instance = Instance,
	     interface_type = InterfaceType,
	     key = Key},
    {IE1, Rest1} = maybe_bin(Rest0, FlagV4,  4, #v2_fully_qualified_tunnel_endpoint_identifier.ipv4,  IE0),
    {IE2, _} = maybe_bin(Rest1, FlagV6, 16, #v2_fully_qualified_tunnel_endpoint_identifier.ipv6,  IE1),
    IE2.

decode_v2_mccmnc(<<MCCHi:8, MNC3:4, MCC3:4, MNCHi:8, _/binary>>, Instance) ->
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
    IE = decode_v2_element(Data, Type, Instance),
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

decode_imsi(Bin) ->
    decode_tbcd(Bin).

encode_imsi(IMSI) ->
    << B:64/bits, _/binary>> = << (encode_tbcd(IMSI))/binary, -1:64 >>,
    B.

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

encode_fqdn(FQDN) ->
    << <<(size(Part)):8, Part/binary>> || Part <- FQDN >>.

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
encode_protocol_opts(_Protocol, [{Id, Data} | T], Opts)
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

%% -include("gtp_packet_v1_gen.hrl").

msg_description(echo_request) -> <<"Echo Request">>;
msg_description(echo_response) -> <<"Echo Response">>;
msg_description(version_not_supported) -> <<"Version Not Supported">>;
msg_description(node_alive_request) -> <<"Node Alive Request">>;
msg_description(node_alive_response) -> <<"Node Alive Response">>;
msg_description(redirection_request) -> <<"Redirection Request">>;
msg_description(redirection_response) -> <<"Redirection Response">>;
msg_description(create_pdp_context_request) -> <<"Create PDP Context Request">>;
msg_description(create_pdp_context_response) -> <<"Create PDP Context Response">>;
msg_description(update_pdp_context_request) -> <<"Update PDP Context Request">>;
msg_description(update_pdp_context_response) -> <<"Update PDP Context Response">>;
msg_description(delete_pdp_context_request) -> <<"Delete PDP Context Request">>;
msg_description(delete_pdp_context_response) -> <<"Delete PDP Context Response">>;
msg_description(initiate_pdp_context_activation_request) -> <<"Initiate PDP Context Activation Request">>;
msg_description(initiate_pdp_context_activation_response) -> <<"Initiate PDP Context Activation Response">>;
msg_description(error_indication) -> <<"Error Indication">>;
msg_description(pdu_notification_request) -> <<"PDU Notification Request">>;
msg_description(pdu_notification_response) -> <<"PDU Notification Response">>;
msg_description(pdu_notification_reject_request) -> <<"PDU Notification Reject Request">>;
msg_description(pdu_notification_reject_response) -> <<"PDU Notification Reject Response">>;
msg_description(supported_extension_headers_notification) -> <<"Supported Extension Headers Notification">>;
msg_description(send_routeing_information_for_gprs_request) -> <<"Send Routeing Information for GPRS Request">>;
msg_description(send_routeing_information_for_gprs_response) -> <<"Send Routeing Information for GPRS Response">>;
msg_description(failure_report_request) -> <<"Failure Report Request">>;
msg_description(failure_report_response) -> <<"Failure Report Response">>;
msg_description(note_ms_gprs_present_request) -> <<"Note MS GPRS Present Request">>;
msg_description(note_ms_gprs_present_response) -> <<"Note MS GPRS Present Response">>;
msg_description(identification_request) -> <<"Identification Request">>;
msg_description(identification_response) -> <<"Identification Response">>;
msg_description(sgsn_context_request) -> <<"SGSN Context Request">>;
msg_description(sgsn_context_response) -> <<"SGSN Context Response">>;
msg_description(sgsn_context_acknowledge) -> <<"SGSN Context Acknowledge">>;
msg_description(forward_relocation_request) -> <<"Forward Relocation Request">>;
msg_description(forward_relocation_response) -> <<"Forward Relocation Response">>;
msg_description(forward_relocation_complete) -> <<"Forward Relocation Complete">>;
msg_description(relocation_cancel_request) -> <<"Relocation Cancel Request">>;
msg_description(relocation_cancel_response) -> <<"Relocation Cancel Response">>;
msg_description(forward_srns_context) -> <<"Forward SRNS Context">>;
msg_description(forward_relocation_complete_acknowledge) -> <<"Forward Relocation Complete Acknowledge">>;
msg_description(forward_srns_context_acknowledge) -> <<"Forward SRNS Context Acknowledge">>;
msg_description(ran_information_relay) -> <<"RAN Information Relay">>;
msg_description(mbms_notification_request) -> <<"MBMS Notification Request">>;
msg_description(mbms_notification_response) -> <<"MBMS Notification Response">>;
msg_description(mbms_notification_reject_request) -> <<"MBMS Notification Reject Request">>;
msg_description(mbms_notification_reject_response) -> <<"MBMS Notification Reject Response">>;
msg_description(create_mbms_context_request) -> <<"Create MBMS Context Request">>;
msg_description(create_mbms_context_response) -> <<"Create MBMS Context Response">>;
msg_description(update_mbms_context_request) -> <<"Update MBMS Context Request">>;
msg_description(update_mbms_context_response) -> <<"Update MBMS Context Response">>;
msg_description(delete_mbms_context_request) -> <<"Delete MBMS Context Request">>;
msg_description(delete_mbms_context_response) -> <<"Delete MBMS Context Response">>;
msg_description(mbms_registration_request) -> <<"MBMS Registration Request">>;
msg_description(mbms_registration_response) -> <<"MBMS Registration Response">>;
msg_description(mbms_de_registration_request) -> <<"MBMS De-Registration Request">>;
msg_description(mbms_de_registration_response) -> <<"MBMS De-Registration Response">>;
msg_description(mbms_session_start_request) -> <<"MBMS Session Start Request">>;
msg_description(mbms_session_start_response) -> <<"MBMS Session Start Response">>;
msg_description(mbms_session_stop_request) -> <<"MBMS Session Stop Request">>;
msg_description(mbms_session_stop_response) -> <<"MBMS Session Stop Response">>;
msg_description(mbms_session_update_request) -> <<"MBMS Session Update Request">>;
msg_description(mbms_session_update_response) -> <<"MBMS Session Update Response">>;
msg_description(ms_info_change_notification_request) -> <<"MS Info Change Notification Request">>;
msg_description(ms_info_change_notification_response) -> <<"MS Info Change Notification Response">>;
msg_description(data_record_transfer_request) -> <<"Data Record Transfer Request">>;
msg_description(data_record_transfer_response) -> <<"Data Record Transfer Response">>;
msg_description(end_marker) -> <<"End Marker">>;
msg_description(g_pdu) -> <<"G-PDU">>;
msg_description(X) -> io_lib:format("~p", [X]).

message_type_v1(echo_request) -> 1;
message_type_v1(echo_response) -> 2;
message_type_v1(version_not_supported) -> 3;
message_type_v1(node_alive_request) -> 4;
message_type_v1(node_alive_response) -> 5;
message_type_v1(redirection_request) -> 6;
message_type_v1(redirection_response) -> 7;
message_type_v1(create_pdp_context_request) -> 16;
message_type_v1(create_pdp_context_response) -> 17;
message_type_v1(update_pdp_context_request) -> 18;
message_type_v1(update_pdp_context_response) -> 19;
message_type_v1(delete_pdp_context_request) -> 20;
message_type_v1(delete_pdp_context_response) -> 21;
message_type_v1(initiate_pdp_context_activation_request) -> 22;
message_type_v1(initiate_pdp_context_activation_response) -> 23;
message_type_v1(error_indication) -> 26;
message_type_v1(pdu_notification_request) -> 27;
message_type_v1(pdu_notification_response) -> 28;
message_type_v1(pdu_notification_reject_request) -> 29;
message_type_v1(pdu_notification_reject_response) -> 30;
message_type_v1(supported_extension_headers_notification) -> 31;
message_type_v1(send_routeing_information_for_gprs_request) -> 32;
message_type_v1(send_routeing_information_for_gprs_response) -> 33;
message_type_v1(failure_report_request) -> 34;
message_type_v1(failure_report_response) -> 35;
message_type_v1(note_ms_gprs_present_request) -> 36;
message_type_v1(note_ms_gprs_present_response) -> 37;
message_type_v1(identification_request) -> 48;
message_type_v1(identification_response) -> 49;
message_type_v1(sgsn_context_request) -> 50;
message_type_v1(sgsn_context_response) -> 51;
message_type_v1(sgsn_context_acknowledge) -> 52;
message_type_v1(forward_relocation_request) -> 53;
message_type_v1(forward_relocation_response) -> 54;
message_type_v1(forward_relocation_complete) -> 55;
message_type_v1(relocation_cancel_request) -> 56;
message_type_v1(relocation_cancel_response) -> 57;
message_type_v1(forward_srns_context) -> 58;
message_type_v1(forward_relocation_complete_acknowledge) -> 59;
message_type_v1(forward_srns_context_acknowledge) -> 60;
message_type_v1(ran_information_relay) -> 70;
message_type_v1(mbms_notification_request) -> 96;
message_type_v1(mbms_notification_response) -> 97;
message_type_v1(mbms_notification_reject_request) -> 98;
message_type_v1(mbms_notification_reject_response) -> 99;
message_type_v1(create_mbms_context_request) -> 100;
message_type_v1(create_mbms_context_response) -> 101;
message_type_v1(update_mbms_context_request) -> 102;
message_type_v1(update_mbms_context_response) -> 103;
message_type_v1(delete_mbms_context_request) -> 104;
message_type_v1(delete_mbms_context_response) -> 105;
message_type_v1(mbms_registration_request) -> 112;
message_type_v1(mbms_registration_response) -> 113;
message_type_v1(mbms_de_registration_request) -> 114;
message_type_v1(mbms_de_registration_response) -> 115;
message_type_v1(mbms_session_start_request) -> 116;
message_type_v1(mbms_session_start_response) -> 117;
message_type_v1(mbms_session_stop_request) -> 118;
message_type_v1(mbms_session_stop_response) -> 119;
message_type_v1(mbms_session_update_request) -> 120;
message_type_v1(mbms_session_update_response) -> 121;
message_type_v1(ms_info_change_notification_request) -> 128;
message_type_v1(ms_info_change_notification_response) -> 129;
message_type_v1(data_record_transfer_request) -> 240;
message_type_v1(data_record_transfer_response) -> 241;
message_type_v1(end_marker) -> 254;
message_type_v1(g_pdu) -> 255;
message_type_v1(1) -> echo_request;
message_type_v1(2) -> echo_response;
message_type_v1(3) -> version_not_supported;
message_type_v1(4) -> node_alive_request;
message_type_v1(5) -> node_alive_response;
message_type_v1(6) -> redirection_request;
message_type_v1(7) -> redirection_response;
message_type_v1(16) -> create_pdp_context_request;
message_type_v1(17) -> create_pdp_context_response;
message_type_v1(18) -> update_pdp_context_request;
message_type_v1(19) -> update_pdp_context_response;
message_type_v1(20) -> delete_pdp_context_request;
message_type_v1(21) -> delete_pdp_context_response;
message_type_v1(22) -> initiate_pdp_context_activation_request;
message_type_v1(23) -> initiate_pdp_context_activation_response;
message_type_v1(26) -> error_indication;
message_type_v1(27) -> pdu_notification_request;
message_type_v1(28) -> pdu_notification_response;
message_type_v1(29) -> pdu_notification_reject_request;
message_type_v1(30) -> pdu_notification_reject_response;
message_type_v1(31) -> supported_extension_headers_notification;
message_type_v1(32) -> send_routeing_information_for_gprs_request;
message_type_v1(33) -> send_routeing_information_for_gprs_response;
message_type_v1(34) -> failure_report_request;
message_type_v1(35) -> failure_report_response;
message_type_v1(36) -> note_ms_gprs_present_request;
message_type_v1(37) -> note_ms_gprs_present_response;
message_type_v1(48) -> identification_request;
message_type_v1(49) -> identification_response;
message_type_v1(50) -> sgsn_context_request;
message_type_v1(51) -> sgsn_context_response;
message_type_v1(52) -> sgsn_context_acknowledge;
message_type_v1(53) -> forward_relocation_request;
message_type_v1(54) -> forward_relocation_response;
message_type_v1(55) -> forward_relocation_complete;
message_type_v1(56) -> relocation_cancel_request;
message_type_v1(57) -> relocation_cancel_response;
message_type_v1(58) -> forward_srns_context;
message_type_v1(59) -> forward_relocation_complete_acknowledge;
message_type_v1(60) -> forward_srns_context_acknowledge;
message_type_v1(70) -> ran_information_relay;
message_type_v1(96) -> mbms_notification_request;
message_type_v1(97) -> mbms_notification_response;
message_type_v1(98) -> mbms_notification_reject_request;
message_type_v1(99) -> mbms_notification_reject_response;
message_type_v1(100) -> create_mbms_context_request;
message_type_v1(101) -> create_mbms_context_response;
message_type_v1(102) -> update_mbms_context_request;
message_type_v1(103) -> update_mbms_context_response;
message_type_v1(104) -> delete_mbms_context_request;
message_type_v1(105) -> delete_mbms_context_response;
message_type_v1(112) -> mbms_registration_request;
message_type_v1(113) -> mbms_registration_response;
message_type_v1(114) -> mbms_de_registration_request;
message_type_v1(115) -> mbms_de_registration_response;
message_type_v1(116) -> mbms_session_start_request;
message_type_v1(117) -> mbms_session_start_response;
message_type_v1(118) -> mbms_session_stop_request;
message_type_v1(119) -> mbms_session_stop_response;
message_type_v1(120) -> mbms_session_update_request;
message_type_v1(121) -> mbms_session_update_response;
message_type_v1(128) -> ms_info_change_notification_request;
message_type_v1(129) -> ms_info_change_notification_response;
message_type_v1(240) -> data_record_transfer_request;
message_type_v1(241) -> data_record_transfer_response;
message_type_v1(254) -> end_marker;
message_type_v1(255) -> g_pdu;
message_type_v1(Type) -> error(badarg, [Type]).

enum_validated(no) -> 0;
enum_validated(yes) -> 1;
enum_validated(0) -> no;
enum_validated(1) -> yes;
enum_validated(X) when is_integer(X) -> X.

enum_required(no) -> 0;
enum_required(yes) -> 1;
enum_required(0) -> no;
enum_required(1) -> yes;
enum_required(X) when is_integer(X) -> X.

enum_value(request_imsi) -> 0;
enum_value(request_imei) -> 1;
enum_value(request_imsi_and_imei) -> 2;
enum_value(no_identity_needed) -> 3;
enum_value(ms_refuses) -> 4;
enum_value(ms_is_not_gprs_responding) -> 5;
enum_value(reactivation_requested) -> 6;
enum_value(pdp_address_inactivity_timer_expires) -> 7;
enum_value(network_failure) -> 8;
enum_value(qos_parameter_mismatch) -> 9;
enum_value(request_accepted) -> 128;
enum_value(new_pdp_type_due_to_network_preference) -> 129;
enum_value(new_pdp_type_due_to_single_address_bearer_only) -> 130;
enum_value(non_existent) -> 192;
enum_value(invalid_message_format) -> 193;
enum_value(imsi_imei_not_known) -> 194;
enum_value(ms_is_gprs_detached) -> 195;
enum_value(ms_is_not_gprs_responding) -> 196;
enum_value(ms_refuses) -> 197;
enum_value(version_not_supported) -> 198;
enum_value(no_resources_available) -> 199;
enum_value(service_not_supported) -> 200;
enum_value(mandatory_ie_incorrect) -> 201;
enum_value(mandatory_ie_missing) -> 202;
enum_value(optional_ie_incorrect) -> 203;
enum_value(system_failure) -> 204;
enum_value(roaming_restriction) -> 205;
enum_value(p_tmsi_signature_mismatch) -> 206;
enum_value(gprs_connection_suspended) -> 207;
enum_value(authentication_failure) -> 208;
enum_value(user_authentication_failed) -> 209;
enum_value(context_not_found) -> 210;
enum_value(all_dynamic_pdp_addresses_are_occupied) -> 211;
enum_value(no_memory_is_available) -> 212;
enum_value(relocation_failure) -> 213;
enum_value(unknown_mandatory_extension_header) -> 214;
enum_value(semantic_error_in_the_tft_operation) -> 215;
enum_value(syntactic_error_in_the_tft_operation) -> 216;
enum_value(semantic_errors_in_packet_filter) -> 217;
enum_value(syntactic_errors_in_packet_filter) -> 218;
enum_value(missing_or_unknown_apn) -> 219;
enum_value(unknown_pdp_address_or_pdp_type) -> 220;
enum_value(pdp_context_without_tft_already_activated) -> 221;
enum_value(apn_access_denied___no_subscription) -> 222;
enum_value(apn_restriction_type_incompatibility_with_currently_active_pdp_contexts) -> 223;
enum_value(ms_mbms_capabilities_insufficient) -> 224;
enum_value(invalid_correlation_id) -> 225;
enum_value(mbms_bearer_context_superseded) -> 226;
enum_value(bearer_control_mode_violation) -> 227;
enum_value(collision_with_network_initiated_request) -> 228;
enum_value(apn_congestion) -> 229;
enum_value(bearer_handling_not_supported) -> 230;
enum_value(target_access_restricted_for_the_subscriber) -> 231;
enum_value(0) -> request_imsi;
enum_value(1) -> request_imei;
enum_value(2) -> request_imsi_and_imei;
enum_value(3) -> no_identity_needed;
enum_value(4) -> ms_refuses;
enum_value(5) -> ms_is_not_gprs_responding;
enum_value(6) -> reactivation_requested;
enum_value(7) -> pdp_address_inactivity_timer_expires;
enum_value(8) -> network_failure;
enum_value(9) -> qos_parameter_mismatch;
enum_value(128) -> request_accepted;
enum_value(129) -> new_pdp_type_due_to_network_preference;
enum_value(130) -> new_pdp_type_due_to_single_address_bearer_only;
enum_value(192) -> non_existent;
enum_value(193) -> invalid_message_format;
enum_value(194) -> imsi_imei_not_known;
enum_value(195) -> ms_is_gprs_detached;
enum_value(196) -> ms_is_not_gprs_responding;
enum_value(197) -> ms_refuses;
enum_value(198) -> version_not_supported;
enum_value(199) -> no_resources_available;
enum_value(200) -> service_not_supported;
enum_value(201) -> mandatory_ie_incorrect;
enum_value(202) -> mandatory_ie_missing;
enum_value(203) -> optional_ie_incorrect;
enum_value(204) -> system_failure;
enum_value(205) -> roaming_restriction;
enum_value(206) -> p_tmsi_signature_mismatch;
enum_value(207) -> gprs_connection_suspended;
enum_value(208) -> authentication_failure;
enum_value(209) -> user_authentication_failed;
enum_value(210) -> context_not_found;
enum_value(211) -> all_dynamic_pdp_addresses_are_occupied;
enum_value(212) -> no_memory_is_available;
enum_value(213) -> relocation_failure;
enum_value(214) -> unknown_mandatory_extension_header;
enum_value(215) -> semantic_error_in_the_tft_operation;
enum_value(216) -> syntactic_error_in_the_tft_operation;
enum_value(217) -> semantic_errors_in_packet_filter;
enum_value(218) -> syntactic_errors_in_packet_filter;
enum_value(219) -> missing_or_unknown_apn;
enum_value(220) -> unknown_pdp_address_or_pdp_type;
enum_value(221) -> pdp_context_without_tft_already_activated;
enum_value(222) -> apn_access_denied___no_subscription;
enum_value(223) -> apn_restriction_type_incompatibility_with_currently_active_pdp_contexts;
enum_value(224) -> ms_mbms_capabilities_insufficient;
enum_value(225) -> invalid_correlation_id;
enum_value(226) -> mbms_bearer_context_superseded;
enum_value(227) -> bearer_control_mode_violation;
enum_value(228) -> collision_with_network_initiated_request;
enum_value(229) -> apn_congestion;
enum_value(230) -> bearer_handling_not_supported;
enum_value(231) -> target_access_restricted_for_the_subscriber;
enum_value(X) when is_integer(X) -> X.

decode_v1_element(<<M_value:8/integer>>, 1, Instance) ->
    #cause{instance = Instance,
	   value = enum_value(M_value)};

decode_v1_element(<<M_imsi:64/bits>>, 2, Instance) ->
    #international_mobile_subscriber_identity{instance = Instance,
					      imsi = decode_imsi(M_imsi)};

decode_v1_element(<<Data/binary>>, 3, Instance) ->
    decode_v1_rai(Data, Instance);

decode_v1_element(<<M_tlli:4/bytes>>, 4, Instance) ->
    #temporary_logical_link_identity{instance = Instance,
				     tlli = M_tlli};

decode_v1_element(<<M_p_tmsi:4/bytes>>, 5, Instance) ->
    #packet_tmsi{instance = Instance,
		 p_tmsi = M_p_tmsi};

decode_v1_element(<<_:7,
		    M_required:1/integer>>, 8, Instance) ->
    #reordering_required{instance = Instance,
			 required = enum_required(M_required)};

decode_v1_element(<<M_rand:16/bytes,
		    M_sres:4/bytes,
		    M_kc:8/bytes>>, 9, Instance) ->
    #authentication_triplet{instance = Instance,
			    rand = M_rand,
			    sres = M_sres,
			    kc = M_kc};

decode_v1_element(<<>>, 11, Instance) ->
    #map_cause{instance = Instance};

decode_v1_element(<<>>, 12, Instance) ->
    #p_tmsi_signature{instance = Instance};

decode_v1_element(<<_:7,
		    M_validated:1/integer>>, 13, Instance) ->
    #ms_validated{instance = Instance,
		  validated = enum_validated(M_validated)};

decode_v1_element(<<M_restart_counter:8/integer>>, 14, Instance) ->
    #recovery{instance = Instance,
	      restart_counter = M_restart_counter};

decode_v1_element(<<_:6,
		    M_mode:2/integer>>, 15, Instance) ->
    #selection_mode{instance = Instance,
		    mode = M_mode};

decode_v1_element(<<M_tei:32/integer>>, 16, Instance) ->
    #tunnel_endpoint_identifier_data_i{instance = Instance,
				       tei = M_tei};

decode_v1_element(<<M_tei:32/integer>>, 17, Instance) ->
    #tunnel_endpoint_identifier_control_plane{instance = Instance,
					      tei = M_tei};

decode_v1_element(<<_:4,
		    M_nsapi:4/integer,
		    M_tei:32/integer>>, 18, Instance) ->
    #tunnel_endpoint_identifier_data_ii{instance = Instance,
					nsapi = M_nsapi,
					tei = M_tei};

decode_v1_element(<<_:7,
		    M_value:1/integer>>, 19, Instance) ->
    #teardown_ind{instance = Instance,
		  value = M_value};

decode_v1_element(<<_:4,
		    M_nsapi:4/integer>>, 20, Instance) ->
    #nsapi{instance = Instance,
	   nsapi = M_nsapi};

decode_v1_element(<<>>, 21, Instance) ->
    #ranap_cause{instance = Instance};

decode_v1_element(<<>>, 22, Instance) ->
    #rab_context{instance = Instance};

decode_v1_element(<<>>, 23, Instance) ->
    #radio_priority_sms{instance = Instance};

decode_v1_element(<<>>, 24, Instance) ->
    #radio_priority{instance = Instance};

decode_v1_element(<<>>, 25, Instance) ->
    #packet_flow_id{instance = Instance};

decode_v1_element(<<M_value:2/bytes>>, 26, Instance) ->
    #charging_characteristics{instance = Instance,
			      value = M_value};

decode_v1_element(<<>>, 27, Instance) ->
    #trace_reference{instance = Instance};

decode_v1_element(<<>>, 28, Instance) ->
    #trace_type{instance = Instance};

decode_v1_element(<<>>, 29, Instance) ->
    #ms_not_reachable_reason{instance = Instance};

decode_v1_element(<<M_id:4/bytes>>, 127, Instance) ->
    #charging_id{instance = Instance,
		 id = M_id};

decode_v1_element(<<_:4,
		    M_pdp_type_organization:4/integer,
		    M_pdp_type_number:8/integer,
		    M_pdp_address/binary>>, 128, Instance) ->
    #end_user_address{instance = Instance,
		      pdp_type_organization = M_pdp_type_organization,
		      pdp_type_number = M_pdp_type_number,
		      pdp_address = M_pdp_address};

decode_v1_element(<<_:4,
		    M_cksn:4/integer,
		    1:2,
		    M_no_of_vectors:3/integer,
		    M_used_cipher:3/integer,
		    M_kc:8/bytes,
		    M_tripple_Rest/binary>>, 129, Instance) ->
    M_tripple_size = M_no_of_vectors * 8,
    <<M_tripple:M_tripple_size/bytes,
      M_drx_parameter:2/bytes,
      M_ms_network_capability_length:8/integer,
      M_ms_network_capability_Rest/binary>> = M_tripple_Rest,
    M_ms_network_capability_size = M_ms_network_capability_length * 1,
    <<M_ms_network_capability:M_ms_network_capability_size/bytes,
      M_container_length:16/integer,
      M_container_Rest/binary>> = M_ms_network_capability_Rest,
    M_container_size = M_container_length * 1,
    <<M_container:M_container_size/bytes,
      _/binary>> = M_container_Rest,
    #mm_context_gsm{instance = Instance,
		    cksn = M_cksn,
		    no_of_vectors = M_no_of_vectors,
		    used_cipher = M_used_cipher,
		    kc = M_kc,
		    tripple = [X || <<X:8/bytes>> <= M_tripple],
		    drx_parameter = M_drx_parameter,
		    ms_network_capability_length = M_ms_network_capability_length,
		    ms_network_capability = [X || <<X:1/bytes>> <= M_ms_network_capability],
		    container_length = M_container_length,
		    container = [X || <<X:1/bytes>> <= M_container]};

decode_v1_element(<<_:4,
		    M_ksi:4/integer,
		    2:2,
		    M_no_of_vectors:3/integer,
		    _:3,
		    M_ck:16/bytes,
		    M_ik:16/bytes,
		    M_quintuplet_length:16/integer,
		    M_quintuplet_Rest/binary>>, 129, Instance) ->
    M_quintuplet_size = M_quintuplet_length * 1,
    <<M_quintuplet:M_quintuplet_size/bytes,
      M_drx_parameter:2/bytes,
      M_ms_network_capability_length:8/integer,
      M_ms_network_capability_Rest/binary>> = M_quintuplet_Rest,
    M_ms_network_capability_size = M_ms_network_capability_length * 1,
    <<M_ms_network_capability:M_ms_network_capability_size/bytes,
      M_container_length:16/integer,
      M_container_Rest/binary>> = M_ms_network_capability_Rest,
    M_container_size = M_container_length * 1,
    <<M_container:M_container_size/bytes,
      _/binary>> = M_container_Rest,
    #mm_context_umts{instance = Instance,
		     ksi = M_ksi,
		     no_of_vectors = M_no_of_vectors,
		     ck = M_ck,
		     ik = M_ik,
		     quintuplet_length = M_quintuplet_length,
		     quintuplet = [X || <<X:1/bytes>> <= M_quintuplet],
		     drx_parameter = M_drx_parameter,
		     ms_network_capability_length = M_ms_network_capability_length,
		     ms_network_capability = [X || <<X:1/bytes>> <= M_ms_network_capability],
		     container_length = M_container_length,
		     container = [X || <<X:1/bytes>> <= M_container]};

decode_v1_element(<<_:4,
		    M_cksn:4/integer,
		    3:2,
		    M_no_of_vectors:3/integer,
		    M_used_cipher:3/integer,
		    M_kc:8/bytes,
		    M_quintuplet_length:16/integer,
		    M_quintuplet_Rest/binary>>, 129, Instance) ->
    M_quintuplet_size = M_quintuplet_length * 1,
    <<M_quintuplet:M_quintuplet_size/bytes,
      M_drx_parameter:2/bytes,
      M_ms_network_capability_length:8/integer,
      M_ms_network_capability_Rest/binary>> = M_quintuplet_Rest,
    M_ms_network_capability_size = M_ms_network_capability_length * 1,
    <<M_ms_network_capability:M_ms_network_capability_size/bytes,
      M_container_length:16/integer,
      M_container_Rest/binary>> = M_ms_network_capability_Rest,
    M_container_size = M_container_length * 1,
    <<M_container:M_container_size/bytes,
      _/binary>> = M_container_Rest,
    #mm_context_gsm_and_umts{instance = Instance,
			     cksn = M_cksn,
			     no_of_vectors = M_no_of_vectors,
			     used_cipher = M_used_cipher,
			     kc = M_kc,
			     quintuplet_length = M_quintuplet_length,
			     quintuplet = [X || <<X:1/bytes>> <= M_quintuplet],
			     drx_parameter = M_drx_parameter,
			     ms_network_capability_length = M_ms_network_capability_length,
			     ms_network_capability = [X || <<X:1/bytes>> <= M_ms_network_capability],
			     container_length = M_container_length,
			     container = [X || <<X:1/bytes>> <= M_container]};

decode_v1_element(<<_:4,
		    M_ksi:4/integer,
		    0:2,
		    M_no_of_vectors:3/integer,
		    M_used_cipher:3/integer,
		    M_ck:16/bytes,
		    M_ik:16/bytes,
		    M_quintuplet_length:16/integer,
		    M_quintuplet_Rest/binary>>, 129, Instance) ->
    M_quintuplet_size = M_quintuplet_length * 1,
    <<M_quintuplet:M_quintuplet_size/bytes,
      M_drx_parameter:2/bytes,
      M_ms_network_capability_length:8/integer,
      M_ms_network_capability_Rest/binary>> = M_quintuplet_Rest,
    M_ms_network_capability_size = M_ms_network_capability_length * 1,
    <<M_ms_network_capability:M_ms_network_capability_size/bytes,
      M_container_length:16/integer,
      M_container_Rest/binary>> = M_ms_network_capability_Rest,
    M_container_size = M_container_length * 1,
    <<M_container:M_container_size/bytes,
      _/binary>> = M_container_Rest,
    #mm_context_umts_and_used_cipher{instance = Instance,
				     ksi = M_ksi,
				     no_of_vectors = M_no_of_vectors,
				     used_cipher = M_used_cipher,
				     ck = M_ck,
				     ik = M_ik,
				     quintuplet_length = M_quintuplet_length,
				     quintuplet = [X || <<X:1/bytes>> <= M_quintuplet],
				     drx_parameter = M_drx_parameter,
				     ms_network_capability_length = M_ms_network_capability_length,
				     ms_network_capability = [X || <<X:1/bytes>> <= M_ms_network_capability],
				     container_length = M_container_length,
				     container = [X || <<X:1/bytes>> <= M_container]};

decode_v1_element(<<>>, 130, Instance) ->
    #pdp_context{instance = Instance};

decode_v1_element(<<M_apn/binary>>, 131, Instance) ->
    #access_point_name{instance = Instance,
		       apn = decode_fqdn(M_apn)};

decode_v1_element(<<M_config/binary>>, 132, Instance) ->
    #protocol_configuration_options{instance = Instance,
				    config = decode_protocol_config_opts(M_config)};

decode_v1_element(<<M_address/binary>>, 133, Instance) ->
    #gsn_address{instance = Instance,
		 address = M_address};

decode_v1_element(<<M_msisdn/binary>>, 134, Instance) ->
    #ms_international_pstn_isdn_number{instance = Instance,
				       msisdn = decode_isdn_address_string(M_msisdn)};

decode_v1_element(<<M_priority:8/integer,
		    M_data/binary>>, 135, Instance) ->
    #quality_of_service_profile{instance = Instance,
				priority = M_priority,
				data = M_data};

decode_v1_element(<<>>, 136, Instance) ->
    #authentication_quintuplet{instance = Instance};

decode_v1_element(<<>>, 137, Instance) ->
    #traffic_flow_template{instance = Instance};

decode_v1_element(<<>>, 138, Instance) ->
    #target_identification{instance = Instance};

decode_v1_element(<<>>, 139, Instance) ->
    #utran_transparent_container{instance = Instance};

decode_v1_element(<<>>, 140, Instance) ->
    #rab_setup_information{instance = Instance};

decode_v1_element(<<>>, 141, Instance) ->
    #extension_header_type_list{instance = Instance};

decode_v1_element(<<>>, 142, Instance) ->
    #trigger_id{instance = Instance};

decode_v1_element(<<>>, 143, Instance) ->
    #omc_identity{instance = Instance};

decode_v1_element(<<>>, 144, Instance) ->
    #ran_transparent_container{instance = Instance};

decode_v1_element(<<>>, 145, Instance) ->
    #pdp_context_prioritization{instance = Instance};

decode_v1_element(<<>>, 146, Instance) ->
    #additional_rab_setup_information{instance = Instance};

decode_v1_element(<<>>, 147, Instance) ->
    #sgsn_number{instance = Instance};

decode_v1_element(<<M_flags_dual_address_bearer_flag:1,
		    M_flags_upgrade_qos_supported:1,
		    M_flags_nrsn:1,
		    M_flags_no_qos_negotiation:1,
		    M_flags_mbms_counting_information:1,
		    M_flags_ran_procedures_ready:1,
		    M_flags_mbms_service_type:1,
		    M_flags_prohibit_payload_compression:1,
		    _/binary>>, 148, Instance) ->
    #common_flags{instance = Instance,
		  flags = [ 'Dual Address Bearer Flag' || M_flags_dual_address_bearer_flag =/= 0 ] ++ [ 'Upgrade QoS Supported' || M_flags_upgrade_qos_supported =/= 0 ] ++ [ 'NRSN' || M_flags_nrsn =/= 0 ] ++ [ 'No QoS negotiation' || M_flags_no_qos_negotiation =/= 0 ] ++ [ 'MBMS Counting Information' || M_flags_mbms_counting_information =/= 0 ] ++ [ 'RAN Procedures Ready' || M_flags_ran_procedures_ready =/= 0 ] ++ [ 'MBMS Service Type' || M_flags_mbms_service_type =/= 0 ] ++ [ 'Prohibit Payload Compression' || M_flags_prohibit_payload_compression =/= 0 ]};

decode_v1_element(<<>>, 149, Instance) ->
    #apn_restriction{instance = Instance};

decode_v1_element(<<>>, 150, Instance) ->
    #radio_priority_lcs{instance = Instance};

decode_v1_element(<<M_rat_type:8/integer,
		    _/binary>>, 151, Instance) ->
    #rat_type{instance = Instance,
	      rat_type = M_rat_type};

decode_v1_element(<<Data/binary>>, 152, Instance) ->
    decode_v1_uli(Data, Instance);

decode_v1_element(<<M_timezone:8/integer,
		    _:6,
		    M_dst:2/integer,
		    _/binary>>, 153, Instance) ->
    #ms_time_zone{instance = Instance,
		  timezone = M_timezone,
		  dst = M_dst};

decode_v1_element(<<M_imei:64/bits,
		    _/binary>>, 154, Instance) ->
    #imei{instance = Instance,
	  imei = decode_tbcd(M_imei)};

decode_v1_element(<<>>, 155, Instance) ->
    #camel_charging_information_container{instance = Instance};

decode_v1_element(<<>>, 156, Instance) ->
    #mbms_ue_context{instance = Instance};

decode_v1_element(<<>>, 157, Instance) ->
    #temporary_mobile_group_identity{instance = Instance};

decode_v1_element(<<>>, 158, Instance) ->
    #rim_routing_address{instance = Instance};

decode_v1_element(<<>>, 159, Instance) ->
    #mbms_protocol_configuration_options{instance = Instance};

decode_v1_element(<<>>, 160, Instance) ->
    #mbms_service_area{instance = Instance};

decode_v1_element(<<>>, 161, Instance) ->
    #source_rnc_pdcp_context_info{instance = Instance};

decode_v1_element(<<>>, 162, Instance) ->
    #additional_trace_info{instance = Instance};

decode_v1_element(<<>>, 163, Instance) ->
    #hop_counter{instance = Instance};

decode_v1_element(<<>>, 164, Instance) ->
    #selected_plmn_id{instance = Instance};

decode_v1_element(<<>>, 165, Instance) ->
    #mbms_session_identifier{instance = Instance};

decode_v1_element(<<>>, 166, Instance) ->
    #mbms_2g_3g_indicator{instance = Instance};

decode_v1_element(<<>>, 167, Instance) ->
    #enhanced_nsapi{instance = Instance};

decode_v1_element(<<>>, 168, Instance) ->
    #mbms_session_duration{instance = Instance};

decode_v1_element(<<>>, 169, Instance) ->
    #additional_mbms_trace_info{instance = Instance};

decode_v1_element(<<>>, 170, Instance) ->
    #mbms_session_repetition_number{instance = Instance};

decode_v1_element(<<>>, 171, Instance) ->
    #mbms_time_to_data_transfer{instance = Instance};

decode_v1_element(<<>>, 173, Instance) ->
    #bss_container{instance = Instance};

decode_v1_element(<<>>, 174, Instance) ->
    #cell_identification{instance = Instance};

decode_v1_element(<<>>, 175, Instance) ->
    #pdu_numbers{instance = Instance};

decode_v1_element(<<>>, 176, Instance) ->
    #bssgp_cause{instance = Instance};

decode_v1_element(<<>>, 177, Instance) ->
    #required_mbms_bearer_capabilities{instance = Instance};

decode_v1_element(<<>>, 178, Instance) ->
    #rim_routing_address_discriminator{instance = Instance};

decode_v1_element(<<>>, 179, Instance) ->
    #list_of_set_up_pfcs{instance = Instance};

decode_v1_element(<<>>, 180, Instance) ->
    #ps_handover_xid_parameters{instance = Instance};

decode_v1_element(<<>>, 181, Instance) ->
    #ms_info_change_reporting_action{instance = Instance};

decode_v1_element(<<>>, 182, Instance) ->
    #direct_tunnel_flags{instance = Instance};

decode_v1_element(<<>>, 183, Instance) ->
    #correlation_id{instance = Instance};

decode_v1_element(<<>>, 184, Instance) ->
    #bearer_control_mode{instance = Instance};

decode_v1_element(<<>>, 185, Instance) ->
    #mbms_flow_identifier{instance = Instance};

decode_v1_element(<<>>, 186, Instance) ->
    #mbms_ip_multicast_distribution{instance = Instance};

decode_v1_element(<<>>, 187, Instance) ->
    #mbms_distribution_acknowledgement{instance = Instance};

decode_v1_element(<<>>, 188, Instance) ->
    #reliable_inter_rat_handover_info{instance = Instance};

decode_v1_element(<<>>, 189, Instance) ->
    #rfsp_index{instance = Instance};

decode_v1_element(<<M_fqdn/binary>>, 190, Instance) ->
    #fully_qualified_domain_name{instance = Instance,
				 fqdn = decode_fqdn(M_fqdn)};

decode_v1_element(<<>>, 191, Instance) ->
    #evolved_allocation_retention_priority_i{instance = Instance};

decode_v1_element(<<>>, 192, Instance) ->
    #evolved_allocation_retention_priority_ii{instance = Instance};

decode_v1_element(<<M_flags_unauthenticated_imsi:1,
		    M_flags_ccrsi:1,
		    M_flags_cpsr:1,
		    M_flags_retloc:1,
		    M_flags_vb:1,
		    M_flags_pcri:1,
		    M_flags_bdwi:1,
		    M_flags_uasi:1,
		    _/binary>>, 193, Instance) ->
    #extended_common_flags{instance = Instance,
			   flags = [ 'Unauthenticated IMSI' || M_flags_unauthenticated_imsi =/= 0 ] ++ [ 'CCRSI' || M_flags_ccrsi =/= 0 ] ++ [ 'CPSR' || M_flags_cpsr =/= 0 ] ++ [ 'RetLoc' || M_flags_retloc =/= 0 ] ++ [ 'VB' || M_flags_vb =/= 0 ] ++ [ 'PCRI' || M_flags_pcri =/= 0 ] ++ [ 'BDWI' || M_flags_bdwi =/= 0 ] ++ [ 'UASI' || M_flags_uasi =/= 0 ]};

decode_v1_element(<<>>, 194, Instance) ->
    #user_csg_information{instance = Instance};

decode_v1_element(<<>>, 195, Instance) ->
    #csg_information_reporting_action{instance = Instance};

decode_v1_element(<<>>, 196, Instance) ->
    #csg_id{instance = Instance};

decode_v1_element(<<>>, 197, Instance) ->
    #csg_membership_indication{instance = Instance};

decode_v1_element(<<>>, 198, Instance) ->
    #aggregate_maximum_bit_rate{instance = Instance};

decode_v1_element(<<>>, 199, Instance) ->
    #ue_network_capability{instance = Instance};

decode_v1_element(<<>>, 200, Instance) ->
    #ue_ambr{instance = Instance};

decode_v1_element(<<>>, 201, Instance) ->
    #apn_ambr_with_nsapi{instance = Instance};

decode_v1_element(<<>>, 202, Instance) ->
    #ggsn_back_off_time{instance = Instance};

decode_v1_element(<<>>, 203, Instance) ->
    #signalling_priority_indication{instance = Instance};

decode_v1_element(<<>>, 204, Instance) ->
    #signalling_priority_indication_with_nsapi{instance = Instance};

decode_v1_element(<<>>, 205, Instance) ->
    #higher_bitrates_than_16_mbps_flag{instance = Instance};

decode_v1_element(<<>>, 207, Instance) ->
    #additional_mm_context_for_srvcc{instance = Instance};

decode_v1_element(<<>>, 208, Instance) ->
    #additional_flags_for_srvcc{instance = Instance};

decode_v1_element(<<>>, 209, Instance) ->
    #stn_sr{instance = Instance};

decode_v1_element(<<>>, 210, Instance) ->
    #c_msisdn{instance = Instance};

decode_v1_element(<<>>, 211, Instance) ->
    #extended_ranap_cause{instance = Instance};

decode_v1_element(<<>>, 212, Instance) ->
    #enodeb_id{instance = Instance};

decode_v1_element(<<>>, 213, Instance) ->
    #selection_mode_with_nsapi{instance = Instance};

decode_v1_element(<<>>, 214, Instance) ->
    #uli_timestamp{instance = Instance};

decode_v1_element(<<>>, 215, Instance) ->
    #local_home_network_id_with_nsapi{instance = Instance};

decode_v1_element(<<>>, 216, Instance) ->
    #cn_operator_selection_entity{instance = Instance};

decode_v1_element(<<>>, 251, Instance) ->
    #charging_gateway_address{instance = Instance};

decode_v1_element(<<>>, 255, Instance) ->
    #private_extension{instance = Instance};

decode_v1_element(Value, Tag, Instance) ->
    {Tag, Instance, Value}.

decode_v1(<<>>, _PrevId, _PrevInst, IEs) ->
    IEs;
decode_v1(<<1, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(1, PrevId, PrevInst),
    IE = decode_v1_element(Data, 1, Instance),
    decode_v1(Next, 1, Instance, put_ie(IE, IEs));
decode_v1(<<2, Data:8/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(2, PrevId, PrevInst),
    IE = decode_v1_element(Data, 2, Instance),
    decode_v1(Next, 2, Instance, put_ie(IE, IEs));
decode_v1(<<3, Data:6/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(3, PrevId, PrevInst),
    IE = decode_v1_element(Data, 3, Instance),
    decode_v1(Next, 3, Instance, put_ie(IE, IEs));
decode_v1(<<4, Data:4/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(4, PrevId, PrevInst),
    IE = decode_v1_element(Data, 4, Instance),
    decode_v1(Next, 4, Instance, put_ie(IE, IEs));
decode_v1(<<5, Data:4/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(5, PrevId, PrevInst),
    IE = decode_v1_element(Data, 5, Instance),
    decode_v1(Next, 5, Instance, put_ie(IE, IEs));
decode_v1(<<8, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(8, PrevId, PrevInst),
    IE = decode_v1_element(Data, 8, Instance),
    decode_v1(Next, 8, Instance, put_ie(IE, IEs));
decode_v1(<<9, Data:28/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(9, PrevId, PrevInst),
    IE = decode_v1_element(Data, 9, Instance),
    decode_v1(Next, 9, Instance, put_ie(IE, IEs));
decode_v1(<<11, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(11, PrevId, PrevInst),
    IE = decode_v1_element(Data, 11, Instance),
    decode_v1(Next, 11, Instance, put_ie(IE, IEs));
decode_v1(<<12, Data:3/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(12, PrevId, PrevInst),
    IE = decode_v1_element(Data, 12, Instance),
    decode_v1(Next, 12, Instance, put_ie(IE, IEs));
decode_v1(<<13, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(13, PrevId, PrevInst),
    IE = decode_v1_element(Data, 13, Instance),
    decode_v1(Next, 13, Instance, put_ie(IE, IEs));
decode_v1(<<14, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(14, PrevId, PrevInst),
    IE = decode_v1_element(Data, 14, Instance),
    decode_v1(Next, 14, Instance, put_ie(IE, IEs));
decode_v1(<<15, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(15, PrevId, PrevInst),
    IE = decode_v1_element(Data, 15, Instance),
    decode_v1(Next, 15, Instance, put_ie(IE, IEs));
decode_v1(<<16, Data:4/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(16, PrevId, PrevInst),
    IE = decode_v1_element(Data, 16, Instance),
    decode_v1(Next, 16, Instance, put_ie(IE, IEs));
decode_v1(<<17, Data:4/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(17, PrevId, PrevInst),
    IE = decode_v1_element(Data, 17, Instance),
    decode_v1(Next, 17, Instance, put_ie(IE, IEs));
decode_v1(<<18, Data:5/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(18, PrevId, PrevInst),
    IE = decode_v1_element(Data, 18, Instance),
    decode_v1(Next, 18, Instance, put_ie(IE, IEs));
decode_v1(<<19, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(19, PrevId, PrevInst),
    IE = decode_v1_element(Data, 19, Instance),
    decode_v1(Next, 19, Instance, put_ie(IE, IEs));
decode_v1(<<20, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(20, PrevId, PrevInst),
    IE = decode_v1_element(Data, 20, Instance),
    decode_v1(Next, 20, Instance, put_ie(IE, IEs));
decode_v1(<<21, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(21, PrevId, PrevInst),
    IE = decode_v1_element(Data, 21, Instance),
    decode_v1(Next, 21, Instance, put_ie(IE, IEs));
decode_v1(<<22, Data:9/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(22, PrevId, PrevInst),
    IE = decode_v1_element(Data, 22, Instance),
    decode_v1(Next, 22, Instance, put_ie(IE, IEs));
decode_v1(<<23, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(23, PrevId, PrevInst),
    IE = decode_v1_element(Data, 23, Instance),
    decode_v1(Next, 23, Instance, put_ie(IE, IEs));
decode_v1(<<24, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(24, PrevId, PrevInst),
    IE = decode_v1_element(Data, 24, Instance),
    decode_v1(Next, 24, Instance, put_ie(IE, IEs));
decode_v1(<<25, Data:2/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(25, PrevId, PrevInst),
    IE = decode_v1_element(Data, 25, Instance),
    decode_v1(Next, 25, Instance, put_ie(IE, IEs));
decode_v1(<<26, Data:2/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(26, PrevId, PrevInst),
    IE = decode_v1_element(Data, 26, Instance),
    decode_v1(Next, 26, Instance, put_ie(IE, IEs));
decode_v1(<<27, Data:2/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(27, PrevId, PrevInst),
    IE = decode_v1_element(Data, 27, Instance),
    decode_v1(Next, 27, Instance, put_ie(IE, IEs));
decode_v1(<<28, Data:2/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(28, PrevId, PrevInst),
    IE = decode_v1_element(Data, 28, Instance),
    decode_v1(Next, 28, Instance, put_ie(IE, IEs));
decode_v1(<<29, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(29, PrevId, PrevInst),
    IE = decode_v1_element(Data, 29, Instance),
    decode_v1(Next, 29, Instance, put_ie(IE, IEs));
decode_v1(<<127, Data:4/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(127, PrevId, PrevInst),
    IE = decode_v1_element(Data, 127, Instance),
    decode_v1(Next, 127, Instance, put_ie(IE, IEs));
decode_v1(<<Id, Length:16/integer, Rest/binary>>, PrevId, PrevInst, IEs) when Id > 127 ->
    <<Data:Length/binary, Next/binary>> = Rest,
    Instance = v1_instance(Id, PrevId, PrevInst),
    IE = decode_v1_element(Data, Id, Instance),
    decode_v1(Next, Id, Instance, put_ie(IE, IEs));
decode_v1(<<Id, Rest/binary>>, PrevId, PrevInst, IEs) ->
    Instance = v1_instance(Id, PrevId, PrevInst),
    IE = {Id, Instance, Rest},
    decode_v1(<<>>, Id, Instance, put_ie(IE, IEs)).


encode_v1_element(#cause{
		     instance = Instance,
		     value = M_value}) ->
    encode_v1_element(1, Instance, <<(enum_value(M_value)):8/integer>>);

encode_v1_element(#international_mobile_subscriber_identity{
		     instance = Instance,
		     imsi = M_imsi}) ->
    encode_v1_element(2, Instance, <<(encode_imsi(M_imsi)):64/bits>>);

encode_v1_element(#routeing_area_identity{instance = Instance} = IE) ->
    encode_v1_element(3, Instance, encode_v1_rai(IE));

encode_v1_element(#temporary_logical_link_identity{
		     instance = Instance,
		     tlli = M_tlli}) ->
    encode_v1_element(4, Instance, <<M_tlli:4/bytes>>);

encode_v1_element(#packet_tmsi{
		     instance = Instance,
		     p_tmsi = M_p_tmsi}) ->
    encode_v1_element(5, Instance, <<M_p_tmsi:4/bytes>>);

encode_v1_element(#reordering_required{
		     instance = Instance,
		     required = M_required}) ->
    encode_v1_element(8, Instance, <<127:7,
				     (enum_required(M_required)):1/integer>>);

encode_v1_element(#authentication_triplet{
		     instance = Instance,
		     rand = M_rand,
		     sres = M_sres,
		     kc = M_kc}) ->
    encode_v1_element(9, Instance, <<M_rand:16/bytes,
				     M_sres:4/bytes,
				     M_kc:8/bytes>>);

encode_v1_element(#map_cause{
		     instance = Instance,
		     content = Content}) ->
    encode_v1_element(11, Instance, <<Content:1/bytes>>);

encode_v1_element(#p_tmsi_signature{
		     instance = Instance,
		     content = Content}) ->
    encode_v1_element(12, Instance, <<Content:3/bytes>>);

encode_v1_element(#ms_validated{
		     instance = Instance,
		     validated = M_validated}) ->
    encode_v1_element(13, Instance, <<127:7,
				      (enum_validated(M_validated)):1/integer>>);

encode_v1_element(#recovery{
		     instance = Instance,
		     restart_counter = M_restart_counter}) ->
    encode_v1_element(14, Instance, <<M_restart_counter:8>>);

encode_v1_element(#selection_mode{
		     instance = Instance,
		     mode = M_mode}) ->
    encode_v1_element(15, Instance, <<63:6,
				      M_mode:2>>);

encode_v1_element(#tunnel_endpoint_identifier_data_i{
		     instance = Instance,
		     tei = M_tei}) ->
    encode_v1_element(16, Instance, <<M_tei:32>>);

encode_v1_element(#tunnel_endpoint_identifier_control_plane{
		     instance = Instance,
		     tei = M_tei}) ->
    encode_v1_element(17, Instance, <<M_tei:32>>);

encode_v1_element(#tunnel_endpoint_identifier_data_ii{
		     instance = Instance,
		     nsapi = M_nsapi,
		     tei = M_tei}) ->
    encode_v1_element(18, Instance, <<0:4,
				      M_nsapi:4,
				      M_tei:32>>);

encode_v1_element(#teardown_ind{
		     instance = Instance,
		     value = M_value}) ->
    encode_v1_element(19, Instance, <<127:7,
				      M_value:1>>);

encode_v1_element(#nsapi{
		     instance = Instance,
		     nsapi = M_nsapi}) ->
    encode_v1_element(20, Instance, <<0:4,
				      M_nsapi:4>>);

encode_v1_element(#ranap_cause{
		     instance = Instance,
		     content = Content}) ->
    encode_v1_element(21, Instance, <<Content:1/bytes>>);

encode_v1_element(#rab_context{
		     instance = Instance,
		     content = Content}) ->
    encode_v1_element(22, Instance, <<Content:9/bytes>>);

encode_v1_element(#radio_priority_sms{
		     instance = Instance,
		     content = Content}) ->
    encode_v1_element(23, Instance, <<Content:1/bytes>>);

encode_v1_element(#radio_priority{
		     instance = Instance,
		     content = Content}) ->
    encode_v1_element(24, Instance, <<Content:1/bytes>>);

encode_v1_element(#packet_flow_id{
		     instance = Instance,
		     content = Content}) ->
    encode_v1_element(25, Instance, <<Content:2/bytes>>);

encode_v1_element(#charging_characteristics{
		     instance = Instance,
		     value = M_value}) ->
    encode_v1_element(26, Instance, <<M_value:2/bytes>>);

encode_v1_element(#trace_reference{
		     instance = Instance,
		     content = Content}) ->
    encode_v1_element(27, Instance, <<Content:2/bytes>>);

encode_v1_element(#trace_type{
		     instance = Instance,
		     content = Content}) ->
    encode_v1_element(28, Instance, <<Content:2/bytes>>);

encode_v1_element(#ms_not_reachable_reason{
		     instance = Instance,
		     content = Content}) ->
    encode_v1_element(29, Instance, <<Content:1/bytes>>);

encode_v1_element(#charging_id{
		     instance = Instance,
		     id = M_id}) ->
    encode_v1_element(127, Instance, <<M_id:4/bytes>>);

encode_v1_element(#end_user_address{
		     instance = Instance,
		     pdp_type_organization = M_pdp_type_organization,
		     pdp_type_number = M_pdp_type_number,
		     pdp_address = M_pdp_address}) ->
    encode_v1_element(128, Instance, <<15:4,
				       M_pdp_type_organization:4,
				       M_pdp_type_number:8,
				       M_pdp_address/binary>>);

encode_v1_element(#mm_context_gsm{
		     instance = Instance,
		     cksn = M_cksn,
		     no_of_vectors = M_no_of_vectors,
		     used_cipher = M_used_cipher,
		     kc = M_kc,
		     tripple = M_tripple,
		     drx_parameter = M_drx_parameter,
		     ms_network_capability_length = M_ms_network_capability_length,
		     ms_network_capability = M_ms_network_capability,
		     container_length = M_container_length,
		     container = M_container}) ->
    encode_v1_element(129, Instance, <<15:4,
				       M_cksn:4,
				       1:2,
				       M_no_of_vectors:3,
				       M_used_cipher:3,
				       M_kc:8/bytes,
				       (length(M_tripple)):8/integer, (<< <<X/binary>> || X <- M_tripple>>)/binary,
				       M_drx_parameter:2/bytes,
				       M_ms_network_capability_length:8,
				       (length(M_ms_network_capability)):1/integer, (<< <<X/binary>> || X <- M_ms_network_capability>>)/binary,
				       M_container_length:16,
				       (length(M_container)):1/integer, (<< <<X/binary>> || X <- M_container>>)/binary>>);

encode_v1_element(#mm_context_umts{
		     instance = Instance,
		     ksi = M_ksi,
		     no_of_vectors = M_no_of_vectors,
		     ck = M_ck,
		     ik = M_ik,
		     quintuplet_length = M_quintuplet_length,
		     quintuplet = M_quintuplet,
		     drx_parameter = M_drx_parameter,
		     ms_network_capability_length = M_ms_network_capability_length,
		     ms_network_capability = M_ms_network_capability,
		     container_length = M_container_length,
		     container = M_container}) ->
    encode_v1_element(129, Instance, <<15:4,
				       M_ksi:4,
				       2:2,
				       M_no_of_vectors:3,
				       7:3,
				       M_ck:16/bytes,
				       M_ik:16/bytes,
				       M_quintuplet_length:16,
				       (length(M_quintuplet)):1/integer, (<< <<X/binary>> || X <- M_quintuplet>>)/binary,
				       M_drx_parameter:2/bytes,
				       M_ms_network_capability_length:8,
				       (length(M_ms_network_capability)):1/integer, (<< <<X/binary>> || X <- M_ms_network_capability>>)/binary,
				       M_container_length:16,
				       (length(M_container)):1/integer, (<< <<X/binary>> || X <- M_container>>)/binary>>);

encode_v1_element(#mm_context_gsm_and_umts{
		     instance = Instance,
		     cksn = M_cksn,
		     no_of_vectors = M_no_of_vectors,
		     used_cipher = M_used_cipher,
		     kc = M_kc,
		     quintuplet_length = M_quintuplet_length,
		     quintuplet = M_quintuplet,
		     drx_parameter = M_drx_parameter,
		     ms_network_capability_length = M_ms_network_capability_length,
		     ms_network_capability = M_ms_network_capability,
		     container_length = M_container_length,
		     container = M_container}) ->
    encode_v1_element(129, Instance, <<15:4,
				       M_cksn:4,
				       3:2,
				       M_no_of_vectors:3,
				       M_used_cipher:3,
				       M_kc:8/bytes,
				       M_quintuplet_length:16,
				       (length(M_quintuplet)):1/integer, (<< <<X/binary>> || X <- M_quintuplet>>)/binary,
				       M_drx_parameter:2/bytes,
				       M_ms_network_capability_length:8,
				       (length(M_ms_network_capability)):1/integer, (<< <<X/binary>> || X <- M_ms_network_capability>>)/binary,
				       M_container_length:16,
				       (length(M_container)):1/integer, (<< <<X/binary>> || X <- M_container>>)/binary>>);

encode_v1_element(#mm_context_umts_and_used_cipher{
		     instance = Instance,
		     ksi = M_ksi,
		     no_of_vectors = M_no_of_vectors,
		     used_cipher = M_used_cipher,
		     ck = M_ck,
		     ik = M_ik,
		     quintuplet_length = M_quintuplet_length,
		     quintuplet = M_quintuplet,
		     drx_parameter = M_drx_parameter,
		     ms_network_capability_length = M_ms_network_capability_length,
		     ms_network_capability = M_ms_network_capability,
		     container_length = M_container_length,
		     container = M_container}) ->
    encode_v1_element(129, Instance, <<15:4,
				       M_ksi:4,
				       0:2,
				       M_no_of_vectors:3,
				       M_used_cipher:3,
				       M_ck:16/bytes,
				       M_ik:16/bytes,
				       M_quintuplet_length:16,
				       (length(M_quintuplet)):1/integer, (<< <<X/binary>> || X <- M_quintuplet>>)/binary,
				       M_drx_parameter:2/bytes,
				       M_ms_network_capability_length:8,
				       (length(M_ms_network_capability)):1/integer, (<< <<X/binary>> || X <- M_ms_network_capability>>)/binary,
				       M_container_length:16,
				       (length(M_container)):1/integer, (<< <<X/binary>> || X <- M_container>>)/binary>>);

encode_v1_element(#pdp_context{
		     instance = Instance}) ->
    encode_v1_element(130, Instance, <<>>);

encode_v1_element(#access_point_name{
		     instance = Instance,
		     apn = M_apn}) ->
    encode_v1_element(131, Instance, <<(encode_fqdn(M_apn))/binary>>);

encode_v1_element(#protocol_configuration_options{
		     instance = Instance,
		     config = M_config}) ->
    encode_v1_element(132, Instance, <<(encode_protocol_config_opts(M_config))/binary>>);

encode_v1_element(#gsn_address{
		     instance = Instance,
		     address = M_address}) ->
    encode_v1_element(133, Instance, <<M_address/binary>>);

encode_v1_element(#ms_international_pstn_isdn_number{
		     instance = Instance,
		     msisdn = M_msisdn}) ->
    encode_v1_element(134, Instance, <<(encode_isdn_address_string(M_msisdn))/binary>>);

encode_v1_element(#quality_of_service_profile{
		     instance = Instance,
		     priority = M_priority,
		     data = M_data}) ->
    encode_v1_element(135, Instance, <<M_priority:8,
				       M_data/binary>>);

encode_v1_element(#authentication_quintuplet{
		     instance = Instance}) ->
    encode_v1_element(136, Instance, <<>>);

encode_v1_element(#traffic_flow_template{
		     instance = Instance}) ->
    encode_v1_element(137, Instance, <<>>);

encode_v1_element(#target_identification{
		     instance = Instance}) ->
    encode_v1_element(138, Instance, <<>>);

encode_v1_element(#utran_transparent_container{
		     instance = Instance}) ->
    encode_v1_element(139, Instance, <<>>);

encode_v1_element(#rab_setup_information{
		     instance = Instance}) ->
    encode_v1_element(140, Instance, <<>>);

encode_v1_element(#extension_header_type_list{
		     instance = Instance}) ->
    encode_v1_element(141, Instance, <<>>);

encode_v1_element(#trigger_id{
		     instance = Instance}) ->
    encode_v1_element(142, Instance, <<>>);

encode_v1_element(#omc_identity{
		     instance = Instance}) ->
    encode_v1_element(143, Instance, <<>>);

encode_v1_element(#ran_transparent_container{
		     instance = Instance}) ->
    encode_v1_element(144, Instance, <<>>);

encode_v1_element(#pdp_context_prioritization{
		     instance = Instance}) ->
    encode_v1_element(145, Instance, <<>>);

encode_v1_element(#additional_rab_setup_information{
		     instance = Instance}) ->
    encode_v1_element(146, Instance, <<>>);

encode_v1_element(#sgsn_number{
		     instance = Instance}) ->
    encode_v1_element(147, Instance, <<>>);

encode_v1_element(#common_flags{
		     instance = Instance,
		     flags = M_flags}) ->
    encode_v1_element(148, Instance, <<(encode_flag('Dual Address Bearer Flag', M_flags)):1,
				       (encode_flag('Upgrade QoS Supported', M_flags)):1,
				       (encode_flag('NRSN', M_flags)):1,
				       (encode_flag('No QoS negotiation', M_flags)):1,
				       (encode_flag('MBMS Counting Information', M_flags)):1,
				       (encode_flag('RAN Procedures Ready', M_flags)):1,
				       (encode_flag('MBMS Service Type', M_flags)):1,
				       (encode_flag('Prohibit Payload Compression', M_flags)):1>>);

encode_v1_element(#apn_restriction{
		     instance = Instance}) ->
    encode_v1_element(149, Instance, <<>>);

encode_v1_element(#radio_priority_lcs{
		     instance = Instance}) ->
    encode_v1_element(150, Instance, <<>>);

encode_v1_element(#rat_type{
		     instance = Instance,
		     rat_type = M_rat_type}) ->
    encode_v1_element(151, Instance, <<M_rat_type:8>>);

encode_v1_element(#user_location_information{instance = Instance} = IE) ->
    encode_v1_element(152, Instance, encode_v1_uli(IE));

encode_v1_element(#ms_time_zone{
		     instance = Instance,
		     timezone = M_timezone,
		     dst = M_dst}) ->
    encode_v1_element(153, Instance, <<M_timezone:8,
				       0:6,
				       M_dst:2>>);

encode_v1_element(#imei{
		     instance = Instance,
		     imei = M_imei}) ->
    encode_v1_element(154, Instance, <<(encode_tbcd(M_imei)):64/bits>>);

encode_v1_element(#camel_charging_information_container{
		     instance = Instance}) ->
    encode_v1_element(155, Instance, <<>>);

encode_v1_element(#mbms_ue_context{
		     instance = Instance}) ->
    encode_v1_element(156, Instance, <<>>);

encode_v1_element(#temporary_mobile_group_identity{
		     instance = Instance}) ->
    encode_v1_element(157, Instance, <<>>);

encode_v1_element(#rim_routing_address{
		     instance = Instance}) ->
    encode_v1_element(158, Instance, <<>>);

encode_v1_element(#mbms_protocol_configuration_options{
		     instance = Instance}) ->
    encode_v1_element(159, Instance, <<>>);

encode_v1_element(#mbms_service_area{
		     instance = Instance}) ->
    encode_v1_element(160, Instance, <<>>);

encode_v1_element(#source_rnc_pdcp_context_info{
		     instance = Instance}) ->
    encode_v1_element(161, Instance, <<>>);

encode_v1_element(#additional_trace_info{
		     instance = Instance}) ->
    encode_v1_element(162, Instance, <<>>);

encode_v1_element(#hop_counter{
		     instance = Instance}) ->
    encode_v1_element(163, Instance, <<>>);

encode_v1_element(#selected_plmn_id{
		     instance = Instance}) ->
    encode_v1_element(164, Instance, <<>>);

encode_v1_element(#mbms_session_identifier{
		     instance = Instance}) ->
    encode_v1_element(165, Instance, <<>>);

encode_v1_element(#mbms_2g_3g_indicator{
		     instance = Instance}) ->
    encode_v1_element(166, Instance, <<>>);

encode_v1_element(#enhanced_nsapi{
		     instance = Instance}) ->
    encode_v1_element(167, Instance, <<>>);

encode_v1_element(#mbms_session_duration{
		     instance = Instance}) ->
    encode_v1_element(168, Instance, <<>>);

encode_v1_element(#additional_mbms_trace_info{
		     instance = Instance}) ->
    encode_v1_element(169, Instance, <<>>);

encode_v1_element(#mbms_session_repetition_number{
		     instance = Instance}) ->
    encode_v1_element(170, Instance, <<>>);

encode_v1_element(#mbms_time_to_data_transfer{
		     instance = Instance}) ->
    encode_v1_element(171, Instance, <<>>);

encode_v1_element(#bss_container{
		     instance = Instance}) ->
    encode_v1_element(173, Instance, <<>>);

encode_v1_element(#cell_identification{
		     instance = Instance}) ->
    encode_v1_element(174, Instance, <<>>);

encode_v1_element(#pdu_numbers{
		     instance = Instance}) ->
    encode_v1_element(175, Instance, <<>>);

encode_v1_element(#bssgp_cause{
		     instance = Instance}) ->
    encode_v1_element(176, Instance, <<>>);

encode_v1_element(#required_mbms_bearer_capabilities{
		     instance = Instance}) ->
    encode_v1_element(177, Instance, <<>>);

encode_v1_element(#rim_routing_address_discriminator{
		     instance = Instance}) ->
    encode_v1_element(178, Instance, <<>>);

encode_v1_element(#list_of_set_up_pfcs{
		     instance = Instance}) ->
    encode_v1_element(179, Instance, <<>>);

encode_v1_element(#ps_handover_xid_parameters{
		     instance = Instance}) ->
    encode_v1_element(180, Instance, <<>>);

encode_v1_element(#ms_info_change_reporting_action{
		     instance = Instance}) ->
    encode_v1_element(181, Instance, <<>>);

encode_v1_element(#direct_tunnel_flags{
		     instance = Instance}) ->
    encode_v1_element(182, Instance, <<>>);

encode_v1_element(#correlation_id{
		     instance = Instance}) ->
    encode_v1_element(183, Instance, <<>>);

encode_v1_element(#bearer_control_mode{
		     instance = Instance}) ->
    encode_v1_element(184, Instance, <<>>);

encode_v1_element(#mbms_flow_identifier{
		     instance = Instance}) ->
    encode_v1_element(185, Instance, <<>>);

encode_v1_element(#mbms_ip_multicast_distribution{
		     instance = Instance}) ->
    encode_v1_element(186, Instance, <<>>);

encode_v1_element(#mbms_distribution_acknowledgement{
		     instance = Instance}) ->
    encode_v1_element(187, Instance, <<>>);

encode_v1_element(#reliable_inter_rat_handover_info{
		     instance = Instance}) ->
    encode_v1_element(188, Instance, <<>>);

encode_v1_element(#rfsp_index{
		     instance = Instance}) ->
    encode_v1_element(189, Instance, <<>>);

encode_v1_element(#fully_qualified_domain_name{
		     instance = Instance,
		     fqdn = M_fqdn}) ->
    encode_v1_element(190, Instance, <<(encode_fqdn(M_fqdn))/binary>>);

encode_v1_element(#evolved_allocation_retention_priority_i{
		     instance = Instance}) ->
    encode_v1_element(191, Instance, <<>>);

encode_v1_element(#evolved_allocation_retention_priority_ii{
		     instance = Instance}) ->
    encode_v1_element(192, Instance, <<>>);

encode_v1_element(#extended_common_flags{
		     instance = Instance,
		     flags = M_flags}) ->
    encode_v1_element(193, Instance, <<(encode_flag('Unauthenticated IMSI', M_flags)):1,
				       (encode_flag('CCRSI', M_flags)):1,
				       (encode_flag('CPSR', M_flags)):1,
				       (encode_flag('RetLoc', M_flags)):1,
				       (encode_flag('VB', M_flags)):1,
				       (encode_flag('PCRI', M_flags)):1,
				       (encode_flag('BDWI', M_flags)):1,
				       (encode_flag('UASI', M_flags)):1>>);

encode_v1_element(#user_csg_information{
		     instance = Instance}) ->
    encode_v1_element(194, Instance, <<>>);

encode_v1_element(#csg_information_reporting_action{
		     instance = Instance}) ->
    encode_v1_element(195, Instance, <<>>);

encode_v1_element(#csg_id{
		     instance = Instance}) ->
    encode_v1_element(196, Instance, <<>>);

encode_v1_element(#csg_membership_indication{
		     instance = Instance}) ->
    encode_v1_element(197, Instance, <<>>);

encode_v1_element(#aggregate_maximum_bit_rate{
		     instance = Instance}) ->
    encode_v1_element(198, Instance, <<>>);

encode_v1_element(#ue_network_capability{
		     instance = Instance}) ->
    encode_v1_element(199, Instance, <<>>);

encode_v1_element(#ue_ambr{
		     instance = Instance}) ->
    encode_v1_element(200, Instance, <<>>);

encode_v1_element(#apn_ambr_with_nsapi{
		     instance = Instance}) ->
    encode_v1_element(201, Instance, <<>>);

encode_v1_element(#ggsn_back_off_time{
		     instance = Instance}) ->
    encode_v1_element(202, Instance, <<>>);

encode_v1_element(#signalling_priority_indication{
		     instance = Instance}) ->
    encode_v1_element(203, Instance, <<>>);

encode_v1_element(#signalling_priority_indication_with_nsapi{
		     instance = Instance}) ->
    encode_v1_element(204, Instance, <<>>);

encode_v1_element(#higher_bitrates_than_16_mbps_flag{
		     instance = Instance}) ->
    encode_v1_element(205, Instance, <<>>);

encode_v1_element(#additional_mm_context_for_srvcc{
		     instance = Instance}) ->
    encode_v1_element(207, Instance, <<>>);

encode_v1_element(#additional_flags_for_srvcc{
		     instance = Instance}) ->
    encode_v1_element(208, Instance, <<>>);

encode_v1_element(#stn_sr{
		     instance = Instance}) ->
    encode_v1_element(209, Instance, <<>>);

encode_v1_element(#c_msisdn{
		     instance = Instance}) ->
    encode_v1_element(210, Instance, <<>>);

encode_v1_element(#extended_ranap_cause{
		     instance = Instance}) ->
    encode_v1_element(211, Instance, <<>>);

encode_v1_element(#enodeb_id{
		     instance = Instance}) ->
    encode_v1_element(212, Instance, <<>>);

encode_v1_element(#selection_mode_with_nsapi{
		     instance = Instance}) ->
    encode_v1_element(213, Instance, <<>>);

encode_v1_element(#uli_timestamp{
		     instance = Instance}) ->
    encode_v1_element(214, Instance, <<>>);

encode_v1_element(#local_home_network_id_with_nsapi{
		     instance = Instance}) ->
    encode_v1_element(215, Instance, <<>>);

encode_v1_element(#cn_operator_selection_entity{
		     instance = Instance}) ->
    encode_v1_element(216, Instance, <<>>);

encode_v1_element(#charging_gateway_address{
		     instance = Instance}) ->
    encode_v1_element(251, Instance, <<>>);

encode_v1_element(#private_extension{
		     instance = Instance}) ->
    encode_v1_element(255, Instance, <<>>);

encode_v1_element({Tag, Instance, Value}) when is_integer(Tag), is_integer(Instance), is_binary(Value) ->
    encode_v1_element(Tag, Instance, Value).

?PRETTY_PRINT(pretty_print_v1, cause);
?PRETTY_PRINT(pretty_print_v1, international_mobile_subscriber_identity);
?PRETTY_PRINT(pretty_print_v1, routeing_area_identity);
?PRETTY_PRINT(pretty_print_v1, temporary_logical_link_identity);
?PRETTY_PRINT(pretty_print_v1, packet_tmsi);
?PRETTY_PRINT(pretty_print_v1, reordering_required);
?PRETTY_PRINT(pretty_print_v1, authentication_triplet);
?PRETTY_PRINT(pretty_print_v1, map_cause);
?PRETTY_PRINT(pretty_print_v1, p_tmsi_signature);
?PRETTY_PRINT(pretty_print_v1, ms_validated);
?PRETTY_PRINT(pretty_print_v1, recovery);
?PRETTY_PRINT(pretty_print_v1, selection_mode);
?PRETTY_PRINT(pretty_print_v1, tunnel_endpoint_identifier_data_i);
?PRETTY_PRINT(pretty_print_v1, tunnel_endpoint_identifier_control_plane);
?PRETTY_PRINT(pretty_print_v1, tunnel_endpoint_identifier_data_ii);
?PRETTY_PRINT(pretty_print_v1, teardown_ind);
?PRETTY_PRINT(pretty_print_v1, nsapi);
?PRETTY_PRINT(pretty_print_v1, ranap_cause);
?PRETTY_PRINT(pretty_print_v1, rab_context);
?PRETTY_PRINT(pretty_print_v1, radio_priority_sms);
?PRETTY_PRINT(pretty_print_v1, radio_priority);
?PRETTY_PRINT(pretty_print_v1, packet_flow_id);
?PRETTY_PRINT(pretty_print_v1, charging_characteristics);
?PRETTY_PRINT(pretty_print_v1, trace_reference);
?PRETTY_PRINT(pretty_print_v1, trace_type);
?PRETTY_PRINT(pretty_print_v1, ms_not_reachable_reason);
?PRETTY_PRINT(pretty_print_v1, charging_id);
?PRETTY_PRINT(pretty_print_v1, end_user_address);
?PRETTY_PRINT(pretty_print_v1, mm_context_gsm);
?PRETTY_PRINT(pretty_print_v1, mm_context_umts);
?PRETTY_PRINT(pretty_print_v1, mm_context_gsm_and_umts);
?PRETTY_PRINT(pretty_print_v1, mm_context_umts_and_used_cipher);
?PRETTY_PRINT(pretty_print_v1, pdp_context);
?PRETTY_PRINT(pretty_print_v1, access_point_name);
?PRETTY_PRINT(pretty_print_v1, protocol_configuration_options);
?PRETTY_PRINT(pretty_print_v1, gsn_address);
?PRETTY_PRINT(pretty_print_v1, ms_international_pstn_isdn_number);
?PRETTY_PRINT(pretty_print_v1, quality_of_service_profile);
?PRETTY_PRINT(pretty_print_v1, authentication_quintuplet);
?PRETTY_PRINT(pretty_print_v1, traffic_flow_template);
?PRETTY_PRINT(pretty_print_v1, target_identification);
?PRETTY_PRINT(pretty_print_v1, utran_transparent_container);
?PRETTY_PRINT(pretty_print_v1, rab_setup_information);
?PRETTY_PRINT(pretty_print_v1, extension_header_type_list);
?PRETTY_PRINT(pretty_print_v1, trigger_id);
?PRETTY_PRINT(pretty_print_v1, omc_identity);
?PRETTY_PRINT(pretty_print_v1, ran_transparent_container);
?PRETTY_PRINT(pretty_print_v1, pdp_context_prioritization);
?PRETTY_PRINT(pretty_print_v1, additional_rab_setup_information);
?PRETTY_PRINT(pretty_print_v1, sgsn_number);
?PRETTY_PRINT(pretty_print_v1, common_flags);
?PRETTY_PRINT(pretty_print_v1, apn_restriction);
?PRETTY_PRINT(pretty_print_v1, radio_priority_lcs);
?PRETTY_PRINT(pretty_print_v1, rat_type);
?PRETTY_PRINT(pretty_print_v1, user_location_information);
?PRETTY_PRINT(pretty_print_v1, ms_time_zone);
?PRETTY_PRINT(pretty_print_v1, imei);
?PRETTY_PRINT(pretty_print_v1, camel_charging_information_container);
?PRETTY_PRINT(pretty_print_v1, mbms_ue_context);
?PRETTY_PRINT(pretty_print_v1, temporary_mobile_group_identity);
?PRETTY_PRINT(pretty_print_v1, rim_routing_address);
?PRETTY_PRINT(pretty_print_v1, mbms_protocol_configuration_options);
?PRETTY_PRINT(pretty_print_v1, mbms_service_area);
?PRETTY_PRINT(pretty_print_v1, source_rnc_pdcp_context_info);
?PRETTY_PRINT(pretty_print_v1, additional_trace_info);
?PRETTY_PRINT(pretty_print_v1, hop_counter);
?PRETTY_PRINT(pretty_print_v1, selected_plmn_id);
?PRETTY_PRINT(pretty_print_v1, mbms_session_identifier);
?PRETTY_PRINT(pretty_print_v1, mbms_2g_3g_indicator);
?PRETTY_PRINT(pretty_print_v1, enhanced_nsapi);
?PRETTY_PRINT(pretty_print_v1, mbms_session_duration);
?PRETTY_PRINT(pretty_print_v1, additional_mbms_trace_info);
?PRETTY_PRINT(pretty_print_v1, mbms_session_repetition_number);
?PRETTY_PRINT(pretty_print_v1, mbms_time_to_data_transfer);
?PRETTY_PRINT(pretty_print_v1, bss_container);
?PRETTY_PRINT(pretty_print_v1, cell_identification);
?PRETTY_PRINT(pretty_print_v1, pdu_numbers);
?PRETTY_PRINT(pretty_print_v1, bssgp_cause);
?PRETTY_PRINT(pretty_print_v1, required_mbms_bearer_capabilities);
?PRETTY_PRINT(pretty_print_v1, rim_routing_address_discriminator);
?PRETTY_PRINT(pretty_print_v1, list_of_set_up_pfcs);
?PRETTY_PRINT(pretty_print_v1, ps_handover_xid_parameters);
?PRETTY_PRINT(pretty_print_v1, ms_info_change_reporting_action);
?PRETTY_PRINT(pretty_print_v1, direct_tunnel_flags);
?PRETTY_PRINT(pretty_print_v1, correlation_id);
?PRETTY_PRINT(pretty_print_v1, bearer_control_mode);
?PRETTY_PRINT(pretty_print_v1, mbms_flow_identifier);
?PRETTY_PRINT(pretty_print_v1, mbms_ip_multicast_distribution);
?PRETTY_PRINT(pretty_print_v1, mbms_distribution_acknowledgement);
?PRETTY_PRINT(pretty_print_v1, reliable_inter_rat_handover_info);
?PRETTY_PRINT(pretty_print_v1, rfsp_index);
?PRETTY_PRINT(pretty_print_v1, fully_qualified_domain_name);
?PRETTY_PRINT(pretty_print_v1, evolved_allocation_retention_priority_i);
?PRETTY_PRINT(pretty_print_v1, evolved_allocation_retention_priority_ii);
?PRETTY_PRINT(pretty_print_v1, extended_common_flags);
?PRETTY_PRINT(pretty_print_v1, user_csg_information);
?PRETTY_PRINT(pretty_print_v1, csg_information_reporting_action);
?PRETTY_PRINT(pretty_print_v1, csg_id);
?PRETTY_PRINT(pretty_print_v1, csg_membership_indication);
?PRETTY_PRINT(pretty_print_v1, aggregate_maximum_bit_rate);
?PRETTY_PRINT(pretty_print_v1, ue_network_capability);
?PRETTY_PRINT(pretty_print_v1, ue_ambr);
?PRETTY_PRINT(pretty_print_v1, apn_ambr_with_nsapi);
?PRETTY_PRINT(pretty_print_v1, ggsn_back_off_time);
?PRETTY_PRINT(pretty_print_v1, signalling_priority_indication);
?PRETTY_PRINT(pretty_print_v1, signalling_priority_indication_with_nsapi);
?PRETTY_PRINT(pretty_print_v1, higher_bitrates_than_16_mbps_flag);
?PRETTY_PRINT(pretty_print_v1, additional_mm_context_for_srvcc);
?PRETTY_PRINT(pretty_print_v1, additional_flags_for_srvcc);
?PRETTY_PRINT(pretty_print_v1, stn_sr);
?PRETTY_PRINT(pretty_print_v1, c_msisdn);
?PRETTY_PRINT(pretty_print_v1, extended_ranap_cause);
?PRETTY_PRINT(pretty_print_v1, enodeb_id);
?PRETTY_PRINT(pretty_print_v1, selection_mode_with_nsapi);
?PRETTY_PRINT(pretty_print_v1, uli_timestamp);
?PRETTY_PRINT(pretty_print_v1, local_home_network_id_with_nsapi);
?PRETTY_PRINT(pretty_print_v1, cn_operator_selection_entity);
?PRETTY_PRINT(pretty_print_v1, charging_gateway_address);
?PRETTY_PRINT(pretty_print_v1, private_extension);
pretty_print_v1(_, _) ->
    no.
%% -include("gtp_packet_v2_gen.hrl").

msg_description_v2(echo_request) -> <<"Echo Request">>;
msg_description_v2(echo_response) -> <<"Echo Response">>;
msg_description_v2(version_not_supported) -> <<"Version Not Supported">>;
msg_description_v2(create_session_request) -> <<"Create Session Request">>;
msg_description_v2(create_session_response) -> <<"Create Session Response">>;
msg_description_v2(delete_session_request) -> <<"Delete Session Request">>;
msg_description_v2(delete_session_response) -> <<"Delete Session Response">>;
msg_description_v2(modify_bearer_request) -> <<"Modify Bearer Request">>;
msg_description_v2(modify_bearer_response) -> <<"Modify Bearer Response">>;
msg_description_v2(change_notification_request) -> <<"Change Notification Request">>;
msg_description_v2(change_notification_response) -> <<"Change Notification Response">>;
msg_description_v2(modify_bearer_command) -> <<"Modify Bearer Command">>;
msg_description_v2(modify_bearer_failure_indication) -> <<"Modify Bearer Failure Indication">>;
msg_description_v2(delete_bearer_command) -> <<"Delete Bearer Command">>;
msg_description_v2(delete_bearer_failure_indication) -> <<"Delete Bearer Failure Indication">>;
msg_description_v2(bearer_resource_command) -> <<"Bearer Resource Command">>;
msg_description_v2(bearer_resource_failure_indication) -> <<"Bearer Resource Failure Indication">>;
msg_description_v2(downlink_data_notification_failure_indication) -> <<"Downlink Data Notification Failure Indication">>;
msg_description_v2(trace_session_activation) -> <<"Trace Session Activation">>;
msg_description_v2(trace_session_deactivation) -> <<"Trace Session Deactivation">>;
msg_description_v2(stop_paging_indication) -> <<"Stop Paging Indication">>;
msg_description_v2(create_bearer_request) -> <<"Create Bearer Request">>;
msg_description_v2(create_bearer_response) -> <<"Create Bearer Response">>;
msg_description_v2(update_bearer_request) -> <<"Update Bearer Request">>;
msg_description_v2(update_bearer_response) -> <<"Update Bearer Response">>;
msg_description_v2(delete_bearer_request) -> <<"Delete Bearer Request">>;
msg_description_v2(delete_bearer_response) -> <<"Delete Bearer Response">>;
msg_description_v2(delete_pdn_connection_set_request) -> <<"Delete PDN Connection Set Request">>;
msg_description_v2(delete_pdn_connection_set_response) -> <<"Delete PDN Connection Set Response">>;
msg_description_v2(pgw_downlink_triggering_notification) -> <<"PGW Downlink Triggering Notification">>;
msg_description_v2(pgw_downlink_triggering_acknowledge) -> <<"PGW Downlink Triggering Acknowledge">>;
msg_description_v2(identification_request) -> <<"Identification Request">>;
msg_description_v2(identification_response) -> <<"Identification Response">>;
msg_description_v2(context_request) -> <<"Context Request">>;
msg_description_v2(context_response) -> <<"Context Response">>;
msg_description_v2(context_acknowledge) -> <<"Context Acknowledge">>;
msg_description_v2(forward_relocation_request) -> <<"Forward Relocation Request">>;
msg_description_v2(forward_relocation_response) -> <<"Forward Relocation Response">>;
msg_description_v2(forward_relocation_complete_notification) -> <<"Forward Relocation Complete Notification">>;
msg_description_v2(forward_relocation_complete_acknowledge) -> <<"Forward Relocation Complete Acknowledge">>;
msg_description_v2(forward_access_context_notification) -> <<"Forward Access Context Notification">>;
msg_description_v2(forward_access_context_acknowledge) -> <<"Forward Access Context Acknowledge">>;
msg_description_v2(relocation_cancel_request) -> <<"Relocation Cancel Request">>;
msg_description_v2(relocation_cancel_response) -> <<"Relocation Cancel Response">>;
msg_description_v2(configuration_transfer_tunnel) -> <<"Configuration Transfer Tunnel">>;
msg_description_v2(detach_notification) -> <<"Detach Notification">>;
msg_description_v2(detach_acknowledge) -> <<"Detach Acknowledge">>;
msg_description_v2(cs_paging_indication) -> <<"CS Paging Indication">>;
msg_description_v2(ran_information_relay) -> <<"RAN Information Relay">>;
msg_description_v2(alert_mme_notification) -> <<"Alert MME Notification">>;
msg_description_v2(alert_mme_acknowledge) -> <<"Alert MME Acknowledge">>;
msg_description_v2(ue_activity_notification) -> <<"UE Activity Notification">>;
msg_description_v2(ue_activity_acknowledge) -> <<"UE Activity Acknowledge">>;
msg_description_v2(isr_status_indication) -> <<"ISR Status Indication">>;
msg_description_v2(create_forwarding_tunnel_request) -> <<"Create Forwarding Tunnel Request">>;
msg_description_v2(create_forwarding_tunnel_response) -> <<"Create Forwarding Tunnel Response">>;
msg_description_v2(suspend_notification) -> <<"Suspend Notification">>;
msg_description_v2(suspend_acknowledge) -> <<"Suspend Acknowledge">>;
msg_description_v2(resume_notification) -> <<"Resume Notification">>;
msg_description_v2(resume_acknowledge) -> <<"Resume Acknowledge">>;
msg_description_v2(create_indirect_data_forwarding_tunnel_request) -> <<"Create Indirect Data Forwarding Tunnel Request">>;
msg_description_v2(create_indirect_data_forwarding_tunnel_response) -> <<"Create Indirect Data Forwarding Tunnel Response">>;
msg_description_v2(delete_indirect_data_forwarding_tunnel_request) -> <<"Delete Indirect Data Forwarding Tunnel Request">>;
msg_description_v2(delete_indirect_data_forwarding_tunnel_response) -> <<"Delete Indirect Data Forwarding Tunnel Response">>;
msg_description_v2(release_access_bearers_request) -> <<"Release Access Bearers Request">>;
msg_description_v2(release_access_bearers_response) -> <<"Release Access Bearers Response">>;
msg_description_v2(downlink_data_notification) -> <<"Downlink Data Notification">>;
msg_description_v2(downlink_data_notification_acknowledge) -> <<"Downlink Data Notification Acknowledge">>;
msg_description_v2(pgw_restart_notification) -> <<"PGW Restart Notification">>;
msg_description_v2(pgw_restart_notification_acknowledge) -> <<"PGW Restart Notification Acknowledge">>;
msg_description_v2(update_pdn_connection_set_request) -> <<"Update PDN Connection Set Request">>;
msg_description_v2(update_pdn_connection_set_response) -> <<"Update PDN Connection Set Response">>;
msg_description_v2(mbms_session_start_response) -> <<"MBMS Session Start Response">>;
msg_description_v2(mbms_session_update_request) -> <<"MBMS Session Update Request">>;
msg_description_v2(mbms_session_update_response) -> <<"MBMS Session Update Response">>;
msg_description_v2(mbms_session_stop_request) -> <<"MBMS Session Stop Request">>;
msg_description_v2(mbms_session_stop_response) -> <<"MBMS Session Stop Response">>;
msg_description_v2(X) -> io_lib:format("~p", [X]).

message_type_v2(echo_request) -> 1;
message_type_v2(echo_response) -> 2;
message_type_v2(version_not_supported) -> 3;
message_type_v2(create_session_request) -> 32;
message_type_v2(create_session_response) -> 33;
message_type_v2(delete_session_request) -> 36;
message_type_v2(delete_session_response) -> 37;
message_type_v2(modify_bearer_request) -> 34;
message_type_v2(modify_bearer_response) -> 35;
message_type_v2(change_notification_request) -> 38;
message_type_v2(change_notification_response) -> 39;
message_type_v2(modify_bearer_command) -> 64;
message_type_v2(modify_bearer_failure_indication) -> 65;
message_type_v2(delete_bearer_command) -> 66;
message_type_v2(delete_bearer_failure_indication) -> 67;
message_type_v2(bearer_resource_command) -> 68;
message_type_v2(bearer_resource_failure_indication) -> 69;
message_type_v2(downlink_data_notification_failure_indication) -> 70;
message_type_v2(trace_session_activation) -> 71;
message_type_v2(trace_session_deactivation) -> 72;
message_type_v2(stop_paging_indication) -> 73;
message_type_v2(create_bearer_request) -> 95;
message_type_v2(create_bearer_response) -> 96;
message_type_v2(update_bearer_request) -> 97;
message_type_v2(update_bearer_response) -> 98;
message_type_v2(delete_bearer_request) -> 99;
message_type_v2(delete_bearer_response) -> 100;
message_type_v2(delete_pdn_connection_set_request) -> 101;
message_type_v2(delete_pdn_connection_set_response) -> 102;
message_type_v2(pgw_downlink_triggering_notification) -> 103;
message_type_v2(pgw_downlink_triggering_acknowledge) -> 104;
message_type_v2(identification_request) -> 128;
message_type_v2(identification_response) -> 129;
message_type_v2(context_request) -> 130;
message_type_v2(context_response) -> 131;
message_type_v2(context_acknowledge) -> 132;
message_type_v2(forward_relocation_request) -> 133;
message_type_v2(forward_relocation_response) -> 134;
message_type_v2(forward_relocation_complete_notification) -> 135;
message_type_v2(forward_relocation_complete_acknowledge) -> 136;
message_type_v2(forward_access_context_notification) -> 137;
message_type_v2(forward_access_context_acknowledge) -> 138;
message_type_v2(relocation_cancel_request) -> 139;
message_type_v2(relocation_cancel_response) -> 140;
message_type_v2(configuration_transfer_tunnel) -> 141;
message_type_v2(detach_notification) -> 149;
message_type_v2(detach_acknowledge) -> 150;
message_type_v2(cs_paging_indication) -> 151;
message_type_v2(ran_information_relay) -> 152;
message_type_v2(alert_mme_notification) -> 153;
message_type_v2(alert_mme_acknowledge) -> 154;
message_type_v2(ue_activity_notification) -> 155;
message_type_v2(ue_activity_acknowledge) -> 156;
message_type_v2(isr_status_indication) -> 157;
message_type_v2(create_forwarding_tunnel_request) -> 160;
message_type_v2(create_forwarding_tunnel_response) -> 161;
message_type_v2(suspend_notification) -> 162;
message_type_v2(suspend_acknowledge) -> 163;
message_type_v2(resume_notification) -> 164;
message_type_v2(resume_acknowledge) -> 165;
message_type_v2(create_indirect_data_forwarding_tunnel_request) -> 166;
message_type_v2(create_indirect_data_forwarding_tunnel_response) -> 167;
message_type_v2(delete_indirect_data_forwarding_tunnel_request) -> 168;
message_type_v2(delete_indirect_data_forwarding_tunnel_response) -> 169;
message_type_v2(release_access_bearers_request) -> 170;
message_type_v2(release_access_bearers_response) -> 171;
message_type_v2(downlink_data_notification) -> 176;
message_type_v2(downlink_data_notification_acknowledge) -> 177;
message_type_v2(pgw_restart_notification) -> 179;
message_type_v2(pgw_restart_notification_acknowledge) -> 180;
message_type_v2(update_pdn_connection_set_request) -> 200;
message_type_v2(update_pdn_connection_set_response) -> 201;
message_type_v2(mbms_session_start_response) -> 232;
message_type_v2(mbms_session_update_request) -> 233;
message_type_v2(mbms_session_update_response) -> 234;
message_type_v2(mbms_session_stop_request) -> 235;
message_type_v2(mbms_session_stop_response) -> 236;
message_type_v2(1) -> echo_request;
message_type_v2(2) -> echo_response;
message_type_v2(3) -> version_not_supported;
message_type_v2(32) -> create_session_request;
message_type_v2(33) -> create_session_response;
message_type_v2(36) -> delete_session_request;
message_type_v2(37) -> delete_session_response;
message_type_v2(34) -> modify_bearer_request;
message_type_v2(35) -> modify_bearer_response;
message_type_v2(38) -> change_notification_request;
message_type_v2(39) -> change_notification_response;
message_type_v2(64) -> modify_bearer_command;
message_type_v2(65) -> modify_bearer_failure_indication;
message_type_v2(66) -> delete_bearer_command;
message_type_v2(67) -> delete_bearer_failure_indication;
message_type_v2(68) -> bearer_resource_command;
message_type_v2(69) -> bearer_resource_failure_indication;
message_type_v2(70) -> downlink_data_notification_failure_indication;
message_type_v2(71) -> trace_session_activation;
message_type_v2(72) -> trace_session_deactivation;
message_type_v2(73) -> stop_paging_indication;
message_type_v2(95) -> create_bearer_request;
message_type_v2(96) -> create_bearer_response;
message_type_v2(97) -> update_bearer_request;
message_type_v2(98) -> update_bearer_response;
message_type_v2(99) -> delete_bearer_request;
message_type_v2(100) -> delete_bearer_response;
message_type_v2(101) -> delete_pdn_connection_set_request;
message_type_v2(102) -> delete_pdn_connection_set_response;
message_type_v2(103) -> pgw_downlink_triggering_notification;
message_type_v2(104) -> pgw_downlink_triggering_acknowledge;
message_type_v2(128) -> identification_request;
message_type_v2(129) -> identification_response;
message_type_v2(130) -> context_request;
message_type_v2(131) -> context_response;
message_type_v2(132) -> context_acknowledge;
message_type_v2(133) -> forward_relocation_request;
message_type_v2(134) -> forward_relocation_response;
message_type_v2(135) -> forward_relocation_complete_notification;
message_type_v2(136) -> forward_relocation_complete_acknowledge;
message_type_v2(137) -> forward_access_context_notification;
message_type_v2(138) -> forward_access_context_acknowledge;
message_type_v2(139) -> relocation_cancel_request;
message_type_v2(140) -> relocation_cancel_response;
message_type_v2(141) -> configuration_transfer_tunnel;
message_type_v2(149) -> detach_notification;
message_type_v2(150) -> detach_acknowledge;
message_type_v2(151) -> cs_paging_indication;
message_type_v2(152) -> ran_information_relay;
message_type_v2(153) -> alert_mme_notification;
message_type_v2(154) -> alert_mme_acknowledge;
message_type_v2(155) -> ue_activity_notification;
message_type_v2(156) -> ue_activity_acknowledge;
message_type_v2(157) -> isr_status_indication;
message_type_v2(160) -> create_forwarding_tunnel_request;
message_type_v2(161) -> create_forwarding_tunnel_response;
message_type_v2(162) -> suspend_notification;
message_type_v2(163) -> suspend_acknowledge;
message_type_v2(164) -> resume_notification;
message_type_v2(165) -> resume_acknowledge;
message_type_v2(166) -> create_indirect_data_forwarding_tunnel_request;
message_type_v2(167) -> create_indirect_data_forwarding_tunnel_response;
message_type_v2(168) -> delete_indirect_data_forwarding_tunnel_request;
message_type_v2(169) -> delete_indirect_data_forwarding_tunnel_response;
message_type_v2(170) -> release_access_bearers_request;
message_type_v2(171) -> release_access_bearers_response;
message_type_v2(176) -> downlink_data_notification;
message_type_v2(177) -> downlink_data_notification_acknowledge;
message_type_v2(179) -> pgw_restart_notification;
message_type_v2(180) -> pgw_restart_notification_acknowledge;
message_type_v2(200) -> update_pdn_connection_set_request;
message_type_v2(201) -> update_pdn_connection_set_response;
message_type_v2(232) -> mbms_session_start_response;
message_type_v2(233) -> mbms_session_update_request;
message_type_v2(234) -> mbms_session_update_response;
message_type_v2(235) -> mbms_session_stop_request;
message_type_v2(236) -> mbms_session_stop_response;
message_type_v2(Type) -> error(badarg, [Type]).

enum_v2_pdn_type(ipv4) -> 1;
enum_v2_pdn_type(ipv6) -> 2;
enum_v2_pdn_type(ipv4v6) -> 3;
enum_v2_pdn_type(non_ip) -> 4;
enum_v2_pdn_type(1) -> ipv4;
enum_v2_pdn_type(2) -> ipv6;
enum_v2_pdn_type(3) -> ipv4v6;
enum_v2_pdn_type(4) -> non_ip;
enum_v2_pdn_type(X) when is_integer(X) -> X.

enum_v2_type(ipv4) -> 1;
enum_v2_type(ipv6) -> 2;
enum_v2_type(ipv4v6) -> 3;
enum_v2_type(non_ip) -> 4;
enum_v2_type(1) -> ipv4;
enum_v2_type(2) -> ipv6;
enum_v2_type(3) -> ipv4v6;
enum_v2_type(4) -> non_ip;
enum_v2_type(X) when is_integer(X) -> X.

enum_v2_v2_cause(reserved) -> 1;
enum_v2_v2_cause(local_detach) -> 2;
enum_v2_v2_cause(complete_detach) -> 3;
enum_v2_v2_cause(rat_changed_from_3gpp_to_non_3gpp) -> 4;
enum_v2_v2_cause(isr_deactivation) -> 5;
enum_v2_v2_cause(error_indication_received_from_rnc_enodeb_s4_sgsn) -> 6;
enum_v2_v2_cause(imsi_detach_only) -> 7;
enum_v2_v2_cause(reactivation_requested) -> 8;
enum_v2_v2_cause(pdn_reconnection_to_this_apn_disallowed) -> 9;
enum_v2_v2_cause(access_changed_from_non_3gpp_to_3gpp) -> 10;
enum_v2_v2_cause(pdn_connection_inactivity_timer_expires) -> 11;
enum_v2_v2_cause(pgw_not_responding) -> 12;
enum_v2_v2_cause(network_failure) -> 13;
enum_v2_v2_cause(qos_parameter_mismatch) -> 14;
enum_v2_v2_cause(request_accepted) -> 16;
enum_v2_v2_cause(request_accepted_partially) -> 17;
enum_v2_v2_cause(new_pdn_type_due_to_network_preference) -> 18;
enum_v2_v2_cause(new_pdn_type_due_to_single_address_bearer_only) -> 19;
enum_v2_v2_cause(context_not_found) -> 64;
enum_v2_v2_cause(invalid_message_format) -> 65;
enum_v2_v2_cause(version_not_supported_by_next_peer) -> 66;
enum_v2_v2_cause(invalid_length) -> 67;
enum_v2_v2_cause(service_not_supported) -> 68;
enum_v2_v2_cause(mandatory_ie_incorrect) -> 69;
enum_v2_v2_cause(mandatory_ie_missing) -> 70;
enum_v2_v2_cause(system_failure) -> 72;
enum_v2_v2_cause(no_resources_available) -> 73;
enum_v2_v2_cause(semantic_error_in_the_tft_operation) -> 74;
enum_v2_v2_cause(syntactic_error_in_the_tft_operation) -> 75;
enum_v2_v2_cause(semantic_errors_in_packet_filter) -> 76;
enum_v2_v2_cause(syntactic_errors_in_packet_filter) -> 77;
enum_v2_v2_cause(missing_or_unknown_apn) -> 78;
enum_v2_v2_cause(gre_key_not_found) -> 80;
enum_v2_v2_cause(relocation_failure) -> 81;
enum_v2_v2_cause(denied_in_rat) -> 82;
enum_v2_v2_cause(preferred_pdn_type_not_supported) -> 83;
enum_v2_v2_cause(all_dynamic_addresses_are_occupied) -> 84;
enum_v2_v2_cause(ue_context_without_tft_already_activated) -> 85;
enum_v2_v2_cause(protocol_type_not_supported) -> 86;
enum_v2_v2_cause(ue_not_responding) -> 87;
enum_v2_v2_cause(ue_refuses) -> 88;
enum_v2_v2_cause(service_denied) -> 89;
enum_v2_v2_cause(unable_to_page_ue) -> 90;
enum_v2_v2_cause(no_memory_available) -> 91;
enum_v2_v2_cause(user_authentication_failed) -> 92;
enum_v2_v2_cause(apn_access_denied___no_subscription) -> 93;
enum_v2_v2_cause(request_rejected) -> 94;
enum_v2_v2_cause(p_tmsi_signature_mismatch) -> 95;
enum_v2_v2_cause(imsi_imei_not_known) -> 96;
enum_v2_v2_cause(semantic_error_in_the_tad_operation) -> 97;
enum_v2_v2_cause(syntactic_error_in_the_tad_operation) -> 98;
enum_v2_v2_cause(remote_peer_not_responding) -> 100;
enum_v2_v2_cause(collision_with_network_initiated_request) -> 101;
enum_v2_v2_cause(unable_to_page_ue_due_to_suspension) -> 102;
enum_v2_v2_cause(conditional_ie_missing) -> 103;
enum_v2_v2_cause(apn_restriction_type_incompatible_with_currently_active_pdn_connection) -> 104;
enum_v2_v2_cause(invalid_overall_length_of_the_triggered_response_message_and_a_piggybacked_initial_message) -> 105;
enum_v2_v2_cause(data_forwarding_not_supported) -> 106;
enum_v2_v2_cause(invalid_reply_from_remote_peer) -> 107;
enum_v2_v2_cause(fallback_to_gtpv1) -> 108;
enum_v2_v2_cause(invalid_peer) -> 109;
enum_v2_v2_cause(temporarily_rejected_due_to_handover_tau_rau_procedure_in_progress) -> 110;
enum_v2_v2_cause(modifications_not_limited_to_s1_u_bearers) -> 111;
enum_v2_v2_cause(request_rejected_for_a_pmipv6_reason) -> 112;
enum_v2_v2_cause(apn_congestion) -> 113;
enum_v2_v2_cause(bearer_handling_not_supported) -> 114;
enum_v2_v2_cause(ue_already_re_attached) -> 115;
enum_v2_v2_cause(multiple_pdn_connections_for_a_given_apn_not_allowed) -> 116;
enum_v2_v2_cause(target_access_restricted_for_the_subscriber) -> 117;
enum_v2_v2_cause(mme_sgsn_refuses_due_to_vplmn_policy) -> 119;
enum_v2_v2_cause(gtp_c_entity_congestion) -> 120;
enum_v2_v2_cause(1) -> reserved;
enum_v2_v2_cause(2) -> local_detach;
enum_v2_v2_cause(3) -> complete_detach;
enum_v2_v2_cause(4) -> rat_changed_from_3gpp_to_non_3gpp;
enum_v2_v2_cause(5) -> isr_deactivation;
enum_v2_v2_cause(6) -> error_indication_received_from_rnc_enodeb_s4_sgsn;
enum_v2_v2_cause(7) -> imsi_detach_only;
enum_v2_v2_cause(8) -> reactivation_requested;
enum_v2_v2_cause(9) -> pdn_reconnection_to_this_apn_disallowed;
enum_v2_v2_cause(10) -> access_changed_from_non_3gpp_to_3gpp;
enum_v2_v2_cause(11) -> pdn_connection_inactivity_timer_expires;
enum_v2_v2_cause(12) -> pgw_not_responding;
enum_v2_v2_cause(13) -> network_failure;
enum_v2_v2_cause(14) -> qos_parameter_mismatch;
enum_v2_v2_cause(16) -> request_accepted;
enum_v2_v2_cause(17) -> request_accepted_partially;
enum_v2_v2_cause(18) -> new_pdn_type_due_to_network_preference;
enum_v2_v2_cause(19) -> new_pdn_type_due_to_single_address_bearer_only;
enum_v2_v2_cause(64) -> context_not_found;
enum_v2_v2_cause(65) -> invalid_message_format;
enum_v2_v2_cause(66) -> version_not_supported_by_next_peer;
enum_v2_v2_cause(67) -> invalid_length;
enum_v2_v2_cause(68) -> service_not_supported;
enum_v2_v2_cause(69) -> mandatory_ie_incorrect;
enum_v2_v2_cause(70) -> mandatory_ie_missing;
enum_v2_v2_cause(72) -> system_failure;
enum_v2_v2_cause(73) -> no_resources_available;
enum_v2_v2_cause(74) -> semantic_error_in_the_tft_operation;
enum_v2_v2_cause(75) -> syntactic_error_in_the_tft_operation;
enum_v2_v2_cause(76) -> semantic_errors_in_packet_filter;
enum_v2_v2_cause(77) -> syntactic_errors_in_packet_filter;
enum_v2_v2_cause(78) -> missing_or_unknown_apn;
enum_v2_v2_cause(80) -> gre_key_not_found;
enum_v2_v2_cause(81) -> relocation_failure;
enum_v2_v2_cause(82) -> denied_in_rat;
enum_v2_v2_cause(83) -> preferred_pdn_type_not_supported;
enum_v2_v2_cause(84) -> all_dynamic_addresses_are_occupied;
enum_v2_v2_cause(85) -> ue_context_without_tft_already_activated;
enum_v2_v2_cause(86) -> protocol_type_not_supported;
enum_v2_v2_cause(87) -> ue_not_responding;
enum_v2_v2_cause(88) -> ue_refuses;
enum_v2_v2_cause(89) -> service_denied;
enum_v2_v2_cause(90) -> unable_to_page_ue;
enum_v2_v2_cause(91) -> no_memory_available;
enum_v2_v2_cause(92) -> user_authentication_failed;
enum_v2_v2_cause(93) -> apn_access_denied___no_subscription;
enum_v2_v2_cause(94) -> request_rejected;
enum_v2_v2_cause(95) -> p_tmsi_signature_mismatch;
enum_v2_v2_cause(96) -> imsi_imei_not_known;
enum_v2_v2_cause(97) -> semantic_error_in_the_tad_operation;
enum_v2_v2_cause(98) -> syntactic_error_in_the_tad_operation;
enum_v2_v2_cause(100) -> remote_peer_not_responding;
enum_v2_v2_cause(101) -> collision_with_network_initiated_request;
enum_v2_v2_cause(102) -> unable_to_page_ue_due_to_suspension;
enum_v2_v2_cause(103) -> conditional_ie_missing;
enum_v2_v2_cause(104) -> apn_restriction_type_incompatible_with_currently_active_pdn_connection;
enum_v2_v2_cause(105) -> invalid_overall_length_of_the_triggered_response_message_and_a_piggybacked_initial_message;
enum_v2_v2_cause(106) -> data_forwarding_not_supported;
enum_v2_v2_cause(107) -> invalid_reply_from_remote_peer;
enum_v2_v2_cause(108) -> fallback_to_gtpv1;
enum_v2_v2_cause(109) -> invalid_peer;
enum_v2_v2_cause(110) -> temporarily_rejected_due_to_handover_tau_rau_procedure_in_progress;
enum_v2_v2_cause(111) -> modifications_not_limited_to_s1_u_bearers;
enum_v2_v2_cause(112) -> request_rejected_for_a_pmipv6_reason;
enum_v2_v2_cause(113) -> apn_congestion;
enum_v2_v2_cause(114) -> bearer_handling_not_supported;
enum_v2_v2_cause(115) -> ue_already_re_attached;
enum_v2_v2_cause(116) -> multiple_pdn_connections_for_a_given_apn_not_allowed;
enum_v2_v2_cause(117) -> target_access_restricted_for_the_subscriber;
enum_v2_v2_cause(119) -> mme_sgsn_refuses_due_to_vplmn_policy;
enum_v2_v2_cause(120) -> gtp_c_entity_congestion;
enum_v2_v2_cause(X) when is_integer(X) -> X.

decode_v2_element(<<M_imsi/binary>>, 1, Instance) ->
    #v2_international_mobile_subscriber_identity{instance = Instance,
						 imsi = decode_tbcd(M_imsi)};

decode_v2_element(<<M_v2_cause:8/integer,
		    _:5,
		    M_pce:1/integer,
		    M_bce:1/integer,
		    M_cs:1/integer,
		    _/binary>>, 2, Instance) ->
    #v2_cause{instance = Instance,
	      v2_cause = enum_v2_v2_cause(M_v2_cause),
	      pce = M_pce,
	      bce = M_bce,
	      cs = M_cs};

decode_v2_element(<<M_restart_counter:8/integer,
		    _/binary>>, 3, Instance) ->
    #v2_recovery{instance = Instance,
		 restart_counter = M_restart_counter};

decode_v2_element(<<>>, 51, Instance) ->
    #v2_stn_sr{instance = Instance};

decode_v2_element(<<M_apn/binary>>, 71, Instance) ->
    #v2_access_point_name{instance = Instance,
			  apn = decode_fqdn(M_apn)};

decode_v2_element(<<M_uplink:32/integer,
		    M_downlink:32/integer,
		    _/binary>>, 72, Instance) ->
    #v2_aggregate_maximum_bit_rate{instance = Instance,
				   uplink = M_uplink,
				   downlink = M_downlink};

decode_v2_element(<<_:4,
		    M_eps_bearer_id:4/integer,
		    _/binary>>, 73, Instance) ->
    #v2_eps_bearer_id{instance = Instance,
		      eps_bearer_id = M_eps_bearer_id};

decode_v2_element(<<M_ip/binary>>, 74, Instance) ->
    #v2_ip_address{instance = Instance,
		   ip = M_ip};

decode_v2_element(<<M_mei/binary>>, 75, Instance) ->
    #v2_mobile_equipment_identity{instance = Instance,
				  mei = M_mei};

decode_v2_element(<<M_msisdn/binary>>, 76, Instance) ->
    #v2_msisdn{instance = Instance,
	       msisdn = decode_tbcd(M_msisdn)};

decode_v2_element(<<M_flags/binary>>, 77, Instance) ->
    #v2_indication{instance = Instance,
		   flags = decode_v2_indication_flags(M_flags)};

decode_v2_element(<<M_config/binary>>, 78, Instance) ->
    #v2_protocol_configuration_options{instance = Instance,
				       config = decode_protocol_config_opts(M_config)};

decode_v2_element(<<_:5,
		    M_type:3/integer,
		    M_address/binary>>, 79, Instance) ->
    #v2_pdn_address_allocation{instance = Instance,
			       type = enum_v2_type(M_type),
			       address = M_address};

decode_v2_element(<<_:1,
		    M_pci:1/integer,
		    M_pl:4/integer,
		    _:1,
		    M_pvi:1/integer,
		    M_label:8/integer,
		    M_maximum_bit_rate_for_uplink:40/integer,
		    M_maximum_bit_rate_for_downlink:40/integer,
		    M_guaranteed_bit_rate_for_uplink:40/integer,
		    M_guaranteed_bit_rate_for_downlink:40/integer,
		    _/binary>>, 80, Instance) ->
    #v2_bearer_level_quality_of_service{instance = Instance,
					pci = M_pci,
					pl = M_pl,
					pvi = M_pvi,
					label = M_label,
					maximum_bit_rate_for_uplink = M_maximum_bit_rate_for_uplink,
					maximum_bit_rate_for_downlink = M_maximum_bit_rate_for_downlink,
					guaranteed_bit_rate_for_uplink = M_guaranteed_bit_rate_for_uplink,
					guaranteed_bit_rate_for_downlink = M_guaranteed_bit_rate_for_downlink};

decode_v2_element(<<>>, 81, Instance) ->
    #v2_flow_quality_of_service{instance = Instance};

decode_v2_element(<<M_rat_type:8/integer,
		    _/binary>>, 82, Instance) ->
    #v2_rat_type{instance = Instance,
		 rat_type = M_rat_type};

decode_v2_element(<<Data/binary>>, 83, Instance) ->
    decode_v2_mccmnc(Data, Instance);

decode_v2_element(<<>>, 84, Instance) ->
    #v2_eps_bearer_level_traffic_flow_template{instance = Instance};

decode_v2_element(<<>>, 85, Instance) ->
    #v2_traffic_aggregation_description{instance = Instance};

decode_v2_element(<<Data/binary>>, 86, Instance) ->
    decode_v2_user_location_information(Data, Instance);

decode_v2_element(<<Data/binary>>, 87, Instance) ->
    decode_v2_fully_qualified_tunnel_endpoint_identifier(Data, Instance);

decode_v2_element(<<>>, 88, Instance) ->
    #v2_tmsi{instance = Instance};

decode_v2_element(<<>>, 89, Instance) ->
    #v2_global_cn_id{instance = Instance};

decode_v2_element(<<>>, 90, Instance) ->
    #v2_s103_pdn_data_forwarding_info{instance = Instance};

decode_v2_element(<<>>, 91, Instance) ->
    #v2_s1_u_data_forwarding_info{instance = Instance};

decode_v2_element(<<>>, 92, Instance) ->
    #v2_delay_value{instance = Instance};

decode_v2_element(<<M_group/binary>>, 93, Instance) ->
    #v2_bearer_context{instance = Instance,
		       group = decode_v2_grouped(M_group)};

decode_v2_element(<<M_id:4/bytes>>, 94, Instance) ->
    #v2_charging_id{instance = Instance,
		    id = M_id};

decode_v2_element(<<>>, 95, Instance) ->
    #v2_charging_characteristics{instance = Instance};

decode_v2_element(<<>>, 96, Instance) ->
    #v2_trace_information{instance = Instance};

decode_v2_element(<<>>, 97, Instance) ->
    #v2_bearer_flags{instance = Instance};

decode_v2_element(<<_:4,
		    M_pdn_type:4/integer,
		    _/binary>>, 99, Instance) ->
    #v2_pdn_type{instance = Instance,
		 pdn_type = enum_v2_pdn_type(M_pdn_type)};

decode_v2_element(<<>>, 100, Instance) ->
    #v2_procedure_transaction_id{instance = Instance};

decode_v2_element(<<>>, 103, Instance) ->
    #v2_mm_context_1{instance = Instance};

decode_v2_element(<<>>, 104, Instance) ->
    #v2_mm_context_2{instance = Instance};

decode_v2_element(<<>>, 105, Instance) ->
    #v2_mm_context_3{instance = Instance};

decode_v2_element(<<>>, 106, Instance) ->
    #v2_mm_context_4{instance = Instance};

decode_v2_element(<<>>, 107, Instance) ->
    #v2_mm_context_5{instance = Instance};

decode_v2_element(<<>>, 108, Instance) ->
    #v2_mm_context_6{instance = Instance};

decode_v2_element(<<>>, 109, Instance) ->
    #v2_pdn_connection{instance = Instance};

decode_v2_element(<<>>, 110, Instance) ->
    #v2_pdu_numbers{instance = Instance};

decode_v2_element(<<>>, 111, Instance) ->
    #v2_p_tmsi{instance = Instance};

decode_v2_element(<<>>, 112, Instance) ->
    #v2_p_tmsi_signature{instance = Instance};

decode_v2_element(<<>>, 113, Instance) ->
    #v2_hop_counter{instance = Instance};

decode_v2_element(<<M_timezone:8/integer,
		    _:6,
		    M_dst:2/integer,
		    _/binary>>, 114, Instance) ->
    #v2_ue_time_zone{instance = Instance,
		     timezone = M_timezone,
		     dst = M_dst};

decode_v2_element(<<>>, 115, Instance) ->
    #v2_trace_reference{instance = Instance};

decode_v2_element(<<>>, 116, Instance) ->
    #v2_complete_request_message{instance = Instance};

decode_v2_element(<<>>, 117, Instance) ->
    #v2_guti{instance = Instance};

decode_v2_element(<<>>, 118, Instance) ->
    #v2_f_container{instance = Instance};

decode_v2_element(<<>>, 119, Instance) ->
    #v2_f_cause{instance = Instance};

decode_v2_element(<<>>, 120, Instance) ->
    #v2_plmn_id{instance = Instance};

decode_v2_element(<<>>, 121, Instance) ->
    #v2_target_identification{instance = Instance};

decode_v2_element(<<>>, 123, Instance) ->
    #v2_packet_flow_id_{instance = Instance};

decode_v2_element(<<>>, 124, Instance) ->
    #v2_rab_context_{instance = Instance};

decode_v2_element(<<>>, 125, Instance) ->
    #v2_source_rnc_pdcp_context_info{instance = Instance};

decode_v2_element(<<>>, 126, Instance) ->
    #v2_udp_source_port_number{instance = Instance};

decode_v2_element(<<M_restriction_type_value:8/integer,
		    _/binary>>, 127, Instance) ->
    #v2_apn_restriction{instance = Instance,
			restriction_type_value = M_restriction_type_value};

decode_v2_element(<<_:6,
		    M_mode:2/integer,
		    _/binary>>, 128, Instance) ->
    #v2_selection_mode{instance = Instance,
		       mode = M_mode};

decode_v2_element(<<>>, 129, Instance) ->
    #v2_source_identification{instance = Instance};

decode_v2_element(<<>>, 131, Instance) ->
    #v2_change_reporting_action{instance = Instance};

decode_v2_element(<<>>, 132, Instance) ->
    #v2_fully_qualified_pdn_connection_set_identifier{instance = Instance};

decode_v2_element(<<>>, 133, Instance) ->
    #v2_channel_needed{instance = Instance};

decode_v2_element(<<>>, 134, Instance) ->
    #v2_emlpp_priority{instance = Instance};

decode_v2_element(<<>>, 135, Instance) ->
    #v2_node_type{instance = Instance};

decode_v2_element(<<M_fqdn/binary>>, 136, Instance) ->
    #v2_fully_qualified_domain_name{instance = Instance,
				    fqdn = decode_fqdn(M_fqdn)};

decode_v2_element(<<>>, 137, Instance) ->
    #v2_transaction_identifier{instance = Instance};

decode_v2_element(<<>>, 138, Instance) ->
    #v2_mbms_session_duration{instance = Instance};

decode_v2_element(<<>>, 139, Instance) ->
    #v2_mbms_service_area{instance = Instance};

decode_v2_element(<<>>, 140, Instance) ->
    #v2_mbms_session_identifier{instance = Instance};

decode_v2_element(<<>>, 141, Instance) ->
    #v2_mbms_flow_identifier{instance = Instance};

decode_v2_element(<<>>, 142, Instance) ->
    #v2_mbms_ip_multicast_distribution{instance = Instance};

decode_v2_element(<<>>, 143, Instance) ->
    #v2_mbms_distribution_acknowledge{instance = Instance};

decode_v2_element(<<>>, 144, Instance) ->
    #v2_rfsp_index{instance = Instance};

decode_v2_element(<<>>, 145, Instance) ->
    #v2_user_csg_information{instance = Instance};

decode_v2_element(<<>>, 146, Instance) ->
    #v2_csg_information_reporting_action{instance = Instance};

decode_v2_element(<<>>, 147, Instance) ->
    #v2_csg_id{instance = Instance};

decode_v2_element(<<>>, 148, Instance) ->
    #v2_csg_membership_indication{instance = Instance};

decode_v2_element(<<>>, 149, Instance) ->
    #v2_service_indicator{instance = Instance};

decode_v2_element(<<>>, 150, Instance) ->
    #v2_detach_type{instance = Instance};

decode_v2_element(<<>>, 151, Instance) ->
    #v2_local_distiguished_name{instance = Instance};

decode_v2_element(<<>>, 152, Instance) ->
    #v2_node_features{instance = Instance};

decode_v2_element(<<>>, 153, Instance) ->
    #v2_mbms_time_to_data_transfer{instance = Instance};

decode_v2_element(<<>>, 154, Instance) ->
    #v2_throttling{instance = Instance};

decode_v2_element(<<>>, 155, Instance) ->
    #v2_allocation_retention_priority{instance = Instance};

decode_v2_element(<<>>, 156, Instance) ->
    #v2_epc_timer{instance = Instance};

decode_v2_element(<<>>, 157, Instance) ->
    #v2_signalling_priority_indication{instance = Instance};

decode_v2_element(<<>>, 158, Instance) ->
    #v2_temporary_mobile_group_identity{instance = Instance};

decode_v2_element(<<>>, 159, Instance) ->
    #v2_additional_mm_context_for_srvcc{instance = Instance};

decode_v2_element(<<>>, 160, Instance) ->
    #v2_additional_flags_for_srvcc{instance = Instance};

decode_v2_element(<<>>, 162, Instance) ->
    #v2_mdt_configuration{instance = Instance};

decode_v2_element(<<>>, 163, Instance) ->
    #v2_additional_protocol_configuration_options{instance = Instance};

decode_v2_element(<<>>, 164, Instance) ->
    #v2_absolute_time_of_mbms_data_transfer{instance = Instance};

decode_v2_element(<<>>, 165, Instance) ->
    #v2_henb_information_reporting_{instance = Instance};

decode_v2_element(<<>>, 166, Instance) ->
    #v2_ipv4_configuration_parameters{instance = Instance};

decode_v2_element(<<>>, 167, Instance) ->
    #v2_change_to_report_flags_{instance = Instance};

decode_v2_element(<<>>, 168, Instance) ->
    #v2_action_indication{instance = Instance};

decode_v2_element(<<>>, 169, Instance) ->
    #v2_twan_identifier{instance = Instance};

decode_v2_element(<<>>, 170, Instance) ->
    #v2_uli_timestamp{instance = Instance};

decode_v2_element(<<>>, 171, Instance) ->
    #v2_mbms_flags{instance = Instance};

decode_v2_element(<<>>, 172, Instance) ->
    #v2_ran_nas_cause{instance = Instance};

decode_v2_element(<<>>, 173, Instance) ->
    #v2_cn_operator_selection_entity{instance = Instance};

decode_v2_element(<<>>, 174, Instance) ->
    #v2_trusted_wlan_mode_indication{instance = Instance};

decode_v2_element(<<>>, 175, Instance) ->
    #v2_node_number{instance = Instance};

decode_v2_element(<<>>, 176, Instance) ->
    #v2_node_identifier{instance = Instance};

decode_v2_element(<<>>, 177, Instance) ->
    #v2_presence_reporting_area_action{instance = Instance};

decode_v2_element(<<>>, 178, Instance) ->
    #v2_presence_reporting_area_information{instance = Instance};

decode_v2_element(<<>>, 179, Instance) ->
    #v2_twan_identifier_timestamp{instance = Instance};

decode_v2_element(<<>>, 180, Instance) ->
    #v2_overload_control_information{instance = Instance};

decode_v2_element(<<>>, 181, Instance) ->
    #v2_load_control_information{instance = Instance};

decode_v2_element(<<>>, 182, Instance) ->
    #v2_metric{instance = Instance};

decode_v2_element(<<>>, 183, Instance) ->
    #v2_sequence_number{instance = Instance};

decode_v2_element(<<>>, 184, Instance) ->
    #v2_apn_and_relative_capacity{instance = Instance};

decode_v2_element(<<>>, 185, Instance) ->
    #v2_wlan_offloadability_indication{instance = Instance};

decode_v2_element(<<>>, 255, Instance) ->
    #v2_private_extension{instance = Instance};

decode_v2_element(Value, Tag, Instance) ->
    {Tag, Instance, Value}.

encode_v2_element(#v2_international_mobile_subscriber_identity{
		     instance = Instance,
		     imsi = M_imsi}) ->
    encode_v2_element(1, Instance, <<(encode_tbcd(M_imsi))/binary>>);

encode_v2_element(#v2_cause{
		     instance = Instance,
		     v2_cause = M_v2_cause,
		     pce = M_pce,
		     bce = M_bce,
		     cs = M_cs}) ->
    encode_v2_element(2, Instance, <<(enum_v2_v2_cause(M_v2_cause)):8/integer,
				     0:5,
				     M_pce:1,
				     M_bce:1,
				     M_cs:1>>);

encode_v2_element(#v2_recovery{
		     instance = Instance,
		     restart_counter = M_restart_counter}) ->
    encode_v2_element(3, Instance, <<M_restart_counter:8>>);

encode_v2_element(#v2_stn_sr{
		     instance = Instance}) ->
    encode_v2_element(51, Instance, <<>>);

encode_v2_element(#v2_access_point_name{
		     instance = Instance,
		     apn = M_apn}) ->
    encode_v2_element(71, Instance, <<(encode_fqdn(M_apn))/binary>>);

encode_v2_element(#v2_aggregate_maximum_bit_rate{
		     instance = Instance,
		     uplink = M_uplink,
		     downlink = M_downlink}) ->
    encode_v2_element(72, Instance, <<M_uplink:32,
				      M_downlink:32>>);

encode_v2_element(#v2_eps_bearer_id{
		     instance = Instance,
		     eps_bearer_id = M_eps_bearer_id}) ->
    encode_v2_element(73, Instance, <<0:4,
				      M_eps_bearer_id:4>>);

encode_v2_element(#v2_ip_address{
		     instance = Instance,
		     ip = M_ip}) ->
    encode_v2_element(74, Instance, <<M_ip/binary>>);

encode_v2_element(#v2_mobile_equipment_identity{
		     instance = Instance,
		     mei = M_mei}) ->
    encode_v2_element(75, Instance, <<M_mei/binary>>);

encode_v2_element(#v2_msisdn{
		     instance = Instance,
		     msisdn = M_msisdn}) ->
    encode_v2_element(76, Instance, <<(encode_tbcd(M_msisdn))/binary>>);

encode_v2_element(#v2_indication{
		     instance = Instance,
		     flags = M_flags}) ->
    encode_v2_element(77, Instance, <<(encode_v2_indication_flags(M_flags))/binary>>);

encode_v2_element(#v2_protocol_configuration_options{
		     instance = Instance,
		     config = M_config}) ->
    encode_v2_element(78, Instance, <<(encode_protocol_config_opts(M_config))/binary>>);

encode_v2_element(#v2_pdn_address_allocation{
		     instance = Instance,
		     type = M_type,
		     address = M_address}) ->
    encode_v2_element(79, Instance, <<0:5,
				      (enum_v2_type(M_type)):3/integer,
				      M_address/binary>>);

encode_v2_element(#v2_bearer_level_quality_of_service{
		     instance = Instance,
		     pci = M_pci,
		     pl = M_pl,
		     pvi = M_pvi,
		     label = M_label,
		     maximum_bit_rate_for_uplink = M_maximum_bit_rate_for_uplink,
		     maximum_bit_rate_for_downlink = M_maximum_bit_rate_for_downlink,
		     guaranteed_bit_rate_for_uplink = M_guaranteed_bit_rate_for_uplink,
		     guaranteed_bit_rate_for_downlink = M_guaranteed_bit_rate_for_downlink}) ->
    encode_v2_element(80, Instance, <<0:1,
				      M_pci:1,
				      M_pl:4,
				      0:1,
				      M_pvi:1,
				      M_label:8,
				      M_maximum_bit_rate_for_uplink:40,
				      M_maximum_bit_rate_for_downlink:40,
				      M_guaranteed_bit_rate_for_uplink:40,
				      M_guaranteed_bit_rate_for_downlink:40>>);

encode_v2_element(#v2_flow_quality_of_service{
		     instance = Instance}) ->
    encode_v2_element(81, Instance, <<>>);

encode_v2_element(#v2_rat_type{
		     instance = Instance,
		     rat_type = M_rat_type}) ->
    encode_v2_element(82, Instance, <<M_rat_type:8>>);

encode_v2_element(#v2_serving_network{instance = Instance} = IE) ->
    encode_v2_element(83, Instance, encode_v2_mccmnc(IE));

encode_v2_element(#v2_eps_bearer_level_traffic_flow_template{
		     instance = Instance}) ->
    encode_v2_element(84, Instance, <<>>);

encode_v2_element(#v2_traffic_aggregation_description{
		     instance = Instance}) ->
    encode_v2_element(85, Instance, <<>>);

encode_v2_element(#v2_user_location_information{instance = Instance} = IE) ->
    encode_v2_element(86, Instance, encode_v2_user_location_information(IE));

encode_v2_element(#v2_fully_qualified_tunnel_endpoint_identifier{instance = Instance} = IE) ->
    encode_v2_element(87, Instance, encode_v2_fully_qualified_tunnel_endpoint_identifier(IE));

encode_v2_element(#v2_tmsi{
		     instance = Instance}) ->
    encode_v2_element(88, Instance, <<>>);

encode_v2_element(#v2_global_cn_id{
		     instance = Instance}) ->
    encode_v2_element(89, Instance, <<>>);

encode_v2_element(#v2_s103_pdn_data_forwarding_info{
		     instance = Instance}) ->
    encode_v2_element(90, Instance, <<>>);

encode_v2_element(#v2_s1_u_data_forwarding_info{
		     instance = Instance}) ->
    encode_v2_element(91, Instance, <<>>);

encode_v2_element(#v2_delay_value{
		     instance = Instance}) ->
    encode_v2_element(92, Instance, <<>>);

encode_v2_element(#v2_bearer_context{
		     instance = Instance,
		     group = M_group}) ->
    encode_v2_element(93, Instance, <<(encode_v2_grouped(M_group))/binary>>);

encode_v2_element(#v2_charging_id{
		     instance = Instance,
		     id = M_id}) ->
    encode_v2_element(94, Instance, <<M_id:4/bytes>>);

encode_v2_element(#v2_charging_characteristics{
		     instance = Instance}) ->
    encode_v2_element(95, Instance, <<>>);

encode_v2_element(#v2_trace_information{
		     instance = Instance}) ->
    encode_v2_element(96, Instance, <<>>);

encode_v2_element(#v2_bearer_flags{
		     instance = Instance}) ->
    encode_v2_element(97, Instance, <<>>);

encode_v2_element(#v2_pdn_type{
		     instance = Instance,
		     pdn_type = M_pdn_type}) ->
    encode_v2_element(99, Instance, <<0:4,
				      (enum_v2_pdn_type(M_pdn_type)):4/integer>>);

encode_v2_element(#v2_procedure_transaction_id{
		     instance = Instance}) ->
    encode_v2_element(100, Instance, <<>>);

encode_v2_element(#v2_mm_context_1{
		     instance = Instance}) ->
    encode_v2_element(103, Instance, <<>>);

encode_v2_element(#v2_mm_context_2{
		     instance = Instance}) ->
    encode_v2_element(104, Instance, <<>>);

encode_v2_element(#v2_mm_context_3{
		     instance = Instance}) ->
    encode_v2_element(105, Instance, <<>>);

encode_v2_element(#v2_mm_context_4{
		     instance = Instance}) ->
    encode_v2_element(106, Instance, <<>>);

encode_v2_element(#v2_mm_context_5{
		     instance = Instance}) ->
    encode_v2_element(107, Instance, <<>>);

encode_v2_element(#v2_mm_context_6{
		     instance = Instance}) ->
    encode_v2_element(108, Instance, <<>>);

encode_v2_element(#v2_pdn_connection{
		     instance = Instance}) ->
    encode_v2_element(109, Instance, <<>>);

encode_v2_element(#v2_pdu_numbers{
		     instance = Instance}) ->
    encode_v2_element(110, Instance, <<>>);

encode_v2_element(#v2_p_tmsi{
		     instance = Instance}) ->
    encode_v2_element(111, Instance, <<>>);

encode_v2_element(#v2_p_tmsi_signature{
		     instance = Instance}) ->
    encode_v2_element(112, Instance, <<>>);

encode_v2_element(#v2_hop_counter{
		     instance = Instance}) ->
    encode_v2_element(113, Instance, <<>>);

encode_v2_element(#v2_ue_time_zone{
		     instance = Instance,
		     timezone = M_timezone,
		     dst = M_dst}) ->
    encode_v2_element(114, Instance, <<M_timezone:8,
				       0:6,
				       M_dst:2>>);

encode_v2_element(#v2_trace_reference{
		     instance = Instance}) ->
    encode_v2_element(115, Instance, <<>>);

encode_v2_element(#v2_complete_request_message{
		     instance = Instance}) ->
    encode_v2_element(116, Instance, <<>>);

encode_v2_element(#v2_guti{
		     instance = Instance}) ->
    encode_v2_element(117, Instance, <<>>);

encode_v2_element(#v2_f_container{
		     instance = Instance}) ->
    encode_v2_element(118, Instance, <<>>);

encode_v2_element(#v2_f_cause{
		     instance = Instance}) ->
    encode_v2_element(119, Instance, <<>>);

encode_v2_element(#v2_plmn_id{
		     instance = Instance}) ->
    encode_v2_element(120, Instance, <<>>);

encode_v2_element(#v2_target_identification{
		     instance = Instance}) ->
    encode_v2_element(121, Instance, <<>>);

encode_v2_element(#v2_packet_flow_id_{
		     instance = Instance}) ->
    encode_v2_element(123, Instance, <<>>);

encode_v2_element(#v2_rab_context_{
		     instance = Instance}) ->
    encode_v2_element(124, Instance, <<>>);

encode_v2_element(#v2_source_rnc_pdcp_context_info{
		     instance = Instance}) ->
    encode_v2_element(125, Instance, <<>>);

encode_v2_element(#v2_udp_source_port_number{
		     instance = Instance}) ->
    encode_v2_element(126, Instance, <<>>);

encode_v2_element(#v2_apn_restriction{
		     instance = Instance,
		     restriction_type_value = M_restriction_type_value}) ->
    encode_v2_element(127, Instance, <<M_restriction_type_value:8>>);

encode_v2_element(#v2_selection_mode{
		     instance = Instance,
		     mode = M_mode}) ->
    encode_v2_element(128, Instance, <<0:6,
				       M_mode:2>>);

encode_v2_element(#v2_source_identification{
		     instance = Instance}) ->
    encode_v2_element(129, Instance, <<>>);

encode_v2_element(#v2_change_reporting_action{
		     instance = Instance}) ->
    encode_v2_element(131, Instance, <<>>);

encode_v2_element(#v2_fully_qualified_pdn_connection_set_identifier{
		     instance = Instance}) ->
    encode_v2_element(132, Instance, <<>>);

encode_v2_element(#v2_channel_needed{
		     instance = Instance}) ->
    encode_v2_element(133, Instance, <<>>);

encode_v2_element(#v2_emlpp_priority{
		     instance = Instance}) ->
    encode_v2_element(134, Instance, <<>>);

encode_v2_element(#v2_node_type{
		     instance = Instance}) ->
    encode_v2_element(135, Instance, <<>>);

encode_v2_element(#v2_fully_qualified_domain_name{
		     instance = Instance,
		     fqdn = M_fqdn}) ->
    encode_v2_element(136, Instance, <<(encode_fqdn(M_fqdn))/binary>>);

encode_v2_element(#v2_transaction_identifier{
		     instance = Instance}) ->
    encode_v2_element(137, Instance, <<>>);

encode_v2_element(#v2_mbms_session_duration{
		     instance = Instance}) ->
    encode_v2_element(138, Instance, <<>>);

encode_v2_element(#v2_mbms_service_area{
		     instance = Instance}) ->
    encode_v2_element(139, Instance, <<>>);

encode_v2_element(#v2_mbms_session_identifier{
		     instance = Instance}) ->
    encode_v2_element(140, Instance, <<>>);

encode_v2_element(#v2_mbms_flow_identifier{
		     instance = Instance}) ->
    encode_v2_element(141, Instance, <<>>);

encode_v2_element(#v2_mbms_ip_multicast_distribution{
		     instance = Instance}) ->
    encode_v2_element(142, Instance, <<>>);

encode_v2_element(#v2_mbms_distribution_acknowledge{
		     instance = Instance}) ->
    encode_v2_element(143, Instance, <<>>);

encode_v2_element(#v2_rfsp_index{
		     instance = Instance}) ->
    encode_v2_element(144, Instance, <<>>);

encode_v2_element(#v2_user_csg_information{
		     instance = Instance}) ->
    encode_v2_element(145, Instance, <<>>);

encode_v2_element(#v2_csg_information_reporting_action{
		     instance = Instance}) ->
    encode_v2_element(146, Instance, <<>>);

encode_v2_element(#v2_csg_id{
		     instance = Instance}) ->
    encode_v2_element(147, Instance, <<>>);

encode_v2_element(#v2_csg_membership_indication{
		     instance = Instance}) ->
    encode_v2_element(148, Instance, <<>>);

encode_v2_element(#v2_service_indicator{
		     instance = Instance}) ->
    encode_v2_element(149, Instance, <<>>);

encode_v2_element(#v2_detach_type{
		     instance = Instance}) ->
    encode_v2_element(150, Instance, <<>>);

encode_v2_element(#v2_local_distiguished_name{
		     instance = Instance}) ->
    encode_v2_element(151, Instance, <<>>);

encode_v2_element(#v2_node_features{
		     instance = Instance}) ->
    encode_v2_element(152, Instance, <<>>);

encode_v2_element(#v2_mbms_time_to_data_transfer{
		     instance = Instance}) ->
    encode_v2_element(153, Instance, <<>>);

encode_v2_element(#v2_throttling{
		     instance = Instance}) ->
    encode_v2_element(154, Instance, <<>>);

encode_v2_element(#v2_allocation_retention_priority{
		     instance = Instance}) ->
    encode_v2_element(155, Instance, <<>>);

encode_v2_element(#v2_epc_timer{
		     instance = Instance}) ->
    encode_v2_element(156, Instance, <<>>);

encode_v2_element(#v2_signalling_priority_indication{
		     instance = Instance}) ->
    encode_v2_element(157, Instance, <<>>);

encode_v2_element(#v2_temporary_mobile_group_identity{
		     instance = Instance}) ->
    encode_v2_element(158, Instance, <<>>);

encode_v2_element(#v2_additional_mm_context_for_srvcc{
		     instance = Instance}) ->
    encode_v2_element(159, Instance, <<>>);

encode_v2_element(#v2_additional_flags_for_srvcc{
		     instance = Instance}) ->
    encode_v2_element(160, Instance, <<>>);

encode_v2_element(#v2_mdt_configuration{
		     instance = Instance}) ->
    encode_v2_element(162, Instance, <<>>);

encode_v2_element(#v2_additional_protocol_configuration_options{
		     instance = Instance}) ->
    encode_v2_element(163, Instance, <<>>);

encode_v2_element(#v2_absolute_time_of_mbms_data_transfer{
		     instance = Instance}) ->
    encode_v2_element(164, Instance, <<>>);

encode_v2_element(#v2_henb_information_reporting_{
		     instance = Instance}) ->
    encode_v2_element(165, Instance, <<>>);

encode_v2_element(#v2_ipv4_configuration_parameters{
		     instance = Instance}) ->
    encode_v2_element(166, Instance, <<>>);

encode_v2_element(#v2_change_to_report_flags_{
		     instance = Instance}) ->
    encode_v2_element(167, Instance, <<>>);

encode_v2_element(#v2_action_indication{
		     instance = Instance}) ->
    encode_v2_element(168, Instance, <<>>);

encode_v2_element(#v2_twan_identifier{
		     instance = Instance}) ->
    encode_v2_element(169, Instance, <<>>);

encode_v2_element(#v2_uli_timestamp{
		     instance = Instance}) ->
    encode_v2_element(170, Instance, <<>>);

encode_v2_element(#v2_mbms_flags{
		     instance = Instance}) ->
    encode_v2_element(171, Instance, <<>>);

encode_v2_element(#v2_ran_nas_cause{
		     instance = Instance}) ->
    encode_v2_element(172, Instance, <<>>);

encode_v2_element(#v2_cn_operator_selection_entity{
		     instance = Instance}) ->
    encode_v2_element(173, Instance, <<>>);

encode_v2_element(#v2_trusted_wlan_mode_indication{
		     instance = Instance}) ->
    encode_v2_element(174, Instance, <<>>);

encode_v2_element(#v2_node_number{
		     instance = Instance}) ->
    encode_v2_element(175, Instance, <<>>);

encode_v2_element(#v2_node_identifier{
		     instance = Instance}) ->
    encode_v2_element(176, Instance, <<>>);

encode_v2_element(#v2_presence_reporting_area_action{
		     instance = Instance}) ->
    encode_v2_element(177, Instance, <<>>);

encode_v2_element(#v2_presence_reporting_area_information{
		     instance = Instance}) ->
    encode_v2_element(178, Instance, <<>>);

encode_v2_element(#v2_twan_identifier_timestamp{
		     instance = Instance}) ->
    encode_v2_element(179, Instance, <<>>);

encode_v2_element(#v2_overload_control_information{
		     instance = Instance}) ->
    encode_v2_element(180, Instance, <<>>);

encode_v2_element(#v2_load_control_information{
		     instance = Instance}) ->
    encode_v2_element(181, Instance, <<>>);

encode_v2_element(#v2_metric{
		     instance = Instance}) ->
    encode_v2_element(182, Instance, <<>>);

encode_v2_element(#v2_sequence_number{
		     instance = Instance}) ->
    encode_v2_element(183, Instance, <<>>);

encode_v2_element(#v2_apn_and_relative_capacity{
		     instance = Instance}) ->
    encode_v2_element(184, Instance, <<>>);

encode_v2_element(#v2_wlan_offloadability_indication{
		     instance = Instance}) ->
    encode_v2_element(185, Instance, <<>>);

encode_v2_element(#v2_private_extension{
		     instance = Instance}) ->
    encode_v2_element(255, Instance, <<>>);

encode_v2_element({Tag, Instance, Value}) when is_integer(Tag), is_integer(Instance), is_binary(Value) ->
    encode_v2_element(Tag, Instance, Value).

?PRETTY_PRINT(pretty_print_v2, v2_international_mobile_subscriber_identity);
?PRETTY_PRINT(pretty_print_v2, v2_cause);
?PRETTY_PRINT(pretty_print_v2, v2_recovery);
?PRETTY_PRINT(pretty_print_v2, v2_stn_sr);
?PRETTY_PRINT(pretty_print_v2, v2_access_point_name);
?PRETTY_PRINT(pretty_print_v2, v2_aggregate_maximum_bit_rate);
?PRETTY_PRINT(pretty_print_v2, v2_eps_bearer_id);
?PRETTY_PRINT(pretty_print_v2, v2_ip_address);
?PRETTY_PRINT(pretty_print_v2, v2_mobile_equipment_identity);
?PRETTY_PRINT(pretty_print_v2, v2_msisdn);
?PRETTY_PRINT(pretty_print_v2, v2_indication);
?PRETTY_PRINT(pretty_print_v2, v2_protocol_configuration_options);
?PRETTY_PRINT(pretty_print_v2, v2_pdn_address_allocation);
?PRETTY_PRINT(pretty_print_v2, v2_bearer_level_quality_of_service);
?PRETTY_PRINT(pretty_print_v2, v2_flow_quality_of_service);
?PRETTY_PRINT(pretty_print_v2, v2_rat_type);
?PRETTY_PRINT(pretty_print_v2, v2_serving_network);
?PRETTY_PRINT(pretty_print_v2, v2_eps_bearer_level_traffic_flow_template);
?PRETTY_PRINT(pretty_print_v2, v2_traffic_aggregation_description);
?PRETTY_PRINT(pretty_print_v2, v2_user_location_information);
?PRETTY_PRINT(pretty_print_v2, v2_fully_qualified_tunnel_endpoint_identifier);
?PRETTY_PRINT(pretty_print_v2, v2_tmsi);
?PRETTY_PRINT(pretty_print_v2, v2_global_cn_id);
?PRETTY_PRINT(pretty_print_v2, v2_s103_pdn_data_forwarding_info);
?PRETTY_PRINT(pretty_print_v2, v2_s1_u_data_forwarding_info);
?PRETTY_PRINT(pretty_print_v2, v2_delay_value);
?PRETTY_PRINT(pretty_print_v2, v2_bearer_context);
?PRETTY_PRINT(pretty_print_v2, v2_charging_id);
?PRETTY_PRINT(pretty_print_v2, v2_charging_characteristics);
?PRETTY_PRINT(pretty_print_v2, v2_trace_information);
?PRETTY_PRINT(pretty_print_v2, v2_bearer_flags);
?PRETTY_PRINT(pretty_print_v2, v2_pdn_type);
?PRETTY_PRINT(pretty_print_v2, v2_procedure_transaction_id);
?PRETTY_PRINT(pretty_print_v2, v2_mm_context_1);
?PRETTY_PRINT(pretty_print_v2, v2_mm_context_2);
?PRETTY_PRINT(pretty_print_v2, v2_mm_context_3);
?PRETTY_PRINT(pretty_print_v2, v2_mm_context_4);
?PRETTY_PRINT(pretty_print_v2, v2_mm_context_5);
?PRETTY_PRINT(pretty_print_v2, v2_mm_context_6);
?PRETTY_PRINT(pretty_print_v2, v2_pdn_connection);
?PRETTY_PRINT(pretty_print_v2, v2_pdu_numbers);
?PRETTY_PRINT(pretty_print_v2, v2_p_tmsi);
?PRETTY_PRINT(pretty_print_v2, v2_p_tmsi_signature);
?PRETTY_PRINT(pretty_print_v2, v2_hop_counter);
?PRETTY_PRINT(pretty_print_v2, v2_ue_time_zone);
?PRETTY_PRINT(pretty_print_v2, v2_trace_reference);
?PRETTY_PRINT(pretty_print_v2, v2_complete_request_message);
?PRETTY_PRINT(pretty_print_v2, v2_guti);
?PRETTY_PRINT(pretty_print_v2, v2_f_container);
?PRETTY_PRINT(pretty_print_v2, v2_f_cause);
?PRETTY_PRINT(pretty_print_v2, v2_plmn_id);
?PRETTY_PRINT(pretty_print_v2, v2_target_identification);
?PRETTY_PRINT(pretty_print_v2, v2_packet_flow_id_);
?PRETTY_PRINT(pretty_print_v2, v2_rab_context_);
?PRETTY_PRINT(pretty_print_v2, v2_source_rnc_pdcp_context_info);
?PRETTY_PRINT(pretty_print_v2, v2_udp_source_port_number);
?PRETTY_PRINT(pretty_print_v2, v2_apn_restriction);
?PRETTY_PRINT(pretty_print_v2, v2_selection_mode);
?PRETTY_PRINT(pretty_print_v2, v2_source_identification);
?PRETTY_PRINT(pretty_print_v2, v2_change_reporting_action);
?PRETTY_PRINT(pretty_print_v2, v2_fully_qualified_pdn_connection_set_identifier);
?PRETTY_PRINT(pretty_print_v2, v2_channel_needed);
?PRETTY_PRINT(pretty_print_v2, v2_emlpp_priority);
?PRETTY_PRINT(pretty_print_v2, v2_node_type);
?PRETTY_PRINT(pretty_print_v2, v2_fully_qualified_domain_name);
?PRETTY_PRINT(pretty_print_v2, v2_transaction_identifier);
?PRETTY_PRINT(pretty_print_v2, v2_mbms_session_duration);
?PRETTY_PRINT(pretty_print_v2, v2_mbms_service_area);
?PRETTY_PRINT(pretty_print_v2, v2_mbms_session_identifier);
?PRETTY_PRINT(pretty_print_v2, v2_mbms_flow_identifier);
?PRETTY_PRINT(pretty_print_v2, v2_mbms_ip_multicast_distribution);
?PRETTY_PRINT(pretty_print_v2, v2_mbms_distribution_acknowledge);
?PRETTY_PRINT(pretty_print_v2, v2_rfsp_index);
?PRETTY_PRINT(pretty_print_v2, v2_user_csg_information);
?PRETTY_PRINT(pretty_print_v2, v2_csg_information_reporting_action);
?PRETTY_PRINT(pretty_print_v2, v2_csg_id);
?PRETTY_PRINT(pretty_print_v2, v2_csg_membership_indication);
?PRETTY_PRINT(pretty_print_v2, v2_service_indicator);
?PRETTY_PRINT(pretty_print_v2, v2_detach_type);
?PRETTY_PRINT(pretty_print_v2, v2_local_distiguished_name);
?PRETTY_PRINT(pretty_print_v2, v2_node_features);
?PRETTY_PRINT(pretty_print_v2, v2_mbms_time_to_data_transfer);
?PRETTY_PRINT(pretty_print_v2, v2_throttling);
?PRETTY_PRINT(pretty_print_v2, v2_allocation_retention_priority);
?PRETTY_PRINT(pretty_print_v2, v2_epc_timer);
?PRETTY_PRINT(pretty_print_v2, v2_signalling_priority_indication);
?PRETTY_PRINT(pretty_print_v2, v2_temporary_mobile_group_identity);
?PRETTY_PRINT(pretty_print_v2, v2_additional_mm_context_for_srvcc);
?PRETTY_PRINT(pretty_print_v2, v2_additional_flags_for_srvcc);
?PRETTY_PRINT(pretty_print_v2, v2_mdt_configuration);
?PRETTY_PRINT(pretty_print_v2, v2_additional_protocol_configuration_options);
?PRETTY_PRINT(pretty_print_v2, v2_absolute_time_of_mbms_data_transfer);
?PRETTY_PRINT(pretty_print_v2, v2_henb_information_reporting_);
?PRETTY_PRINT(pretty_print_v2, v2_ipv4_configuration_parameters);
?PRETTY_PRINT(pretty_print_v2, v2_change_to_report_flags_);
?PRETTY_PRINT(pretty_print_v2, v2_action_indication);
?PRETTY_PRINT(pretty_print_v2, v2_twan_identifier);
?PRETTY_PRINT(pretty_print_v2, v2_uli_timestamp);
?PRETTY_PRINT(pretty_print_v2, v2_mbms_flags);
?PRETTY_PRINT(pretty_print_v2, v2_ran_nas_cause);
?PRETTY_PRINT(pretty_print_v2, v2_cn_operator_selection_entity);
?PRETTY_PRINT(pretty_print_v2, v2_trusted_wlan_mode_indication);
?PRETTY_PRINT(pretty_print_v2, v2_node_number);
?PRETTY_PRINT(pretty_print_v2, v2_node_identifier);
?PRETTY_PRINT(pretty_print_v2, v2_presence_reporting_area_action);
?PRETTY_PRINT(pretty_print_v2, v2_presence_reporting_area_information);
?PRETTY_PRINT(pretty_print_v2, v2_twan_identifier_timestamp);
?PRETTY_PRINT(pretty_print_v2, v2_overload_control_information);
?PRETTY_PRINT(pretty_print_v2, v2_load_control_information);
?PRETTY_PRINT(pretty_print_v2, v2_metric);
?PRETTY_PRINT(pretty_print_v2, v2_sequence_number);
?PRETTY_PRINT(pretty_print_v2, v2_apn_and_relative_capacity);
?PRETTY_PRINT(pretty_print_v2, v2_wlan_offloadability_indication);
?PRETTY_PRINT(pretty_print_v2, v2_private_extension);
pretty_print_v2(_, _) ->
    no.
