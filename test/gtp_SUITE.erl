%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% Copyright 2015, Travelping GmbH <info@travelping.com>

%%%-------------------------------------------------------------------
%%% @author Andreas Schultz <aschultz@tpip.net>
%%% @copyright (C) 2011, Andreas Schultz
%%% @doc
%%%
%%% @end
%%% Created : 29 Jun 2011 by Andreas Schultz <aschultz@tpip.net>
%%%-------------------------------------------------------------------
-module(gtp_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("../include/gtp_packet.hrl").

-define(equal(Expected, Actual),
    (fun (Expected@@@, Expected@@@) -> true;
         (Expected@@@, Actual@@@) ->
             ct:pal("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
                    [?FILE, ?LINE, ??Actual, Expected@@@, Actual@@@]),
             false
     end)(Expected, Actual) orelse error(badmatch)).

-define(match(Guard, Expr),
	((fun () ->
		  case (Expr) of
		      Guard -> ok;
		      V -> ct:pal("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
				   [?FILE, ?LINE, ??Expr, ??Guard, V]),
			    error(badmatch)
		  end
	  end)())).

%% hexstr2bin from otp/lib/crypto/test/crypto_SUITE.erl
hexstr2bin(S) ->
    list_to_binary(hexstr2list(S)).

hexstr2list([X,Y|T]) ->
    [mkint(X)*16 + mkint(Y) | hexstr2list(T)];
hexstr2list([]) ->
    [].
mkint(C) when $0 =< C, C =< $9 ->
    C - $0;
mkint(C) when $A =< C, C =< $F ->
    C - $A + 10;
mkint(C) when $a =< C, C =< $f ->
    C - $a + 10.

v1_echo_request() ->
    hexstr2bin("320100040000000044000000").

v1_echo_response() ->
    hexstr2bin("3202000600000000440000000e2b").

v1_create_pdp_context_request() ->
    hexstr2bin("3210006800000000440100000242000121436587f90e110ffd10000000011100"
	       "00000114001a0800800002f12183000908696e7465726e657484001580c02311"
	       "01010011036d69670868656d6d656c6967850004c0a80d35850004c0a80d3686"
	       "0007916407123254f6870004000b921f").

v1_create_pdp_context_request_2() ->
    hexstr2bin("321000a000000000846100000262000661492080f50372f410fffeff0ebf0ffd"
	       "1020008a781120008a781405800002f12183000c0b6b6e6f77726f616d696e67"
	       "84001a8080211001000010810600000000830600000000000d00000500850004"
	       "c7ff06bd850004c7ff06bd8600099184970000004579f987000f021b921f7396"
	       "fefe742b1040006a00970001019800080172f41001370bbe99000200009a0008"
	       "5368165092359710").

v1_create_pdp_context_response() ->
    hexstr2bin("3211004e0000000144010000018008000e2b100000000111000000017f000000"
	       "01800006f1210a1c010284001480802110020000108106080808088306000000"
	       "00850004ac1410a8850004ac1410a9870004000b921f").

v2_create_session_request() ->
    hexstr2bin("482000f300000000001019000100080032021308000002f44c00060034767654"
	       "30094b000800538582602940091056000d001862f210119962f210018ecb0053"
	       "00030062f2105200010006570009008601ab0f46c1fe8fd547001b0007646566"
	       "61756c74066d6e63303033066d63633233320467707273800001000063000100"
	       "014f00050001000000007f000100004800080000000010000000104e001d0080"
	       "80211001000010810600000000830600000000000d00000a000010005d002c00"
	       "4900010005570009028401ab0f46c1fe8fd5500016006d090000000000000000"
	       "0000000000000000000000000300010001720002008001").

v2_create_session_response() ->
    hexstr2bin("4821009301ab0f4600101900020002001000570009018701bc22dbd5a25bc34f"
	       "000500010a6e13337f000100004e001b0080802110030000108106d5a245aa83"
	       "06d5a24502000d04d5a245aa5d003a0049000100050200020010005700090285"
	       "01b822dbd5a25bc3500016006d09000000000000000000000000000000000000"
	       "00005e00040042bb0e3103000100024a0004000a7e9824").

v1_pco_rel97() ->
    hexstr2bin("3215000A0003A8F4696E0000018084000100").

v2_pco_vendor_ext() ->
    IEs =
	[#v2_access_point_name{
	    apn = [<<"example">>,<<"net">>]},
	 #v2_aggregate_maximum_bit_rate{
	    uplink = 48128,
	    downlink = 1704125},
	 #v2_apn_restriction{
	    restriction_type_value = 0},
	 #v2_bearer_context{
	    group = [#v2_bearer_level_quality_of_service{
			pci = 1,pl = 10,pvi = 0,label = 8,
			maximum_bit_rate_for_uplink = 0,
			maximum_bit_rate_for_downlink = 0,
			guaranteed_bit_rate_for_uplink = 0,
			guaranteed_bit_rate_for_downlink = 0},
		     #v2_eps_bearer_id{eps_bearer_id = 5},
		     #v2_fully_qualified_tunnel_endpoint_identifier{
			instance = 2,
			interface_type = 4,key = 5379562,ipv4 = <<"À¨É°">>,
			ipv6 = undefined}]},
	 #v2_fully_qualified_tunnel_endpoint_identifier{
	    interface_type = 6,key = 5379554,ipv4 = <<"À¨É°">>,
	    ipv6 = undefined},
	 #v2_indication{
	    flags = ['P','CRSI']},
	 #v2_international_mobile_subscriber_identity{
	    imsi = <<"111111111111111">>},
	 #v2_mobile_equipment_identity{
	    mei = <<1,0,2,3,4,4,6,7>>},
	 #v2_msisdn{msisdn = <<"001011111111111">>},
	 #v2_pdn_address_allocation{
	    type = ipv4, address = <<0,0,0,0>>},
	 #v2_pdn_type{pdn_type = ipv4},
	 #v2_protocol_configuration_options{
	    config = {0,
		      [{ipcp,'CP-Configure-Request',0,
			[{ms_dns1,<<0,0,0,0>>},{ms_dns2,<<0,0,0,0>>}]},
		       {13,<<>>},
		       {65280,<<19,1,132>>},
		       {12,<<>>},
		       {10,<<>>},
		       {16,<<>>}]}},
	 #v2_rat_type{rat_type = 6},
	 #v2_selection_mode{mode = 0},
	 #v2_serving_network{
	    mcc = <<"302">>, mnc = <<"610">>},
	 #v2_ue_time_zone{timezone = 138,dst = 0},
	 #v2_user_location_information{
	    cgi = undefined,
	    sai = undefined,
	    rai = undefined,
	    tai = <<3,2,22,43,238>>,
	    ecgi = <<3,2,34,1,187,116,1>>,
	    lai = undefined}],
    Msg = #gtp{version = v2,
	       type = create_session_request,
	       seq_no = 1,
	       tei = 0,
	       ie = IEs}.

g_pdu() ->
    hexstr2bin("30ff00540000000c45000054fd1640003f0113cc0ab41003080808080800b437"
	       "247b000153e61a5900000000e7390b0000000000101112131415161718191a1b"
	       "1c1d1e1f202122232425262728292a2b2c2d2e2f3031323334353637").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
	[{timetrap,{seconds,30}}].

do_test(Msg) ->
    P = gtp_packet:decode(Msg),
    ?match(#gtp{ie = IEs} when is_map(IEs), P),
    ct:pal("Decoded Msg: ~p", [P]),
    ?equal(Msg, gtp_packet:encode(P)).

do_test_v2(Msg) ->
    P = gtp_packet:decode(Msg),
    ?match(#gtp{ie = IEs} when is_map(IEs), P),
    ct:pal("Decoded Msg: ~p", [P]),
    ?equal(P, gtp_packet:decode(gtp_packet:encode(P))).

test_v1_echo_request(_Config) ->
    do_test(v1_echo_request()),
    ok.

test_v1_echo_response(_Config) ->
    do_test(v1_echo_response()),
    ok.

test_v1_create_pdp_context_request(_Config) ->
    do_test(v1_create_pdp_context_request()),
    do_test(v1_create_pdp_context_request_2()),
    ok.

test_v1_create_pdp_context_response(_Config) ->
    do_test(v1_create_pdp_context_response()),
    ok.

test_v2_create_session_request(_Config) ->
    do_test_v2(v2_create_session_request()),
    ok.

test_v2_create_session_response(_Config) ->
    do_test_v2(v2_create_session_response()),
    ok.

test_g_pdu(_Config) ->
    do_test(g_pdu()),
    ok.

test_v1_pco_rel97(_Config) ->
    do_test(v1_pco_rel97()),
    ok.

test_v2_pco_vendor_ext(_Config) ->
    Msg = v2_pco_vendor_ext(),
    ?match(Data when is_binary(Data), (catch gtp_packet:encode(Msg))),
    ok.

partial_decode(_Config) ->
    Msg0 = gtp_packet:decode(v1_create_pdp_context_request_2(), #{ies => binary}),
    ?match(#gtp{ie = IEs} when is_binary(IEs), Msg0),
    Msg1 = gtp_packet:decode_ies(Msg0),
    ?match(#gtp{ie = IEs} when is_map(IEs), Msg1),
    Msg2 = gtp_packet:decode_ies(Msg0, #{ies => map}),
    ?match(#gtp{ie = IEs} when is_map(IEs), Msg2),
    Msg3 = gtp_packet:decode_ies(Msg0, #{ies => binary}),
    ?match(#gtp{ie = IEs} when is_binary(IEs), Msg3),
    Msg4 = (catch gtp_packet:decode_ies(Msg1)),
    ?match(#gtp{ie = IEs} when is_map(IEs), Msg4),
    Msg5 = (catch gtp_packet:decode_ies(Msg1, #{ies => binary})),
    ?match({'EXIT', {badargs,_}}, Msg5),

    Msg10 = gtp_packet:decode(v2_create_session_request(), #{ies => binary}),
    ?match(#gtp{ie = IEs} when is_binary(IEs), Msg10),
    Msg11 = gtp_packet:decode_ies(Msg10),
    ?match(#gtp{ie = IEs} when is_map(IEs), Msg11),
    Msg12 = gtp_packet:decode_ies(Msg10, #{ies => map}),
    ?match(#gtp{ie = IEs} when is_map(IEs), Msg12),
    Msg13 = gtp_packet:decode_ies(Msg10, #{ies => binary}),
    ?match(#gtp{ie = IEs} when is_binary(IEs), Msg13),
    Msg14 = (catch gtp_packet:decode_ies(Msg11)),
    ?match(#gtp{ie = IEs} when is_map(IEs), Msg14),
    Msg15 = (catch gtp_packet:decode_ies(Msg11, #{ies => binary})),
    ?match({'EXIT', {badargs,_}}, Msg15),

    ok.

partial_encode(_Config) ->
    Msg0 = gtp_packet:decode(v1_create_pdp_context_request_2()),
    ?match(#gtp{ie = IEs} when is_map(IEs), Msg0),
    Msg1 = gtp_packet:encode_ies(Msg0),
    ?match(#gtp{ie = IEs} when is_binary(IEs), Msg1),
    Msg2 = gtp_packet:encode(Msg1),
    ?match(_ when is_binary(Msg2), Msg2),

    Msg10 = gtp_packet:decode(v2_create_session_request()),
    ?match(#gtp{ie = IEs} when is_map(IEs), Msg10),
    Msg11 = gtp_packet:encode_ies(Msg10),
    ?match(#gtp{ie = IEs} when is_binary(IEs), Msg11),
    Msg12 = gtp_packet:encode(Msg11),
    ?match(_ when is_binary(Msg12), Msg12),
    ok.

all() ->
	[test_v1_echo_request,
	 test_v1_echo_response,
	 test_v1_create_pdp_context_request,
	 test_v1_create_pdp_context_response,
	 test_v2_create_session_request,
	 test_v2_create_session_response,
	 test_g_pdu,
	 test_v1_pco_rel97,
	 test_v2_pco_vendor_ext,
	 partial_decode,
	 partial_encode].
