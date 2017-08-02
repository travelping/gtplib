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

v1_invalid_msg() ->
    hexstr2bin("320000040000000044000000").

v2_invalid_msg() ->
    hexstr2bin("480000080000000000000100").

test_messages() ->
    [{{v1, echo_request},
      hexstr2bin("320100040000000044000000")},
     {{v1, echo_response},
      hexstr2bin("3202000600000000440000000e2b")},
     {{v1, create_pdp_context_request},
      hexstr2bin("3210006300000000440100000242001111111111f10e110ffd10000000011100"
		 "00000114001a0800800002f12183000908696e7465726e657484001080c0230c"
		 "0101000c03504150035041508500047f0000018500047f000001860007916411"
		 "111111f1870004000b921f")},
     {{v1, create_pdp_context_request_2},
      hexstr2bin("321000a000000000846100000262111111111111f10372f410fffeff0ebf0ffd"
		 "1020008a781120008a781405800002f12183000c076578616d706c65036e6574"
		 "84001a8080211001000010810600000000830600000000000d00000500850004"
		 "7f0000018500047f0000018600099184111111111111f187000f021b921f7396"
		 "fefe742b1040006a00970001019800080172f41001370bbe99000200009a0008"
		 "5311111111111111")},
     {{v1, create_pdp_context_request_3},
      hexstr2bin("321000a9000000008efb00000224111111111111f103030216fffeff0ffc1020"
		 "03b106112003b1061405800002f12183000908696e7465726e657484002680c0"
		 "230901010009035041500080211001010010810600000000830600000000000d"
		 "000010008500047f0000018500047f00000186000991abdc1e11111111f18700"
		 "0d0123911f7396fefe9305ffff009400012097000101980008010302162906a7"
		 "1799000200009a00085311111111111111")},
     {{v1, create_pdp_context_response},
      hexstr2bin("3211004e0000000144010000018008fe0e2b100000000111000000017f000000"
		 "01800006f1210a1c010284001480802110020000108106080808088306000000"
		 "00850004ac1410a8850004ac1410a9870004000b921f")},
     {{v1, pco_rel97},
      hexstr2bin("3215000A0003A8F4696E0000018084000100")},
     {{v2, echo_request},
      hexstr2bin("40010009006a50000300010073")},

     {{v2, create_session_request},
      hexstr2bin("482000f200000000001019000100080032111111111111f1030001000147001a"
		 "000764656661756c74066d6e63303031056d6363303104677072734800080000"
		 "000010000000104b00080053858260294009104c0006003411111111114e001d"
		 "008080211001000010810600000000830600000000000d00000a000010004f00"
		 "0500010000000052000100065300030062f21056000d001862f210119962f210"
		 "018ecb00570009008601ab0f467f0000015d002c004900010005500016006d09"
		 "0000000000000000000000000000000000000000570009028401ab0f467f0000"
		 "0163000100017200020080017f000100008000010000")},
     {{v2, create_session_request_2},
      hexstr2bin("4820012000000000014c52000100080013111111111111f147000c0007657861"
		 "6d706c65036e6574480008000000bc00001a00bd4b0008000100020304040607"
		 "4c00080078111111111111f14d00050000180000004e00530080c2231e010000"
		 "1e100a3131310a3131310a3131310a3131316368616c6c656e6765c223150200"
		 "0015104a1597e4c971b22f8dbc2a8948151a9c80211001000010810600000000"
		 "830600000000000d00000a004f000500010000000052000100065300030032f4"
		 "5156000d001832f451200932f40107a1211457000900860010ecaa7f0000015d"
		 "002c004900010005500016004807000000000000000000000000000000000000"
		 "000057000902840010ecb27f00000163000100017200020040017f0001000080"
		 "00010000")},
     {{v2, create_session_response},
      hexstr2bin("4821009301ab0f4600101900020002001000570009018701bc22dbd5a25bc34f"
		 "000500010a6e13337f000100004e001b0080802110030000108106d5a245aa83"
		 "06d5a24502000d04d5a245aa5d003a0049000100050200020010005700090285"
		 "01b822dbd5a25bc3500016006d09000000000000000000000000000000000000"
		 "00005e00040042bb0e3103000100024a0004000a7e9824")},
     {{v2, update_bearer_request},
      hexstr2bin("4861003f0010e58a014c53005d001f0049000100055000160048070000000000"
		 "000000000000000000000000000000480008000000bc0000094ed04d00040000"
		 "008000")}
    ].

v1_ignore_spare_bits() ->
    hexstr2bin("3211004e0000000144010000018008000e2b100000000111000000017f000000"
	       "01800006f1210a1c010284001480802110020000108106080808088306000000"
	       "00850004ac1410a8850004ac1410a9870004000b921f").

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
			interface_type = 4,key = 5379562,ipv4 = <<127,0,0,1>>,
			ipv6 = undefined}]},
	 #v2_fully_qualified_tunnel_endpoint_identifier{
	    interface_type = 6,key = 5379554,ipv4 = <<127,0,0,1>>,
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
    #gtp{version = v2,
	 type = create_session_request,
	 seq_no = 1,
	 tei = 0,
	 ie = IEs}.

g_pdu() ->
    hexstr2bin("30ff00540000000c45000054fd1640003f0113cc0ab41003080808080800b437"
	       "247b000153e61a5900000000e7390b0000000000101112131415161718191a1b"
	       "1c1d1e1f202122232425262728292a2b2c2d2e2f3031323334353637").

get_msg(Version, Name) ->
    proplists:get_value({Version, Name}, test_messages()).

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
	[{timetrap,{seconds,30}}].

msg_test({{Version, Name}, Msg}) ->
    ct:pal("Test version and message: ~s, ~s", [Version, Name]),
    P = gtp_packet:decode(Msg),
    ?match(#gtp{ie = IEs} when is_map(IEs), P),
    ct:pal("Decoded Msg: ~p", [P]),
    if Version =:= v1 ->
	    ?equal(Msg, gtp_packet:encode(P));
       Version =:= v2 ->
	    %% we don't rencode bit identical for v2
	    ?equal(P, gtp_packet:decode(gtp_packet:encode(P)))
    end.

test_v1_invalid_msg(_Config) ->
    ?match({'EXIT', {badarg, _}}, (catch gtp_packet:decode(v1_invalid_msg()))),
    ok.

encode_decode(_Config) ->
    [msg_test(Msg) || Msg <- test_messages()],
    ok.

test_v1_ignore_spare_bits() ->
    [{doc, "Check that decode ignores spare bit values"}].
test_v1_ignore_spare_bits(_Config) ->
    Msg = gtp_packet:decode(v1_ignore_spare_bits()),
    ?equal(Msg, gtp_packet:decode(gtp_packet:encode(Msg))).

test_v2_invalid_msg(_Config) ->
    ?match({'EXIT', {badarg, _}}, (catch gtp_packet:decode(v2_invalid_msg()))),
    ok.

test_g_pdu(_Config) ->
    BinMsg = g_pdu(),
    Msg0 = gtp_packet:decode(BinMsg, #{ies => binary}),
    ?match(#gtp{ie = IEs} when is_binary(IEs), Msg0),
    Msg1 = gtp_packet:decode(BinMsg, #{ies => map}),
    ?match(#gtp{ie = IEs} when is_binary(IEs), Msg1),

    ?equal(BinMsg, gtp_packet:encode(Msg1)),
    ok.

test_v2_pco_vendor_ext(_Config) ->
    Msg = v2_pco_vendor_ext(),
    ?match(Data when is_binary(Data), (catch gtp_packet:encode(Msg))),
    ok.

partial_decode(_Config) ->
    Msg0 = gtp_packet:decode(get_msg(v1, create_pdp_context_request_2), #{ies => binary}),
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

    Msg10 = gtp_packet:decode(get_msg(v2, create_session_request), #{ies => binary}),
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
    Msg0 = gtp_packet:decode(get_msg(v1, create_pdp_context_request_2)),
    ?match(#gtp{ie = IEs} when is_map(IEs), Msg0),
    Msg1 = gtp_packet:encode_ies(Msg0),
    ?match(#gtp{ie = IEs} when is_binary(IEs), Msg1),
    Msg2 = gtp_packet:encode(Msg1),
    ?match(_ when is_binary(Msg2), Msg2),

    Msg10 = gtp_packet:decode(get_msg(v2, create_session_request)),
    ?match(#gtp{ie = IEs} when is_map(IEs), Msg10),
    Msg11 = gtp_packet:encode_ies(Msg10),
    ?match(#gtp{ie = IEs} when is_binary(IEs), Msg11),
    Msg12 = gtp_packet:encode(Msg11),
    ?match(_ when is_binary(Msg12), Msg12),
    ok.

all() ->
	[test_v1_invalid_msg,
	 test_v2_invalid_msg,
	 encode_decode,
	 test_v1_ignore_spare_bits,
	 test_g_pdu,
	 test_v2_pco_vendor_ext,
	 partial_decode,
	 partial_encode].
