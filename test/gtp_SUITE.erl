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

-define(equal(Expected, Actual),
    (fun (Expected@@@, Expected@@@) -> true;
         (Expected@@@, Actual@@@) ->
             ct:pal("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
                    [?FILE, ?LINE, ??Actual, Expected@@@, Actual@@@]),
             false
     end)(Expected, Actual) orelse error(badmatch)).

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
    hexstr2bin("3210006700000000440100000242000121436587f90e110f0110000000011100"
	       "00000114001a0800800002f121830008696e7465726e657484001580c0231101"
	       "010011036d69670868656d6d656c6967850004c0a80d35850004c0a80d368600"
	       "07916407123254f6870004000b921f").

v1_create_pdp_context_response() ->
    hexstr2bin("3211004e0000000144010000018008000e2b100000000111000000017f000000"
	       "01800006f1210a1c010284001480802110020000108106080808088306000000"
	       "00850004ac1410a8850004ac1410a9870004000b921f").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
	[{timetrap,{seconds,30}}].

do_test(Msg) ->
    P = gtp_packet:decode(Msg),
    ?equal(Msg, gtp_packet:encode(P)).

test_v1_echo_request(_Config) ->
    do_test(v1_echo_request()),
    ok.

test_v1_echo_response(_Config) ->
    do_test(v1_echo_response()),
    ok.

test_v1_create_pdp_context_request(_Config) ->
    do_test(v1_create_pdp_context_request()),
    ok.

test_v1_create_pdp_context_response(_Config) ->
    do_test(v1_create_pdp_context_response()),
    ok.

all() ->
	[test_v1_echo_request,
	 test_v1_echo_response,
	 test_v1_create_pdp_context_request,
	 test_v1_create_pdp_context_response].
