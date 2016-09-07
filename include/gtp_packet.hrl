%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% Copyright 2015, Travelping GmbH <info@travelping.com>

-define('3GPP_RAT-TYPE_RESERVED',	0).
-define('3GPP_RAT-TYPE_UTRAN',		1).
-define('3GPP_RAT-TYPE_GERAN',		2).
-define('3GPP_RAT-TYPE_WLAN',		3).
-define('3GPP_RAT-TYPE_GAN',		4).
-define('3GPP_RAT-TYPE_HSPA EVOLUTION',	5).
-define('3GPP_RAT-TYPE_EUTRAN',		6).

-record(gtp, {
	  version	:: 'undefined' | 'v1' | 'v2',
	  type,
	  tei		:: 0..16#ffffffff,
	  seq_no	:: 0..16#ffff,
	  n_pdu		:: 0..16#ff,
	  ext_hdr = []	:: [term()],
	  ie		:: [term()]
	 }).

-include("gtp_packet_v1_gen.hrl").
-include("gtp_packet_v2_gen.hrl").

-record(routeing_area_identity, {
	  instance = 0,
	  mcc,
	  mnc,
	  lac = 0,
	  rac = 0
	 }).

-record(user_location_information, {
	  instance = 0,
	  type,
	  mcc,
	  mnc,
	  lac = 0,
	  ci = 0,
	  sac = 0,
	  rac = 0
	 }).

-record(v2_user_location_information, {
	  instance = 0,
	  cgi,
	  sai,
	  rai,
	  tai,
	  ecgi,
	  lai,
	  data
	}).

-record(v2_fully_qualified_tunnel_endpoint_identifier, {
	  instance = 0,
	  interface_type,
	  key,
	  ipv4,
	  ipv6,
	  data
	 }).

-record(v2_serving_network, {
	  instance = 0,
	  mcc,
	  mnc
	 }).
