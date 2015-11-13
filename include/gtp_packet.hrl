%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% Copyright 2015, Travelping GmbH <info@travelping.com>

-record(gtp, {
	  version,
	  type,
	  tei,
	  seq_no,
	  n_pdu,
	  ext_hdr,
	  ie}).

-include("gtp_packet_v1_gen.hrl").
-include("gtp_packet_v2_gen.hrl").

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
