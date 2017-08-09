%% Copyright 2015, Travelping GmbH <info@travelping.com>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation, either version 3 of the
%% License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with this program. If not, see <http://www.gnu.org/licenses/>.

-define('3GPP_RAT-TYPE_RESERVED',	0).
-define('3GPP_RAT-TYPE_UTRAN',		1).
-define('3GPP_RAT-TYPE_GERAN',		2).
-define('3GPP_RAT-TYPE_WLAN',		3).
-define('3GPP_RAT-TYPE_GAN',		4).
-define('3GPP_RAT-TYPE_HSPA EVOLUTION',	5).
-define('3GPP_RAT-TYPE_EUTRAN',		6).

-define('PCO-P-CSCF-IPv6-Address',			16#01).
-define('PCO-IM-CN-Subsystem-Signaling-Flag',		16#02).
-define('PCO-DNS-Server-IPv6-Address',			16#03).
-define('PCO-Policy-Control-Rejection-Code',		16#04).
-define('PCO-Bearer-Control-Mode',			16#05).
-define('PCO-DSMIPv6-Home-Agent-Address',		16#07).
-define('PCO-DSMIPv6-Home-Network-Prefix',		16#08).
-define('PCO-DSMIPv6-IPv4-Home-Agent-Address',		16#09).
-define('PCO-IP-Address-Allocation-Via-NAS-Signalling',	16#0A).
-define('PCO-IPv4-Address-Allocation-Via-DHCPv4',	16#0B).
-define('PCO-P-CSCF-IPv4-Address',			16#0C).
-define('PCO-DNS-Server-IPv4-Address',			16#0D).
-define('PCO-MSISDN',					16#0E).
-define('PCO-IFOM-Support',				16#0F).
-define('PCO-IPv4-Link-MTU',				16#10).
-define('PCO-Local-Address-In-TFT-Indicator',		16#11).
-define('PCO-P-CSCF-Re-Selection-Support',		16#12).
-define('PCO-NBIFOM-Indicator',				16#13).
-define('PCO-NBIFOM-Mode',				16#14).
-define('PCO-Non-IP-Link-MTU',				16#15).
-define('PCO-APN-Rate-Control',				16#16).

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
	  lai
	}).

-record(v2_fully_qualified_tunnel_endpoint_identifier, {
	  instance = 0,
	  interface_type,
	  key,
	  ipv4,
	  ipv6
	 }).

-record(v2_serving_network, {
	  instance = 0,
	  mcc,
	  mnc
	 }).
