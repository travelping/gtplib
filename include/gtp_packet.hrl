-record(gtp_v1, {
	  type,
	  tei,
	  seq_no,
	  n_pdu,
	  ext_hdr,
	  ie}).

-record(gtp_v2, {
	  type,
	  tei,
	  seq_no,
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
