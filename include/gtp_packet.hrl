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

%% -include("gtp_packet_v1_gen.hrl").

-record(cause, {
	  instance = 0,
	  value = request_imsi
}).

-record(international_mobile_subscriber_identity, {
	  instance = 0,
	  imsi
}).


-record(temporary_logical_link_identity, {
	  instance = 0,
	  tlli = <<0,0,0,0>>
}).

-record(packet_tmsi, {
	  instance = 0,
	  p_tmsi = <<0,0,0,0>>
}).

-record(reordering_required, {
	  instance = 0,
	  required = no
}).

-record(authentication_triplet, {
	  instance = 0,
	  rand = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
	  sres = <<0,0,0,0>>,
	  kc = <<0,0,0,0,0,0,0,0>>
}).

-record(map_cause, {
	  instance = 0,
	  content = <<0>>
}).

-record(p_tmsi_signature, {
	  instance = 0,
	  content = <<0,0,0>>
}).

-record(ms_validated, {
	  instance = 0,
	  validated = no
}).

-record(recovery, {
	  instance = 0,
	  restart_counter = 0
}).

-record(selection_mode, {
	  instance = 0,
	  mode = 0
}).

-record(tunnel_endpoint_identifier_data_i, {
	  instance = 0,
	  tei = 0
}).

-record(tunnel_endpoint_identifier_control_plane, {
	  instance = 0,
	  tei = 0
}).

-record(tunnel_endpoint_identifier_data_ii, {
	  instance = 0,
	  nsapi = 0,
	  tei = 0
}).

-record(teardown_ind, {
	  instance = 0,
	  value = 0
}).

-record(nsapi, {
	  instance = 0,
	  nsapi = 0
}).

-record(ranap_cause, {
	  instance = 0,
	  content = <<0>>
}).

-record(rab_context, {
	  instance = 0,
	  content = <<0,0,0,0,0,0,0,0,0>>
}).

-record(radio_priority_sms, {
	  instance = 0,
	  content = <<0>>
}).

-record(radio_priority, {
	  instance = 0,
	  content = <<0>>
}).

-record(packet_flow_id, {
	  instance = 0,
	  content = <<0,0>>
}).

-record(charging_characteristics, {
	  instance = 0,
	  value = <<0,0>>
}).

-record(trace_reference, {
	  instance = 0,
	  content = <<0,0>>
}).

-record(trace_type, {
	  instance = 0,
	  content = <<0,0>>
}).

-record(ms_not_reachable_reason, {
	  instance = 0,
	  content = <<0>>
}).

-record(charging_id, {
	  instance = 0,
	  id = <<0,0,0,0>>
}).

-record(end_user_address, {
	  instance = 0,
	  pdp_type_organization = 0,
	  pdp_type_number = 0,
	  pdp_address = <<>>
}).

-record(mm_context_gsm, {
	  instance = 0,
	  cksn = 0,
	  no_of_vectors = 0,
	  used_cipher = 0,
	  kc = <<0,0,0,0,0,0,0,0>>,
	  tripple = [],
	  drx_parameter = <<0,0>>,
	  ms_network_capability_length = 0,
	  ms_network_capability = [],
	  container_length = 0,
	  container = []
}).

-record(mm_context_umts, {
	  instance = 0,
	  ksi = 0,
	  no_of_vectors = 0,
	  ck = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
	  ik = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
	  quintuplet_length = 0,
	  quintuplet = [],
	  drx_parameter = <<0,0>>,
	  ms_network_capability_length = 0,
	  ms_network_capability = [],
	  container_length = 0,
	  container = []
}).

-record(mm_context_gsm_and_umts, {
	  instance = 0,
	  cksn = 0,
	  no_of_vectors = 0,
	  used_cipher = 0,
	  kc = <<0,0,0,0,0,0,0,0>>,
	  quintuplet_length = 0,
	  quintuplet = [],
	  drx_parameter = <<0,0>>,
	  ms_network_capability_length = 0,
	  ms_network_capability = [],
	  container_length = 0,
	  container = []
}).

-record(mm_context_umts_and_used_cipher, {
	  instance = 0,
	  ksi = 0,
	  no_of_vectors = 0,
	  used_cipher = 0,
	  ck = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
	  ik = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
	  quintuplet_length = 0,
	  quintuplet = [],
	  drx_parameter = <<0,0>>,
	  ms_network_capability_length = 0,
	  ms_network_capability = [],
	  container_length = 0,
	  container = []
}).

-record(pdp_context, {
	  instance = 0
}).

-record(access_point_name, {
	  instance = 0,
	  apn
}).

-record(protocol_configuration_options, {
	  instance = 0,
	  config
}).

-record(gsn_address, {
	  instance = 0,
	  address = <<>>
}).

-record(ms_international_pstn_isdn_number, {
	  instance = 0,
	  msisdn
}).

-record(quality_of_service_profile, {
	  instance = 0,
	  priority = 0,
	  data = <<>>
}).

-record(authentication_quintuplet, {
	  instance = 0
}).

-record(traffic_flow_template, {
	  instance = 0
}).

-record(target_identification, {
	  instance = 0
}).

-record(utran_transparent_container, {
	  instance = 0
}).

-record(rab_setup_information, {
	  instance = 0
}).

-record(extension_header_type_list, {
	  instance = 0
}).

-record(trigger_id, {
	  instance = 0
}).

-record(omc_identity, {
	  instance = 0
}).

-record(ran_transparent_container, {
	  instance = 0
}).

-record(pdp_context_prioritization, {
	  instance = 0
}).

-record(additional_rab_setup_information, {
	  instance = 0
}).

-record(sgsn_number, {
	  instance = 0
}).

-record(common_flags, {
	  instance = 0,
	  flags = []
}).

-record(apn_restriction, {
	  instance = 0
}).

-record(radio_priority_lcs, {
	  instance = 0
}).

-record(rat_type, {
	  instance = 0,
	  rat_type = 0
}).


-record(ms_time_zone, {
	  instance = 0,
	  timezone = 0,
	  dst = 0
}).

-record(imei, {
	  instance = 0,
	  imei
}).

-record(camel_charging_information_container, {
	  instance = 0
}).

-record(mbms_ue_context, {
	  instance = 0
}).

-record(temporary_mobile_group_identity, {
	  instance = 0
}).

-record(rim_routing_address, {
	  instance = 0
}).

-record(mbms_protocol_configuration_options, {
	  instance = 0
}).

-record(mbms_service_area, {
	  instance = 0
}).

-record(source_rnc_pdcp_context_info, {
	  instance = 0
}).

-record(additional_trace_info, {
	  instance = 0
}).

-record(hop_counter, {
	  instance = 0
}).

-record(selected_plmn_id, {
	  instance = 0
}).

-record(mbms_session_identifier, {
	  instance = 0
}).

-record(mbms_2g_3g_indicator, {
	  instance = 0
}).

-record(enhanced_nsapi, {
	  instance = 0
}).

-record(mbms_session_duration, {
	  instance = 0
}).

-record(additional_mbms_trace_info, {
	  instance = 0
}).

-record(mbms_session_repetition_number, {
	  instance = 0
}).

-record(mbms_time_to_data_transfer, {
	  instance = 0
}).

-record(bss_container, {
	  instance = 0
}).

-record(cell_identification, {
	  instance = 0
}).

-record(pdu_numbers, {
	  instance = 0
}).

-record(bssgp_cause, {
	  instance = 0
}).

-record(required_mbms_bearer_capabilities, {
	  instance = 0
}).

-record(rim_routing_address_discriminator, {
	  instance = 0
}).

-record(list_of_set_up_pfcs, {
	  instance = 0
}).

-record(ps_handover_xid_parameters, {
	  instance = 0
}).

-record(ms_info_change_reporting_action, {
	  instance = 0
}).

-record(direct_tunnel_flags, {
	  instance = 0
}).

-record(correlation_id, {
	  instance = 0
}).

-record(bearer_control_mode, {
	  instance = 0
}).

-record(mbms_flow_identifier, {
	  instance = 0
}).

-record(mbms_ip_multicast_distribution, {
	  instance = 0
}).

-record(mbms_distribution_acknowledgement, {
	  instance = 0
}).

-record(reliable_inter_rat_handover_info, {
	  instance = 0
}).

-record(rfsp_index, {
	  instance = 0
}).

-record(fully_qualified_domain_name, {
	  instance = 0,
	  fqdn
}).

-record(evolved_allocation_retention_priority_i, {
	  instance = 0
}).

-record(evolved_allocation_retention_priority_ii, {
	  instance = 0
}).

-record(extended_common_flags, {
	  instance = 0,
	  flags = []
}).

-record(user_csg_information, {
	  instance = 0
}).

-record(csg_information_reporting_action, {
	  instance = 0
}).

-record(csg_id, {
	  instance = 0
}).

-record(csg_membership_indication, {
	  instance = 0
}).

-record(aggregate_maximum_bit_rate, {
	  instance = 0
}).

-record(ue_network_capability, {
	  instance = 0
}).

-record(ue_ambr, {
	  instance = 0
}).

-record(apn_ambr_with_nsapi, {
	  instance = 0
}).

-record(ggsn_back_off_time, {
	  instance = 0
}).

-record(signalling_priority_indication, {
	  instance = 0
}).

-record(signalling_priority_indication_with_nsapi, {
	  instance = 0
}).

-record(higher_bitrates_than_16_mbps_flag, {
	  instance = 0
}).

-record(additional_mm_context_for_srvcc, {
	  instance = 0
}).

-record(additional_flags_for_srvcc, {
	  instance = 0
}).

-record(stn_sr, {
	  instance = 0
}).

-record(c_msisdn, {
	  instance = 0
}).

-record(extended_ranap_cause, {
	  instance = 0
}).

-record(enodeb_id, {
	  instance = 0
}).

-record(selection_mode_with_nsapi, {
	  instance = 0
}).

-record(uli_timestamp, {
	  instance = 0
}).

-record(local_home_network_id_with_nsapi, {
	  instance = 0
}).

-record(cn_operator_selection_entity, {
	  instance = 0
}).

-record(charging_gateway_address, {
	  instance = 0
}).

-record(private_extension, {
	  instance = 0
}).

%% -include("gtp_packet_v2_gen.hrl").

-record(v2_international_mobile_subscriber_identity, {
	  instance = 0,
	  imsi
}).

-record(v2_cause, {
	  instance = 0,
	  v2_cause = reserved,
	  pce = 0,
	  bce = 0,
	  cs = 0
}).

-record(v2_recovery, {
	  instance = 0,
	  restart_counter = 0
}).

-record(v2_stn_sr, {
	  instance = 0
}).

-record(v2_access_point_name, {
	  instance = 0,
	  apn
}).

-record(v2_aggregate_maximum_bit_rate, {
	  instance = 0,
	  uplink = 0,
	  downlink = 0
}).

-record(v2_eps_bearer_id, {
	  instance = 0,
	  eps_bearer_id = 0
}).

-record(v2_ip_address, {
	  instance = 0,
	  ip = <<>>
}).

-record(v2_mobile_equipment_identity, {
	  instance = 0,
	  mei = <<>>
}).

-record(v2_msisdn, {
	  instance = 0,
	  msisdn
}).

-record(v2_indication, {
	  instance = 0,
	  flags
}).

-record(v2_protocol_configuration_options, {
	  instance = 0,
	  config
}).

-record(v2_pdn_address_allocation, {
	  instance = 0,
	  type = ipv4,
	  address = <<>>
}).

-record(v2_bearer_level_quality_of_service, {
	  instance = 0,
	  pci = 0,
	  pl = 0,
	  pvi = 0,
	  label = 0,
	  maximum_bit_rate_for_uplink = 0,
	  maximum_bit_rate_for_downlink = 0,
	  guaranteed_bit_rate_for_uplink = 0,
	  guaranteed_bit_rate_for_downlink = 0
}).

-record(v2_flow_quality_of_service, {
	  instance = 0
}).

-record(v2_rat_type, {
	  instance = 0,
	  rat_type = 0
}).


-record(v2_eps_bearer_level_traffic_flow_template, {
	  instance = 0
}).

-record(v2_traffic_aggregation_description, {
	  instance = 0
}).



-record(v2_tmsi, {
	  instance = 0
}).

-record(v2_global_cn_id, {
	  instance = 0
}).

-record(v2_s103_pdn_data_forwarding_info, {
	  instance = 0
}).

-record(v2_s1_u_data_forwarding_info, {
	  instance = 0
}).

-record(v2_delay_value, {
	  instance = 0
}).

-record(v2_bearer_context, {
	  instance = 0,
	  group
}).

-record(v2_charging_id, {
	  instance = 0,
	  id = <<0,0,0,0>>
}).

-record(v2_charging_characteristics, {
	  instance = 0
}).

-record(v2_trace_information, {
	  instance = 0
}).

-record(v2_bearer_flags, {
	  instance = 0
}).

-record(v2_pdn_type, {
	  instance = 0,
	  pdn_type = ipv4
}).

-record(v2_procedure_transaction_id, {
	  instance = 0
}).

-record(v2_mm_context_1, {
	  instance = 0
}).

-record(v2_mm_context_2, {
	  instance = 0
}).

-record(v2_mm_context_3, {
	  instance = 0
}).

-record(v2_mm_context_4, {
	  instance = 0
}).

-record(v2_mm_context_5, {
	  instance = 0
}).

-record(v2_mm_context_6, {
	  instance = 0
}).

-record(v2_pdn_connection, {
	  instance = 0
}).

-record(v2_pdu_numbers, {
	  instance = 0
}).

-record(v2_p_tmsi, {
	  instance = 0
}).

-record(v2_p_tmsi_signature, {
	  instance = 0
}).

-record(v2_hop_counter, {
	  instance = 0
}).

-record(v2_ue_time_zone, {
	  instance = 0,
	  timezone = 0,
	  dst = 0
}).

-record(v2_trace_reference, {
	  instance = 0
}).

-record(v2_complete_request_message, {
	  instance = 0
}).

-record(v2_guti, {
	  instance = 0
}).

-record(v2_f_container, {
	  instance = 0
}).

-record(v2_f_cause, {
	  instance = 0
}).

-record(v2_plmn_id, {
	  instance = 0
}).

-record(v2_target_identification, {
	  instance = 0
}).

-record(v2_packet_flow_id_, {
	  instance = 0
}).

-record(v2_rab_context_, {
	  instance = 0
}).

-record(v2_source_rnc_pdcp_context_info, {
	  instance = 0
}).

-record(v2_udp_source_port_number, {
	  instance = 0
}).

-record(v2_apn_restriction, {
	  instance = 0,
	  restriction_type_value = 0
}).

-record(v2_selection_mode, {
	  instance = 0,
	  mode = 0
}).

-record(v2_source_identification, {
	  instance = 0
}).

-record(v2_change_reporting_action, {
	  instance = 0,
	  action = stop_reporting
}).

-record(v2_fully_qualified_pdn_connection_set_identifier, {
	  instance = 0
}).

-record(v2_channel_needed, {
	  instance = 0
}).

-record(v2_emlpp_priority, {
	  instance = 0
}).

-record(v2_node_type, {
	  instance = 0
}).

-record(v2_fully_qualified_domain_name, {
	  instance = 0,
	  fqdn
}).

-record(v2_transaction_identifier, {
	  instance = 0
}).

-record(v2_mbms_session_duration, {
	  instance = 0
}).

-record(v2_mbms_service_area, {
	  instance = 0
}).

-record(v2_mbms_session_identifier, {
	  instance = 0
}).

-record(v2_mbms_flow_identifier, {
	  instance = 0
}).

-record(v2_mbms_ip_multicast_distribution, {
	  instance = 0
}).

-record(v2_mbms_distribution_acknowledge, {
	  instance = 0
}).

-record(v2_rfsp_index, {
	  instance = 0
}).

-record(v2_user_csg_information, {
	  instance = 0
}).

-record(v2_csg_information_reporting_action, {
	  instance = 0
}).

-record(v2_csg_id, {
	  instance = 0
}).

-record(v2_csg_membership_indication, {
	  instance = 0
}).

-record(v2_service_indicator, {
	  instance = 0
}).

-record(v2_detach_type, {
	  instance = 0
}).

-record(v2_local_distiguished_name, {
	  instance = 0
}).

-record(v2_node_features, {
	  instance = 0
}).

-record(v2_mbms_time_to_data_transfer, {
	  instance = 0
}).

-record(v2_throttling, {
	  instance = 0
}).

-record(v2_allocation_retention_priority, {
	  instance = 0
}).

-record(v2_epc_timer, {
	  instance = 0
}).

-record(v2_signalling_priority_indication, {
	  instance = 0
}).

-record(v2_temporary_mobile_group_identity, {
	  instance = 0
}).

-record(v2_additional_mm_context_for_srvcc, {
	  instance = 0
}).

-record(v2_additional_flags_for_srvcc, {
	  instance = 0
}).

-record(v2_mdt_configuration, {
	  instance = 0
}).

-record(v2_additional_protocol_configuration_options, {
	  instance = 0
}).

-record(v2_absolute_time_of_mbms_data_transfer, {
	  instance = 0
}).

-record(v2_henb_information_reporting_, {
	  instance = 0
}).

-record(v2_ipv4_configuration_parameters, {
	  instance = 0
}).

-record(v2_change_to_report_flags_, {
	  instance = 0
}).

-record(v2_action_indication, {
	  instance = 0
}).

-record(v2_twan_identifier, {
	  instance = 0
}).

-record(v2_uli_timestamp, {
	  instance = 0
}).

-record(v2_mbms_flags, {
	  instance = 0
}).

-record(v2_ran_nas_cause, {
	  instance = 0
}).

-record(v2_cn_operator_selection_entity, {
	  instance = 0
}).

-record(v2_trusted_wlan_mode_indication, {
	  instance = 0
}).

-record(v2_node_number, {
	  instance = 0
}).

-record(v2_node_identifier, {
	  instance = 0
}).

-record(v2_presence_reporting_area_action, {
	  instance = 0
}).

-record(v2_presence_reporting_area_information, {
	  instance = 0
}).

-record(v2_twan_identifier_timestamp, {
	  instance = 0
}).

-record(v2_overload_control_information, {
	  instance = 0
}).

-record(v2_load_control_information, {
	  instance = 0
}).

-record(v2_metric, {
	  instance = 0
}).

-record(v2_sequence_number, {
	  instance = 0
}).

-record(v2_apn_and_relative_capacity, {
	  instance = 0
}).

-record(v2_wlan_offloadability_indication, {
	  instance = 0
}).

-record(v2_private_extension, {
	  instance = 0
}).
