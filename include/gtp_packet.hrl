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
	  version	:: 'undefined' | 'v1' | 'v2' |
			   'prime_v0' | 'prime_v0s' | 'prime_v1' | 'prime_v2',
	  type		:: atom(),
	  tei		:: 0..16#ffffffff | undefined,
	  seq_no	:: 0..16#ffffff | undefined,
	  n_pdu		:: 0..16#ff | undefined,
	  ext_hdr = []	:: [term()],
	  ie		:: [term()] | map() | binary()
	 }).

-record(cgi, {plmn_id, lac, ci}).
-record(sai, {plmn_id, lac, sac}).
-record(rai, {plmn_id, lac, rac}).
-record(tai, {plmn_id, tac}).
-record(ecgi, {plmn_id, eci}).
-record(lai, {plmn_id, lac}).
-record(macro_enb, {plmn_id, id}).
-record(ext_macro_enb, {plmn_id, id}).

-record(routeing_area_identity, {
	  instance = 0,
	  identity
	 }).

-record(user_location_information, {
	  instance = 0,
	  location
	 }).

-record(data_record_packet, {
	  instance = 0,
	  format,
	  application,
	  version,
	  records = []
	 }).

-record(v2_user_location_information, {
	  instance = 0,
	  cgi,
	  sai,
	  rai,
	  tai,
	  ecgi,
	  lai,
	  macro_enb,
	  ext_macro_enb
	}).

-record(v2_fully_qualified_tunnel_endpoint_identifier, {
	  instance = 0,
	  interface_type,
	  key,
	  ipv4,
	  ipv6
	 }).

-record(v2_fully_qualified_pdn_connection_set_identifier, {
	  instance = 0,
	  node_id_type = 0,
	  node_id,
	  csids = []
	 }).

-record(v2_private_extension, {
	  instance = 0,
	  enterprise_id = 0,
	  value = <<>>
	 }).

-record(v2_twan_identifier, {
	  instance = 0,
	  ssid = <<>>,
	  bssid,
	  civic_address,
	  plmn_id,
	  operator_name,
	  relay_identity_type,
	  relay_identity,
	  circuit_id
	 }).

-record(v2_paging_and_service_information, {
	  instance = 0,
	  ebi = 0,
	  ppi
	 }).

-record(v2_integer_number, {
	  instance = 0,
	  width = 0,
	  value = 0
	 }).

-record(v2_remote_user_id, {
	  instance = 0,
	  imsi = <<>>,
	  msisdn,
	  imei
	 }).

-record(v2_maximum_packet_loss_rate, {
	  instance = 0,
	  ul,
	  dl
	 }).

-record(v2_monitoring_event_extension_information, {
	  instance = 0,
	  scef_reference_id = 0,
	  scef_id = <<>>,
	  remaining_minimum_lrtp
}).

%% -include("gtp_packet_v1_gen.hrl").

-define(GTP_V1_RECORDS, [cause,international_mobile_subscriber_identity,
                         routeing_area_identity,
                         temporary_logical_link_identity,packet_tmsi,
                         reordering_required,authentication_triplet,map_cause,
                         p_tmsi_signature,ms_validated,recovery,
                         selection_mode,tunnel_endpoint_identifier_data_i,
                         tunnel_endpoint_identifier_control_plane,
                         tunnel_endpoint_identifier_data_ii,teardown_ind,
                         nsapi,ranap_cause,rab_context,radio_priority_sms,
                         radio_priority,packet_flow_id,
                         charging_characteristics,trace_reference,trace_type,
                         ms_not_reachable_reason,packet_transfer_command,
                         charging_id,end_user_address,mm_context_gsm,
                         mm_context_umts,mm_context_gsm_and_umts,
                         mm_context_umts_and_used_cipher,pdp_context,
                         access_point_name,protocol_configuration_options,
                         gsn_address,ms_international_pstn_isdn_number,
                         quality_of_service_profile,authentication_quintuplet,
                         traffic_flow_template,target_identification,
                         utran_transparent_container,rab_setup_information,
                         extension_header_type_list,trigger_id,omc_identity,
                         ran_transparent_container,pdp_context_prioritization,
                         additional_rab_setup_information,sgsn_number,
                         common_flags,apn_restriction,radio_priority_lcs,
                         rat_type,user_location_information,ms_time_zone,imei,
                         camel_charging_information_container,mbms_ue_context,
                         temporary_mobile_group_identity,rim_routing_address,
                         mbms_protocol_configuration_options,
                         mbms_service_area,source_rnc_pdcp_context_info,
                         additional_trace_info,hop_counter,selected_plmn_id,
                         mbms_session_identifier,mbms_2g_3g_indicator,
                         enhanced_nsapi,mbms_session_duration,
                         additional_mbms_trace_info,
                         mbms_session_repetition_number,
                         mbms_time_to_data_transfer,bss_container,
                         cell_identification,pdu_numbers,bssgp_cause,
                         required_mbms_bearer_capabilities,
                         rim_routing_address_discriminator,
                         list_of_set_up_pfcs,ps_handover_xid_parameters,
                         ms_info_change_reporting_action,direct_tunnel_flags,
                         correlation_id,bearer_control_mode,
                         mbms_flow_identifier,mbms_ip_multicast_distribution,
                         mbms_distribution_acknowledgement,
                         reliable_inter_rat_handover_info,rfsp_index,
                         fully_qualified_domain_name,
                         evolved_allocation_retention_priority_i,
                         evolved_allocation_retention_priority_ii,
                         extended_common_flags,user_csg_information,
                         csg_information_reporting_action,csg_id,
                         csg_membership_indication,aggregate_maximum_bit_rate,
                         ue_network_capability,ue_ambr,apn_ambr_with_nsapi,
                         ggsn_back_off_time,signalling_priority_indication,
                         signalling_priority_indication_with_nsapi,
                         higher_bitrates_than_16_mbps_flag,
                         additional_mm_context_for_srvcc,
                         additional_flags_for_srvcc,stn_sr,c_msisdn,
                         extended_ranap_cause,enodeb_id,
                         selection_mode_with_nsapi,uli_timestamp,
                         local_home_network_id_with_nsapi,
                         cn_operator_selection_entity,
                         sequence_numbers_of_released_packets,
                         sequence_numbers_of_cancelled_packets,
                         charging_gateway_address,data_record_packet,
                         requests_responded,address_of_recommended_node,
                         private_extension]).

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
	  value = <<0>>
}).

-record(p_tmsi_signature, {
	  instance = 0,
	  value = <<0,0,0>>
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
	  value = 0
}).

-record(rab_context, {
	  instance = 0,
	  nsapi = 0,
	  dl_gtp_u_sequence_number = 0,
	  ul_gtp_u_sequence_number = 0,
	  dl_pdcp_sequence_number = 0,
	  ul_pdcp_sequence_number = 0
}).

-record(radio_priority_sms, {
	  instance = 0,
	  value = 0
}).

-record(radio_priority, {
	  instance = 0,
	  nsapi = 0,
	  value = 0
}).

-record(packet_flow_id, {
	  instance = 0,
	  nsapi = 0,
	  value = 0
}).

-record(charging_characteristics, {
	  instance = 0,
	  value = <<0,0>>
}).

-record(trace_reference, {
	  instance = 0,
	  value = 0
}).

-record(trace_type, {
	  instance = 0,
	  value = 0
}).

-record(ms_not_reachable_reason, {
	  instance = 0,
	  value = 0
}).

-record(packet_transfer_command, {
	  instance = 0,
	  command = send_data_record_packet
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
	  instance = 0,
	  restriction_type_value = 0
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
	  instance = 0,
	  action = stop_reporting
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
	  instance = 0,
	  pci = 0,
	  pl = 0,
	  pvi = 0
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
	  instance = 0,
	  uplink = 0,
	  downlink = 0
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

-record(sequence_numbers_of_released_packets, {
	  instance = 0,
	  sequence_numbers
}).

-record(sequence_numbers_of_cancelled_packets, {
	  instance = 0,
	  sequence_numbers
}).

-record(charging_gateway_address, {
	  instance = 0,
	  address = <<>>
}).


-record(requests_responded, {
	  instance = 0,
	  sequence_numbers
}).

-record(address_of_recommended_node, {
	  instance = 0,
	  address = <<>>
}).

-record(private_extension, {
	  instance = 0,
	  enterprise_id = 0,
	  value = <<>>
}).

%% -include("gtp_packet_v2_gen.hrl").

-define(GTP_V2_RECORDS, [v2_international_mobile_subscriber_identity,v2_cause,
                         v2_recovery,v2_stn_sr,v2_access_point_name,
                         v2_aggregate_maximum_bit_rate,v2_eps_bearer_id,
                         v2_ip_address,v2_mobile_equipment_identity,v2_msisdn,
                         v2_indication,v2_protocol_configuration_options,
                         v2_pdn_address_allocation,
                         v2_bearer_level_quality_of_service,
                         v2_flow_quality_of_service,v2_rat_type,
                         v2_serving_network,
                         v2_eps_bearer_level_traffic_flow_template,
                         v2_traffic_aggregation_description,
                         v2_user_location_information,
                         v2_fully_qualified_tunnel_endpoint_identifier,
                         v2_tmsi,v2_global_cn_id,
                         v2_s103_pdn_data_forwarding_info,
                         v2_s1_u_data_forwarding_info,v2_delay_value,
                         v2_bearer_context,v2_charging_id,
                         v2_charging_characteristics,v2_trace_information,
                         v2_bearer_flags,v2_pdn_type,
                         v2_procedure_transaction_id,v2_mm_context_1,
                         v2_mm_context_2,v2_mm_context_3,v2_mm_context_4,
                         v2_mm_context_5,v2_mm_context_6,v2_pdn_connection,
                         v2_pdu_numbers,v2_p_tmsi,v2_p_tmsi_signature,
                         v2_hop_counter,v2_ue_time_zone,v2_trace_reference,
                         v2_complete_request_message,v2_guti,v2_f_container,
                         v2_f_cause,v2_plmn_id,v2_target_identification,
                         v2_packet_flow_id,v2_rab_context,
                         v2_source_rnc_pdcp_context_info,
                         v2_udp_source_port_number,v2_apn_restriction,
                         v2_selection_mode,v2_source_identification,
                         v2_change_reporting_action,
                         v2_fully_qualified_pdn_connection_set_identifier,
                         v2_channel_needed,v2_emlpp_priority,v2_node_type,
                         v2_fully_qualified_domain_name,
                         v2_transaction_identifier,v2_mbms_session_duration,
                         v2_mbms_service_area,v2_mbms_session_identifier,
                         v2_mbms_flow_identifier,
                         v2_mbms_ip_multicast_distribution,
                         v2_mbms_distribution_acknowledge,v2_rfsp_index,
                         v2_user_csg_information,
                         v2_csg_information_reporting_action,v2_csg_id,
                         v2_csg_membership_indication,v2_service_indicator,
                         v2_detach_type,v2_local_distiguished_name,
                         v2_node_features,v2_mbms_time_to_data_transfer,
                         v2_throttling,v2_allocation_retention_priority,
                         v2_epc_timer,v2_signalling_priority_indication,
                         v2_temporary_mobile_group_identity,
                         v2_additional_mm_context_for_srvcc,
                         v2_additional_flags_for_srvcc,v2_mdt_configuration,
                         v2_additional_protocol_configuration_options,
                         v2_absolute_time_of_mbms_data_transfer,
                         v2_henb_information_reporting_,
                         v2_ipv4_configuration_parameters,
                         v2_change_to_report_flags_,v2_action_indication,
                         v2_twan_identifier,v2_uli_timestamp,v2_mbms_flags,
                         v2_ran_nas_cause,v2_cn_operator_selection_entity,
                         v2_trusted_wlan_mode_indication,v2_node_number,
                         v2_node_identifier,v2_presence_reporting_area_action,
                         v2_presence_reporting_area_information,
                         v2_twan_identifier_timestamp,
                         v2_overload_control_information,
                         v2_load_control_information,v2_metric,
                         v2_sequence_number,v2_apn_and_relative_capacity,
                         v2_wlan_offloadability_indication,
                         v2_paging_and_service_information,v2_integer_number,
                         v2_millisecond_time_stamp,
                         v2_monitoring_event_information,v2_ecgi_list,
                         v2_remote_ue_context,v2_remote_user_id,
                         v2_remote_ue_ip_information,
                         v2_ciot_optimizations_support_indication,
                         v2_scef_pdn_connection,
                         v2_header_compression_configuration,
                         v2_extended_protocol_configuration_options,
                         v2_serving_plmn_rate_control,v2_counter,
                         v2_mapped_ue_usage_type,
                         v2_secondary_rat_usage_data_report,
                         v2_up_function_selection_indication_flags,
                         v2_maximum_packet_loss_rate,
                         v2_apn_rate_control_status,
                         v2_extended_trace_information,
                         v2_monitoring_event_extension_information,
                         v2_additional_rrm_policy_index,v2_private_extension]).

-record(v2_international_mobile_subscriber_identity, {
	  instance = 0,
	  imsi
}).

-record(v2_cause, {
	  instance = 0,
	  v2_cause = reserved,
	  pce = 0,
	  bce = 0,
	  cs = 0,
	  offending_ie
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
	  mei
}).

-record(v2_msisdn, {
	  instance = 0,
	  msisdn
}).

-record(v2_indication, {
	  instance = 0,
	  flags = []
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
	  instance = 0,
	  label = 0,
	  maximum_bit_rate_for_uplink = 0,
	  maximum_bit_rate_for_downlink = 0,
	  guaranteed_bit_rate_for_uplink = 0,
	  guaranteed_bit_rate_for_downlink = 0
}).

-record(v2_rat_type, {
	  instance = 0,
	  rat_type = 0
}).

-record(v2_serving_network, {
	  instance = 0,
	  plmn_id = {<<"001">>, <<"001">>}
}).

-record(v2_eps_bearer_level_traffic_flow_template, {
	  instance = 0,
	  value = <<>>
}).

-record(v2_traffic_aggregation_description, {
	  instance = 0,
	  value = <<>>
}).



-record(v2_tmsi, {
	  instance = 0,
	  value = 0
}).

-record(v2_global_cn_id, {
	  instance = 0,
	  plmn_id = {<<"001">>, <<"001">>},
	  value = <<>>
}).

-record(v2_s103_pdn_data_forwarding_info, {
	  instance = 0,
	  hsgw_address = <<>>,
	  gre_key = 0,
	  eps_bearer_id = []
}).

-record(v2_s1_u_data_forwarding_info, {
	  instance = 0,
	  service_gw_address = <<>>,
	  teid = 0
}).

-record(v2_delay_value, {
	  instance = 0,
	  delay = 0
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
	  instance = 0,
	  value = <<0,0>>
}).

-record(v2_trace_information, {
	  instance = 0,
	  plmn_id = {<<"001">>, <<"001">>},
	  trace_id = 0,
	  triggering_events = <<0,0,0,0,0,0,0,0,0>>,
	  list_of_ne_types = 0,
	  session_trace_depth = 0,
	  list_of_interfaces = <<0,0,0,0,0,0,0,0,0,0,0,0>>,
	  ip_address_of_trace_collection_entity = <<>>
}).

-record(v2_bearer_flags, {
	  instance = 0,
	  flags = []
}).

-record(v2_pdn_type, {
	  instance = 0,
	  pdn_type = ipv4
}).

-record(v2_procedure_transaction_id, {
	  instance = 0,
	  pti = 0
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
	  instance = 0,
	  group
}).

-record(v2_pdu_numbers, {
	  instance = 0,
	  nsapi = 0,
	  dl_gtp_u_sequence_number = 0,
	  ul_gtp_u_sequence_number = 0,
	  send_n_pdu_number = 0,
	  receive_n_pdu_number = 0
}).

-record(v2_p_tmsi, {
	  instance = 0,
	  value = <<>>
}).

-record(v2_p_tmsi_signature, {
	  instance = 0,
	  value = <<>>
}).

-record(v2_hop_counter, {
	  instance = 0,
	  hop_counter = 0
}).

-record(v2_ue_time_zone, {
	  instance = 0,
	  timezone = 0,
	  dst = 0
}).

-record(v2_trace_reference, {
	  instance = 0,
	  plmn_id = {<<"001">>, <<"001">>},
	  id = 0
}).

-record(v2_complete_request_message, {
	  instance = 0,
	  type = 0,
	  message = <<>>
}).

-record(v2_guti, {
	  instance = 0,
	  plmn_id = {<<"001">>, <<"001">>},
	  group_id = 0,
	  code = 0,
	  m_tmsi = <<>>
}).

-record(v2_f_container, {
	  instance = 0,
	  type = 0,
	  data = <<>>
}).

-record(v2_f_cause, {
	  instance = 0,
	  type = 0,
	  data = <<>>
}).

-record(v2_plmn_id, {
	  instance = 0,
	  id = <<0,0,0>>
}).

-record(v2_target_identification, {
	  instance = 0,
	  type = 0,
	  data = <<>>
}).

-record(v2_packet_flow_id, {
	  instance = 0,
	  ebi = 0,
	  flow_id = <<>>
}).

-record(v2_rab_context, {
	  instance = 0,
	  ulpsi = 0,
	  dlpsi = 0,
	  ulgsi = 0,
	  dlgsi = 0,
	  nsapi = 0,
	  dl_gtp_u_sequence_number = 0,
	  ul_gtp_u_sequence_number = 0,
	  dl_pdcp_number = 0,
	  ul_pdcp_number = 0
}).

-record(v2_source_rnc_pdcp_context_info, {
	  instance = 0,
	  rrc_container = <<>>
}).

-record(v2_udp_source_port_number, {
	  instance = 0,
	  port = 0
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
	  instance = 0,
	  target_cell_id = <<>>,
	  source_type = 0,
	  source_id = <<>>
}).

-record(v2_change_reporting_action, {
	  instance = 0,
	  action = stop_reporting
}).


-record(v2_channel_needed, {
	  instance = 0,
	  value = <<>>
}).

-record(v2_emlpp_priority, {
	  instance = 0,
	  value = <<>>
}).

-record(v2_node_type, {
	  instance = 0,
	  node_type = 0
}).

-record(v2_fully_qualified_domain_name, {
	  instance = 0,
	  fqdn
}).

-record(v2_transaction_identifier, {
	  instance = 0,
	  value = <<>>
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
	  instance = 0,
	  value = 0
}).

-record(v2_user_csg_information, {
	  instance = 0,
	  plmn_id = {<<"001">>, <<"001">>},
	  csg_id = <<0,0,0,0:3>>,
	  access_mode = 0,
	  lcsg = false,
	  cmi = 0
}).

-record(v2_csg_information_reporting_action, {
	  instance = 0,
	  actions = []
}).

-record(v2_csg_id, {
	  instance = 0,
	  id = <<0,0,0,0:3>>
}).

-record(v2_csg_membership_indication, {
	  instance = 0,
	  cmi = 0
}).

-record(v2_service_indicator, {
	  instance = 0,
	  value = 0
}).

-record(v2_detach_type, {
	  instance = 0,
	  value = 0
}).

-record(v2_local_distiguished_name, {
	  instance = 0,
	  value = <<>>
}).

-record(v2_node_features, {
	  instance = 0,
	  features = []
}).

-record(v2_mbms_time_to_data_transfer, {
	  instance = 0
}).

-record(v2_throttling, {
	  instance = 0,
	  unit = 0,
	  value = 0,
	  factor = 0
}).

-record(v2_allocation_retention_priority, {
	  instance = 0,
	  pci = false,
	  pl = 0,
	  pvi = false
}).

-record(v2_epc_timer, {
	  instance = 0,
	  unit = 0,
	  value = 0
}).

-record(v2_signalling_priority_indication, {
	  instance = 0,
	  indication = []
}).

-record(v2_temporary_mobile_group_identity, {
	  instance = 0
}).

-record(v2_additional_mm_context_for_srvcc, {
	  instance = 0,
	  classmark_2 = <<>>,
	  classmark_3 = <<>>,
	  codec_list = <<>>
}).

-record(v2_additional_flags_for_srvcc, {
	  instance = 0,
	  flags = []
}).

-record(v2_mdt_configuration, {
	  instance = 0
}).

-record(v2_additional_protocol_configuration_options, {
	  instance = 0,
	  config
}).

-record(v2_absolute_time_of_mbms_data_transfer, {
	  instance = 0
}).

-record(v2_henb_information_reporting_, {
	  instance = 0,
	  flags = []
}).

-record(v2_ipv4_configuration_parameters, {
	  instance = 0,
	  prefix_length = 0,
	  default_route = <<0,0,0,0>>
}).

-record(v2_change_to_report_flags_, {
	  instance = 0,
	  flags = []
}).

-record(v2_action_indication, {
	  instance = 0,
	  indication = 0
}).


-record(v2_uli_timestamp, {
	  instance = 0,
	  timestamp = 0
}).

-record(v2_mbms_flags, {
	  instance = 0
}).

-record(v2_ran_nas_cause, {
	  instance = 0,
	  protocol = 0,
	  type = 0,
	  cause = <<>>
}).

-record(v2_cn_operator_selection_entity, {
	  instance = 0,
	  entity = 0
}).

-record(v2_trusted_wlan_mode_indication, {
	  instance = 0,
	  indication = []
}).

-record(v2_node_number, {
	  instance = 0,
	  number = <<>>
}).

-record(v2_node_identifier, {
	  instance = 0,
	  name = <<>>,
	  realm = <<>>
}).

-record(v2_presence_reporting_area_action, {
	  instance = 0
}).

-record(v2_presence_reporting_area_information, {
	  instance = 0
}).

-record(v2_twan_identifier_timestamp, {
	  instance = 0,
	  timestamp = 0
}).

-record(v2_overload_control_information, {
	  instance = 0,
	  group
}).

-record(v2_load_control_information, {
	  instance = 0,
	  group
}).

-record(v2_metric, {
	  instance = 0,
	  value = 0
}).

-record(v2_sequence_number, {
	  instance = 0,
	  value = 0
}).

-record(v2_apn_and_relative_capacity, {
	  instance = 0,
	  capacity = 0,
	  apn = <<>>
}).

-record(v2_wlan_offloadability_indication, {
	  instance = 0,
	  indication = []
}).



-record(v2_millisecond_time_stamp, {
	  instance = 0,
	  timestamp = 0
}).

-record(v2_monitoring_event_information, {
	  instance = 0
}).

-record(v2_ecgi_list, {
	  instance = 0,
	  ecgis = []
}).

-record(v2_remote_ue_context, {
	  instance = 0,
	  group
}).


-record(v2_remote_ue_ip_information, {
	  instance = 0,
	  ip = <<>>
}).

-record(v2_ciot_optimizations_support_indication, {
	  instance = 0,
	  indication = []
}).

-record(v2_scef_pdn_connection, {
	  instance = 0,
	  group
}).

-record(v2_header_compression_configuration, {
	  instance = 0,
	  rohc_profiles = 0,
	  max_cid = 0
}).

-record(v2_extended_protocol_configuration_options, {
	  instance = 0,
	  config
}).

-record(v2_serving_plmn_rate_control, {
	  instance = 0,
	  uplink = 0,
	  downlink = 0
}).

-record(v2_counter, {
	  instance = 0,
	  timestamp = 0,
	  counter = 0
}).

-record(v2_mapped_ue_usage_type, {
	  instance = 0,
	  usage_type = 0
}).

-record(v2_secondary_rat_usage_data_report, {
	  instance = 0,
	  irsgw = false,
	  irpgw = false,
	  rat_type = 0,
	  ebi = 0,
	  start_time = 0,
	  end_time = 0,
	  dl = 0,
	  ul = 0
}).

-record(v2_up_function_selection_indication_flags, {
	  instance = 0,
	  indication = []
}).


-record(v2_apn_rate_control_status, {
	  instance = 0,
	  number_of_uplink_packets_allowed = 0,
	  number_of_additional_exception_reports = 0,
	  number_of_downlink_packets_allowed = 0,
	  apn_rate_control_status_validity_time = 0
}).

-record(v2_extended_trace_information, {
	  instance = 0,
	  plmn_id = {<<"001">>, <<"001">>},
	  trace_id = 0,
	  triggering_events = <<>>,
	  list_of_ne_types = <<>>,
	  session_trace_depth = 0,
	  list_of_interfaces = <<>>,
	  ip_address_of_trace_collection_entity = <<>>
}).


-record(v2_additional_rrm_policy_index, {
	  instance = 0,
	  value = 0
}).

