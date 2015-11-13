%% This file is auto-generated. DO NOT EDIT

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
message_type_v2({Vendor, Type}) when is_integer(Vendor), is_integer(Type) -> {Vendor, Type}.

enum_v2_pdn_type(ipv4) -> 1;
enum_v2_pdn_type(ipv6) -> 2;
enum_v2_pdn_type(ipv4v6) -> 3;
enum_v2_pdn_type(1) -> ipv4;
enum_v2_pdn_type(2) -> ipv6;
enum_v2_pdn_type(3) -> ipv4v6.

enum_v2_rat_type(reserved) -> 0;
enum_v2_rat_type(utran) -> 1;
enum_v2_rat_type(geran) -> 2;
enum_v2_rat_type(wlan) -> 3;
enum_v2_rat_type(gan) -> 4;
enum_v2_rat_type(hspa_evolution) -> 5;
enum_v2_rat_type(eutran) -> 6;
enum_v2_rat_type(virtual) -> 7;
enum_v2_rat_type(0) -> reserved;
enum_v2_rat_type(1) -> utran;
enum_v2_rat_type(2) -> geran;
enum_v2_rat_type(3) -> wlan;
enum_v2_rat_type(4) -> gan;
enum_v2_rat_type(5) -> hspa_evolution;
enum_v2_rat_type(6) -> eutran;
enum_v2_rat_type(7) -> virtual.

enum_v2_type(ipv4) -> 1;
enum_v2_type(ipv6) -> 2;
enum_v2_type(ipv4v6) -> 3;
enum_v2_type(1) -> ipv4;
enum_v2_type(2) -> ipv6;
enum_v2_type(3) -> ipv4v6.

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
enum_v2_v2_cause(120) -> gtp_c_entity_congestion.

decode_v2_element(1, Instance, <<M_imsi/binary>>) ->
    #v2_international_mobile_subscriber_identity{instance = Instance,
                                                 imsi = decode_tbcd(M_imsi)};

decode_v2_element(2, Instance, <<M_v2_cause:8/integer,
                                 _:5,
                                 M_pce:1/integer,
                                 M_bce:1/integer,
                                 M_cs:1/integer,
                                 M_data/binary>>) ->
    #v2_cause{instance = Instance,
              v2_cause = enum_v2_v2_cause(M_v2_cause),
              pce = M_pce,
              bce = M_bce,
              cs = M_cs,
              data = M_data};

decode_v2_element(3, Instance, <<M_restart_counter:8/integer>>) ->
    #v2_recovery{instance = Instance,
                 restart_counter = M_restart_counter};

decode_v2_element(51, Instance, <<>>) ->
    #v2_stn_sr{instance = Instance};

decode_v2_element(71, Instance, <<M_apn/binary>>) ->
    #v2_access_point_name{instance = Instance,
                          apn = M_apn};

decode_v2_element(72, Instance, <<M_uplink:32/integer,
                                  M_downlink:32/integer>>) ->
    #v2_aggregate_maximum_bit_rate{instance = Instance,
                                   uplink = M_uplink,
                                   downlink = M_downlink};

decode_v2_element(73, Instance, <<_:4,
                                  M_eps_bearer_id:4/integer,
                                  M_data/binary>>) ->
    #v2_eps_bearer_id{instance = Instance,
                      eps_bearer_id = M_eps_bearer_id,
                      data = M_data};

decode_v2_element(74, Instance, <<M_ip/binary>>) ->
    #v2_ip_address{instance = Instance,
                   ip = M_ip};

decode_v2_element(75, Instance, <<M_mei/binary>>) ->
    #v2_mobile_equipment_identity{instance = Instance,
                                  mei = M_mei};

decode_v2_element(76, Instance, <<M_msisdn/binary>>) ->
    #v2_msisdn{instance = Instance,
               msisdn = decode_tbcd(M_msisdn)};

decode_v2_element(77, Instance, <<M_flags/binary>>) ->
    #v2_indication{instance = Instance,
                   flags = decode_v2_indication_flags(M_flags)};

decode_v2_element(78, Instance, <<M_config/binary>>) ->
    #v2_protocol_configuration_options{instance = Instance,
                                       config = decode_protocol_config_opts(M_config)};

decode_v2_element(79, Instance, <<_:5,
                                  M_type:3/integer,
                                  M_address/binary>>) ->
    #v2_pdn_address_allocation{instance = Instance,
                               type = enum_v2_type(M_type),
                               address = M_address};

decode_v2_element(80, Instance, <<_:1,
                                  M_pci:1/integer,
                                  M_pl:4/integer,
                                  _:1,
                                  M_pvi:1/integer,
                                  M_label:8/integer,
                                  M_maximum_bit_rate_for_uplink:32/integer,
                                  M_maximum_bit_rate_for_downlink:32/integer,
                                  M_guaranteed_bit_rate_for_uplink:32/integer,
                                  M_guaranteed_bit_rate_for_downlink:32/integer,
                                  M_data/binary>>) ->
    #v2_bearer_level_quality_of_service{instance = Instance,
                                        pci = M_pci,
                                        pl = M_pl,
                                        pvi = M_pvi,
                                        label = M_label,
                                        maximum_bit_rate_for_uplink = M_maximum_bit_rate_for_uplink,
                                        maximum_bit_rate_for_downlink = M_maximum_bit_rate_for_downlink,
                                        guaranteed_bit_rate_for_uplink = M_guaranteed_bit_rate_for_uplink,
                                        guaranteed_bit_rate_for_downlink = M_guaranteed_bit_rate_for_downlink,
                                        data = M_data};

decode_v2_element(81, Instance, <<>>) ->
    #v2_flow_quality_of_service{instance = Instance};

decode_v2_element(82, Instance, <<M_rat_type:8/integer,
                                  M_optional/binary>>) ->
    #v2_rat_type{instance = Instance,
                 rat_type = enum_v2_rat_type(M_rat_type),
                 optional = M_optional};

decode_v2_element(83, Instance, Data) ->
    decode_v2_mccmcn(Instance, Data);

decode_v2_element(84, Instance, <<>>) ->
    #v2_eps_bearer_level_traffic_flow_template{instance = Instance};

decode_v2_element(85, Instance, <<>>) ->
    #v2_traffic_aggregation_description{instance = Instance};

decode_v2_element(86, Instance, Data) ->
    decode_v2_user_location_information(Instance, Data);

decode_v2_element(87, Instance, Data) ->
    decode_v2_fully_qualified_tunnel_endpoint_identifier(Instance, Data);

decode_v2_element(88, Instance, <<>>) ->
    #v2_tmsi{instance = Instance};

decode_v2_element(89, Instance, <<>>) ->
    #v2_global_cn_id{instance = Instance};

decode_v2_element(90, Instance, <<>>) ->
    #v2_s103_pdn_data_forwarding_info{instance = Instance};

decode_v2_element(91, Instance, <<>>) ->
    #v2_s1_u_data_forwarding_info{instance = Instance};

decode_v2_element(92, Instance, <<>>) ->
    #v2_delay_value{instance = Instance};

decode_v2_element(93, Instance, <<M_group/binary>>) ->
    #v2_bearer_context{instance = Instance,
                       group = decode_v2_grouped(M_group)};

decode_v2_element(94, Instance, <<>>) ->
    #v2_charging_id{instance = Instance};

decode_v2_element(95, Instance, <<>>) ->
    #v2_charging_characteristics{instance = Instance};

decode_v2_element(96, Instance, <<>>) ->
    #v2_trace_information{instance = Instance};

decode_v2_element(97, Instance, <<>>) ->
    #v2_bearer_flags{instance = Instance};

decode_v2_element(99, Instance, <<_:4,
                                  M_pdn_type:4/integer,
                                  M_data/binary>>) ->
    #v2_pdn_type{instance = Instance,
                 pdn_type = enum_v2_pdn_type(M_pdn_type),
                 data = M_data};

decode_v2_element(100, Instance, <<>>) ->
    #v2_procedure_transaction_id{instance = Instance};

decode_v2_element(103, Instance, <<>>) ->
    #v2_mm_context_1{instance = Instance};

decode_v2_element(104, Instance, <<>>) ->
    #v2_mm_context_2{instance = Instance};

decode_v2_element(105, Instance, <<>>) ->
    #v2_mm_context_3{instance = Instance};

decode_v2_element(106, Instance, <<>>) ->
    #v2_mm_context_4{instance = Instance};

decode_v2_element(107, Instance, <<>>) ->
    #v2_mm_context_5{instance = Instance};

decode_v2_element(108, Instance, <<>>) ->
    #v2_mm_context_6{instance = Instance};

decode_v2_element(109, Instance, <<>>) ->
    #v2_pdn_connection{instance = Instance};

decode_v2_element(110, Instance, <<>>) ->
    #v2_pdu_numbers{instance = Instance};

decode_v2_element(111, Instance, <<>>) ->
    #v2_p_tmsi{instance = Instance};

decode_v2_element(112, Instance, <<>>) ->
    #v2_p_tmsi_signature{instance = Instance};

decode_v2_element(113, Instance, <<>>) ->
    #v2_hop_counter{instance = Instance};

decode_v2_element(114, Instance, <<>>) ->
    #v2_ue_time_zone{instance = Instance};

decode_v2_element(115, Instance, <<>>) ->
    #v2_trace_reference{instance = Instance};

decode_v2_element(116, Instance, <<>>) ->
    #v2_complete_request_message{instance = Instance};

decode_v2_element(117, Instance, <<>>) ->
    #v2_guti{instance = Instance};

decode_v2_element(118, Instance, <<>>) ->
    #v2_f_container{instance = Instance};

decode_v2_element(119, Instance, <<>>) ->
    #v2_f_cause{instance = Instance};

decode_v2_element(120, Instance, <<>>) ->
    #v2_plmn_id{instance = Instance};

decode_v2_element(121, Instance, <<>>) ->
    #v2_target_identification{instance = Instance};

decode_v2_element(123, Instance, <<>>) ->
    #v2_packet_flow_id_{instance = Instance};

decode_v2_element(124, Instance, <<>>) ->
    #v2_rab_context_{instance = Instance};

decode_v2_element(125, Instance, <<>>) ->
    #v2_source_rnc_pdcp_context_info{instance = Instance};

decode_v2_element(126, Instance, <<>>) ->
    #v2_udp_source_port_number{instance = Instance};

decode_v2_element(127, Instance, <<M_restriction_type_value:8/integer,
                                   M_data/binary>>) ->
    #v2_apn_restriction{instance = Instance,
                        restriction_type_value = M_restriction_type_value,
                        data = M_data};

decode_v2_element(128, Instance, <<_:6,
                                   M_mode:2/integer,
                                   M_data/binary>>) ->
    #v2_selection_mode{instance = Instance,
                       mode = M_mode,
                       data = M_data};

decode_v2_element(129, Instance, <<>>) ->
    #v2_source_identification{instance = Instance};

decode_v2_element(131, Instance, <<>>) ->
    #v2_change_reporting_action{instance = Instance};

decode_v2_element(132, Instance, <<>>) ->
    #v2_fully_qualified_pdn_connection_set_identifier{instance = Instance};

decode_v2_element(133, Instance, <<>>) ->
    #v2_channel_needed{instance = Instance};

decode_v2_element(134, Instance, <<>>) ->
    #v2_emlpp_priority{instance = Instance};

decode_v2_element(135, Instance, <<>>) ->
    #v2_node_type{instance = Instance};

decode_v2_element(136, Instance, <<>>) ->
    #v2_fully_qualified_domain_name{instance = Instance};

decode_v2_element(137, Instance, <<>>) ->
    #v2_transaction_identifier{instance = Instance};

decode_v2_element(138, Instance, <<>>) ->
    #v2_mbms_session_duration{instance = Instance};

decode_v2_element(139, Instance, <<>>) ->
    #v2_mbms_service_area{instance = Instance};

decode_v2_element(140, Instance, <<>>) ->
    #v2_mbms_session_identifier{instance = Instance};

decode_v2_element(141, Instance, <<>>) ->
    #v2_mbms_flow_identifier{instance = Instance};

decode_v2_element(142, Instance, <<>>) ->
    #v2_mbms_ip_multicast_distribution{instance = Instance};

decode_v2_element(143, Instance, <<>>) ->
    #v2_mbms_distribution_acknowledge{instance = Instance};

decode_v2_element(144, Instance, <<>>) ->
    #v2_rfsp_index{instance = Instance};

decode_v2_element(145, Instance, <<>>) ->
    #v2_user_csg_information{instance = Instance};

decode_v2_element(146, Instance, <<>>) ->
    #v2_csg_information_reporting_action{instance = Instance};

decode_v2_element(147, Instance, <<>>) ->
    #v2_csg_id{instance = Instance};

decode_v2_element(148, Instance, <<>>) ->
    #v2_csg_membership_indication{instance = Instance};

decode_v2_element(149, Instance, <<>>) ->
    #v2_service_indicator{instance = Instance};

decode_v2_element(150, Instance, <<>>) ->
    #v2_detach_type{instance = Instance};

decode_v2_element(151, Instance, <<>>) ->
    #v2_local_distiguished_name{instance = Instance};

decode_v2_element(152, Instance, <<>>) ->
    #v2_node_features{instance = Instance};

decode_v2_element(153, Instance, <<>>) ->
    #v2_mbms_time_to_data_transfer{instance = Instance};

decode_v2_element(154, Instance, <<>>) ->
    #v2_throttling{instance = Instance};

decode_v2_element(155, Instance, <<>>) ->
    #v2_allocation_retention_priority{instance = Instance};

decode_v2_element(156, Instance, <<>>) ->
    #v2_epc_timer{instance = Instance};

decode_v2_element(157, Instance, <<>>) ->
    #v2_signalling_priority_indication{instance = Instance};

decode_v2_element(158, Instance, <<>>) ->
    #v2_temporary_mobile_group_identity{instance = Instance};

decode_v2_element(159, Instance, <<>>) ->
    #v2_additional_mm_context_for_srvcc{instance = Instance};

decode_v2_element(160, Instance, <<>>) ->
    #v2_additional_flags_for_srvcc{instance = Instance};

decode_v2_element(162, Instance, <<>>) ->
    #v2_mdt_configuration{instance = Instance};

decode_v2_element(163, Instance, <<>>) ->
    #v2_additional_protocol_configuration_options{instance = Instance};

decode_v2_element(164, Instance, <<>>) ->
    #v2_absolute_time_of_mbms_data_transfer{instance = Instance};

decode_v2_element(165, Instance, <<>>) ->
    #v2_henb_information_reporting_{instance = Instance};

decode_v2_element(166, Instance, <<>>) ->
    #v2_ipv4_configuration_parameters{instance = Instance};

decode_v2_element(167, Instance, <<>>) ->
    #v2_change_to_report_flags_{instance = Instance};

decode_v2_element(168, Instance, <<>>) ->
    #v2_action_indication{instance = Instance};

decode_v2_element(169, Instance, <<>>) ->
    #v2_twan_identifier{instance = Instance};

decode_v2_element(170, Instance, <<>>) ->
    #v2_uli_timestamp{instance = Instance};

decode_v2_element(171, Instance, <<>>) ->
    #v2_mbms_flags{instance = Instance};

decode_v2_element(172, Instance, <<>>) ->
    #v2_ran_nas_cause{instance = Instance};

decode_v2_element(173, Instance, <<>>) ->
    #v2_cn_operator_selection_entity{instance = Instance};

decode_v2_element(174, Instance, <<>>) ->
    #v2_trusted_wlan_mode_indication{instance = Instance};

decode_v2_element(175, Instance, <<>>) ->
    #v2_node_number{instance = Instance};

decode_v2_element(176, Instance, <<>>) ->
    #v2_node_identifier{instance = Instance};

decode_v2_element(177, Instance, <<>>) ->
    #v2_presence_reporting_area_action{instance = Instance};

decode_v2_element(178, Instance, <<>>) ->
    #v2_presence_reporting_area_information{instance = Instance};

decode_v2_element(179, Instance, <<>>) ->
    #v2_twan_identifier_timestamp{instance = Instance};

decode_v2_element(180, Instance, <<>>) ->
    #v2_overload_control_information{instance = Instance};

decode_v2_element(181, Instance, <<>>) ->
    #v2_load_control_information{instance = Instance};

decode_v2_element(182, Instance, <<>>) ->
    #v2_metric{instance = Instance};

decode_v2_element(183, Instance, <<>>) ->
    #v2_sequence_number{instance = Instance};

decode_v2_element(184, Instance, <<>>) ->
    #v2_apn_and_relative_capacity{instance = Instance};

decode_v2_element(185, Instance, <<>>) ->
    #v2_wlan_offloadability_indication{instance = Instance};

decode_v2_element(255, Instance, <<>>) ->
    #v2_private_extension{instance = Instance};

decode_v2_element(Tag, Instance, Value) ->
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
                       cs = M_cs,
                       data = M_data}) ->
    encode_v2_element(2, Instance, <<(enum_v2_v2_cause(M_v2_cause)):8/integer,
                                     0:5,
                                     M_pce:1,
                                     M_bce:1,
                                     M_cs:1,
                                     M_data/binary>>);

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
    encode_v2_element(71, Instance, <<M_apn/binary>>);

encode_v2_element(#v2_aggregate_maximum_bit_rate{
                       instance = Instance,
                       uplink = M_uplink,
                       downlink = M_downlink}) ->
    encode_v2_element(72, Instance, <<M_uplink:32,
                                      M_downlink:32>>);

encode_v2_element(#v2_eps_bearer_id{
                       instance = Instance,
                       eps_bearer_id = M_eps_bearer_id,
                       data = M_data}) ->
    encode_v2_element(73, Instance, <<0:4,
                                      M_eps_bearer_id:4,
                                      M_data/binary>>);

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
                       guaranteed_bit_rate_for_downlink = M_guaranteed_bit_rate_for_downlink,
                       data = M_data}) ->
    encode_v2_element(80, Instance, <<0:1,
                                      M_pci:1,
                                      M_pl:4,
                                      0:1,
                                      M_pvi:1,
                                      M_label:8,
                                      M_maximum_bit_rate_for_uplink:32,
                                      M_maximum_bit_rate_for_downlink:32,
                                      M_guaranteed_bit_rate_for_uplink:32,
                                      M_guaranteed_bit_rate_for_downlink:32,
                                      M_data/binary>>);

encode_v2_element(#v2_flow_quality_of_service{
                       instance = Instance}) ->
    encode_v2_element(81, Instance, <<>>);

encode_v2_element(#v2_rat_type{
                       instance = Instance,
                       rat_type = M_rat_type,
                       optional = M_optional}) ->
    encode_v2_element(82, Instance, <<(enum_v2_rat_type(M_rat_type)):8/integer,
                                      M_optional/binary>>);

encode_v2_element(#v2_serving_network{instance = Instance} = IE) ->
    encode_v2_element(83, Instance, encode_v2_mccmcn(IE));

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
                       instance = Instance}) ->
    encode_v2_element(94, Instance, <<>>);

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
                       pdn_type = M_pdn_type,
                       data = M_data}) ->
    encode_v2_element(99, Instance, <<0:4,
                                      (enum_v2_pdn_type(M_pdn_type)):4/integer,
                                      M_data/binary>>);

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
                       instance = Instance}) ->
    encode_v2_element(114, Instance, <<>>);

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
                       restriction_type_value = M_restriction_type_value,
                       data = M_data}) ->
    encode_v2_element(127, Instance, <<M_restriction_type_value:8,
                                       M_data/binary>>);

encode_v2_element(#v2_selection_mode{
                       instance = Instance,
                       mode = M_mode,
                       data = M_data}) ->
    encode_v2_element(128, Instance, <<0:6,
                                       M_mode:2,
                                       M_data/binary>>);

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
                       instance = Instance}) ->
    encode_v2_element(136, Instance, <<>>);

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
