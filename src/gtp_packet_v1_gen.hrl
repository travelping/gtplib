%% This file is auto-generated. DO NOT EDIT

msg_description(echo_request) -> <<"Echo Request">>;
msg_description(echo_response) -> <<"Echo Response">>;
msg_description(version_not_supported) -> <<"Version Not Supported">>;
msg_description(node_alive_request) -> <<"Node Alive Request">>;
msg_description(node_alive_response) -> <<"Node Alive Response">>;
msg_description(redirection_request) -> <<"Redirection Request">>;
msg_description(redirection_response) -> <<"Redirection Response">>;
msg_description(create_pdp_context_request) -> <<"Create PDP Context Request">>;
msg_description(create_pdp_context_response) -> <<"Create PDP Context Response">>;
msg_description(update_pdp_context_request) -> <<"Update PDP Context Request">>;
msg_description(update_pdp_context_response) -> <<"Update PDP Context Response">>;
msg_description(delete_pdp_context_request) -> <<"Delete PDP Context Request">>;
msg_description(delete_pdp_context_response) -> <<"Delete PDP Context Response">>;
msg_description(initiate_pdp_context_activation_request) -> <<"Initiate PDP Context Activation Request">>;
msg_description(initiate_pdp_context_activation_response) -> <<"Initiate PDP Context Activation Response">>;
msg_description(error_indication) -> <<"Error Indication">>;
msg_description(pdu_notification_request) -> <<"PDU Notification Request">>;
msg_description(pdu_notification_response) -> <<"PDU Notification Response">>;
msg_description(pdu_notification_reject_request) -> <<"PDU Notification Reject Request">>;
msg_description(pdu_notification_reject_response) -> <<"PDU Notification Reject Response">>;
msg_description(supported_extension_headers_notification) -> <<"Supported Extension Headers Notification">>;
msg_description(send_routeing_information_for_gprs_request) -> <<"Send Routeing Information for GPRS Request">>;
msg_description(send_routeing_information_for_gprs_response) -> <<"Send Routeing Information for GPRS Response">>;
msg_description(failure_report_request) -> <<"Failure Report Request">>;
msg_description(failure_report_response) -> <<"Failure Report Response">>;
msg_description(note_ms_gprs_present_request) -> <<"Note MS GPRS Present Request">>;
msg_description(note_ms_gprs_present_response) -> <<"Note MS GPRS Present Response">>;
msg_description(identification_request) -> <<"Identification Request">>;
msg_description(identification_response) -> <<"Identification Response">>;
msg_description(sgsn_context_request) -> <<"SGSN Context Request">>;
msg_description(sgsn_context_response) -> <<"SGSN Context Response">>;
msg_description(sgsn_context_acknowledge) -> <<"SGSN Context Acknowledge">>;
msg_description(forward_relocation_request) -> <<"Forward Relocation Request">>;
msg_description(forward_relocation_response) -> <<"Forward Relocation Response">>;
msg_description(forward_relocation_complete) -> <<"Forward Relocation Complete">>;
msg_description(relocation_cancel_request) -> <<"Relocation Cancel Request">>;
msg_description(relocation_cancel_response) -> <<"Relocation Cancel Response">>;
msg_description(forward_srns_context) -> <<"Forward SRNS Context">>;
msg_description(forward_relocation_complete_acknowledge) -> <<"Forward Relocation Complete Acknowledge">>;
msg_description(forward_srns_context_acknowledge) -> <<"Forward SRNS Context Acknowledge">>;
msg_description(ran_information_relay) -> <<"RAN Information Relay">>;
msg_description(mbms_notification_request) -> <<"MBMS Notification Request">>;
msg_description(mbms_notification_response) -> <<"MBMS Notification Response">>;
msg_description(mbms_notification_reject_request) -> <<"MBMS Notification Reject Request">>;
msg_description(mbms_notification_reject_response) -> <<"MBMS Notification Reject Response">>;
msg_description(create_mbms_context_request) -> <<"Create MBMS Context Request">>;
msg_description(create_mbms_context_response) -> <<"Create MBMS Context Response">>;
msg_description(update_mbms_context_request) -> <<"Update MBMS Context Request">>;
msg_description(update_mbms_context_response) -> <<"Update MBMS Context Response">>;
msg_description(delete_mbms_context_request) -> <<"Delete MBMS Context Request">>;
msg_description(delete_mbms_context_response) -> <<"Delete MBMS Context Response">>;
msg_description(mbms_registration_request) -> <<"MBMS Registration Request">>;
msg_description(mbms_registration_response) -> <<"MBMS Registration Response">>;
msg_description(mbms_de_registration_request) -> <<"MBMS De-Registration Request">>;
msg_description(mbms_de_registration_response) -> <<"MBMS De-Registration Response">>;
msg_description(mbms_session_start_request) -> <<"MBMS Session Start Request">>;
msg_description(mbms_session_start_response) -> <<"MBMS Session Start Response">>;
msg_description(mbms_session_stop_request) -> <<"MBMS Session Stop Request">>;
msg_description(mbms_session_stop_response) -> <<"MBMS Session Stop Response">>;
msg_description(mbms_session_update_request) -> <<"MBMS Session Update Request">>;
msg_description(mbms_session_update_response) -> <<"MBMS Session Update Response">>;
msg_description(ms_info_change_notification_request) -> <<"MS Info Change Notification Request">>;
msg_description(ms_info_change_notification_response) -> <<"MS Info Change Notification Response">>;
msg_description(data_record_transfer_request) -> <<"Data Record Transfer Request">>;
msg_description(data_record_transfer_response) -> <<"Data Record Transfer Response">>;
msg_description(end_marker) -> <<"End Marker">>;
msg_description(g_pdu) -> <<"G-PDU">>;
msg_description(X) -> io_lib:format("~p", [X]).

message_type_v1(echo_request) -> 1;
message_type_v1(echo_response) -> 2;
message_type_v1(version_not_supported) -> 3;
message_type_v1(node_alive_request) -> 4;
message_type_v1(node_alive_response) -> 5;
message_type_v1(redirection_request) -> 6;
message_type_v1(redirection_response) -> 7;
message_type_v1(create_pdp_context_request) -> 16;
message_type_v1(create_pdp_context_response) -> 17;
message_type_v1(update_pdp_context_request) -> 18;
message_type_v1(update_pdp_context_response) -> 19;
message_type_v1(delete_pdp_context_request) -> 20;
message_type_v1(delete_pdp_context_response) -> 21;
message_type_v1(initiate_pdp_context_activation_request) -> 22;
message_type_v1(initiate_pdp_context_activation_response) -> 23;
message_type_v1(error_indication) -> 26;
message_type_v1(pdu_notification_request) -> 27;
message_type_v1(pdu_notification_response) -> 28;
message_type_v1(pdu_notification_reject_request) -> 29;
message_type_v1(pdu_notification_reject_response) -> 30;
message_type_v1(supported_extension_headers_notification) -> 31;
message_type_v1(send_routeing_information_for_gprs_request) -> 32;
message_type_v1(send_routeing_information_for_gprs_response) -> 33;
message_type_v1(failure_report_request) -> 34;
message_type_v1(failure_report_response) -> 35;
message_type_v1(note_ms_gprs_present_request) -> 36;
message_type_v1(note_ms_gprs_present_response) -> 37;
message_type_v1(identification_request) -> 48;
message_type_v1(identification_response) -> 49;
message_type_v1(sgsn_context_request) -> 50;
message_type_v1(sgsn_context_response) -> 51;
message_type_v1(sgsn_context_acknowledge) -> 52;
message_type_v1(forward_relocation_request) -> 53;
message_type_v1(forward_relocation_response) -> 54;
message_type_v1(forward_relocation_complete) -> 55;
message_type_v1(relocation_cancel_request) -> 56;
message_type_v1(relocation_cancel_response) -> 57;
message_type_v1(forward_srns_context) -> 58;
message_type_v1(forward_relocation_complete_acknowledge) -> 59;
message_type_v1(forward_srns_context_acknowledge) -> 60;
message_type_v1(ran_information_relay) -> 70;
message_type_v1(mbms_notification_request) -> 96;
message_type_v1(mbms_notification_response) -> 97;
message_type_v1(mbms_notification_reject_request) -> 98;
message_type_v1(mbms_notification_reject_response) -> 99;
message_type_v1(create_mbms_context_request) -> 100;
message_type_v1(create_mbms_context_response) -> 101;
message_type_v1(update_mbms_context_request) -> 102;
message_type_v1(update_mbms_context_response) -> 103;
message_type_v1(delete_mbms_context_request) -> 104;
message_type_v1(delete_mbms_context_response) -> 105;
message_type_v1(mbms_registration_request) -> 112;
message_type_v1(mbms_registration_response) -> 113;
message_type_v1(mbms_de_registration_request) -> 114;
message_type_v1(mbms_de_registration_response) -> 115;
message_type_v1(mbms_session_start_request) -> 116;
message_type_v1(mbms_session_start_response) -> 117;
message_type_v1(mbms_session_stop_request) -> 118;
message_type_v1(mbms_session_stop_response) -> 119;
message_type_v1(mbms_session_update_request) -> 120;
message_type_v1(mbms_session_update_response) -> 121;
message_type_v1(ms_info_change_notification_request) -> 128;
message_type_v1(ms_info_change_notification_response) -> 129;
message_type_v1(data_record_transfer_request) -> 240;
message_type_v1(data_record_transfer_response) -> 241;
message_type_v1(end_marker) -> 254;
message_type_v1(g_pdu) -> 255;
message_type_v1(1) -> echo_request;
message_type_v1(2) -> echo_response;
message_type_v1(3) -> version_not_supported;
message_type_v1(4) -> node_alive_request;
message_type_v1(5) -> node_alive_response;
message_type_v1(6) -> redirection_request;
message_type_v1(7) -> redirection_response;
message_type_v1(16) -> create_pdp_context_request;
message_type_v1(17) -> create_pdp_context_response;
message_type_v1(18) -> update_pdp_context_request;
message_type_v1(19) -> update_pdp_context_response;
message_type_v1(20) -> delete_pdp_context_request;
message_type_v1(21) -> delete_pdp_context_response;
message_type_v1(22) -> initiate_pdp_context_activation_request;
message_type_v1(23) -> initiate_pdp_context_activation_response;
message_type_v1(26) -> error_indication;
message_type_v1(27) -> pdu_notification_request;
message_type_v1(28) -> pdu_notification_response;
message_type_v1(29) -> pdu_notification_reject_request;
message_type_v1(30) -> pdu_notification_reject_response;
message_type_v1(31) -> supported_extension_headers_notification;
message_type_v1(32) -> send_routeing_information_for_gprs_request;
message_type_v1(33) -> send_routeing_information_for_gprs_response;
message_type_v1(34) -> failure_report_request;
message_type_v1(35) -> failure_report_response;
message_type_v1(36) -> note_ms_gprs_present_request;
message_type_v1(37) -> note_ms_gprs_present_response;
message_type_v1(48) -> identification_request;
message_type_v1(49) -> identification_response;
message_type_v1(50) -> sgsn_context_request;
message_type_v1(51) -> sgsn_context_response;
message_type_v1(52) -> sgsn_context_acknowledge;
message_type_v1(53) -> forward_relocation_request;
message_type_v1(54) -> forward_relocation_response;
message_type_v1(55) -> forward_relocation_complete;
message_type_v1(56) -> relocation_cancel_request;
message_type_v1(57) -> relocation_cancel_response;
message_type_v1(58) -> forward_srns_context;
message_type_v1(59) -> forward_relocation_complete_acknowledge;
message_type_v1(60) -> forward_srns_context_acknowledge;
message_type_v1(70) -> ran_information_relay;
message_type_v1(96) -> mbms_notification_request;
message_type_v1(97) -> mbms_notification_response;
message_type_v1(98) -> mbms_notification_reject_request;
message_type_v1(99) -> mbms_notification_reject_response;
message_type_v1(100) -> create_mbms_context_request;
message_type_v1(101) -> create_mbms_context_response;
message_type_v1(102) -> update_mbms_context_request;
message_type_v1(103) -> update_mbms_context_response;
message_type_v1(104) -> delete_mbms_context_request;
message_type_v1(105) -> delete_mbms_context_response;
message_type_v1(112) -> mbms_registration_request;
message_type_v1(113) -> mbms_registration_response;
message_type_v1(114) -> mbms_de_registration_request;
message_type_v1(115) -> mbms_de_registration_response;
message_type_v1(116) -> mbms_session_start_request;
message_type_v1(117) -> mbms_session_start_response;
message_type_v1(118) -> mbms_session_stop_request;
message_type_v1(119) -> mbms_session_stop_response;
message_type_v1(120) -> mbms_session_update_request;
message_type_v1(121) -> mbms_session_update_response;
message_type_v1(128) -> ms_info_change_notification_request;
message_type_v1(129) -> ms_info_change_notification_response;
message_type_v1(240) -> data_record_transfer_request;
message_type_v1(241) -> data_record_transfer_response;
message_type_v1(254) -> end_marker;
message_type_v1(255) -> g_pdu;
message_type_v1({Vendor, Type}) when is_integer(Vendor), is_integer(Type) -> {Vendor, Type}.

enum_selection_mode_value(ms_or_network_provided_apn__subscribed_verified) -> 0;
enum_selection_mode_value(ms_provided_apn__subscription_not_verified) -> 1;
enum_selection_mode_value(network_provided_apn__subscription_not_verified) -> 2;
enum_selection_mode_value(0) -> ms_or_network_provided_apn__subscribed_verified;
enum_selection_mode_value(1) -> ms_provided_apn__subscription_not_verified;
enum_selection_mode_value(2) -> network_provided_apn__subscription_not_verified.

enum_required(no) -> 0;
enum_required(yes) -> 1;
enum_required(0) -> no;
enum_required(1) -> yes.

enum_value(request_imsi) -> 0;
enum_value(request_imei) -> 1;
enum_value(request_imsi_and_imei) -> 2;
enum_value(no_identity_needed) -> 3;
enum_value(ms_refuses) -> 4;
enum_value(ms_is_not_gprs_responding) -> 5;
enum_value(reactivation_requested) -> 6;
enum_value(pdp_address_inactivity_timer_expires) -> 7;
enum_value(network_failure) -> 8;
enum_value(qos_parameter_mismatch) -> 9;
enum_value(request_accepted) -> 128;
enum_value(new_pdp_type_due_to_network_preference) -> 129;
enum_value(new_pdp_type_due_to_single_address_bearer_only) -> 130;
enum_value(non_existent) -> 192;
enum_value(invalid_message_format) -> 193;
enum_value(imsi_imei_not_known) -> 194;
enum_value(ms_is_gprs_detached) -> 195;
enum_value(ms_is_not_gprs_responding) -> 196;
enum_value(ms_refuses) -> 197;
enum_value(version_not_supported) -> 198;
enum_value(no_resources_available) -> 199;
enum_value(service_not_supported) -> 200;
enum_value(mandatory_ie_incorrect) -> 201;
enum_value(mandatory_ie_missing) -> 202;
enum_value(optional_ie_incorrect) -> 203;
enum_value(system_failure) -> 204;
enum_value(roaming_restriction) -> 205;
enum_value(p_tmsi_signature_mismatch) -> 206;
enum_value(gprs_connection_suspended) -> 207;
enum_value(authentication_failure) -> 208;
enum_value(user_authentication_failed) -> 209;
enum_value(context_not_found) -> 210;
enum_value(all_dynamic_pdp_addresses_are_occupied) -> 211;
enum_value(no_memory_is_available) -> 212;
enum_value(relocation_failure) -> 213;
enum_value(unknown_mandatory_extension_header) -> 214;
enum_value(semantic_error_in_the_tft_operation) -> 215;
enum_value(syntactic_error_in_the_tft_operation) -> 216;
enum_value(semantic_errors_in_packet_filter) -> 217;
enum_value(syntactic_errors_in_packet_filter) -> 218;
enum_value(missing_or_unknown_apn) -> 219;
enum_value(unknown_pdp_address_or_pdp_type) -> 220;
enum_value(pdp_context_without_tft_already_activated) -> 221;
enum_value(apn_access_denied___no_subscription) -> 222;
enum_value(apn_restriction_type_incompatibility_with_currently_active_pdp_contexts) -> 223;
enum_value(ms_mbms_capabilities_insufficient) -> 224;
enum_value(invalid_correlation_id) -> 225;
enum_value(mbms_bearer_context_superseded) -> 226;
enum_value(bearer_control_mode_violation) -> 227;
enum_value(collision_with_network_initiated_request) -> 228;
enum_value(apn_congestion) -> 229;
enum_value(bearer_handling_not_supported) -> 230;
enum_value(target_access_restricted_for_the_subscriber) -> 231;
enum_value(0) -> request_imsi;
enum_value(1) -> request_imei;
enum_value(2) -> request_imsi_and_imei;
enum_value(3) -> no_identity_needed;
enum_value(4) -> ms_refuses;
enum_value(5) -> ms_is_not_gprs_responding;
enum_value(6) -> reactivation_requested;
enum_value(7) -> pdp_address_inactivity_timer_expires;
enum_value(8) -> network_failure;
enum_value(9) -> qos_parameter_mismatch;
enum_value(128) -> request_accepted;
enum_value(129) -> new_pdp_type_due_to_network_preference;
enum_value(130) -> new_pdp_type_due_to_single_address_bearer_only;
enum_value(192) -> non_existent;
enum_value(193) -> invalid_message_format;
enum_value(194) -> imsi_imei_not_known;
enum_value(195) -> ms_is_gprs_detached;
enum_value(196) -> ms_is_not_gprs_responding;
enum_value(197) -> ms_refuses;
enum_value(198) -> version_not_supported;
enum_value(199) -> no_resources_available;
enum_value(200) -> service_not_supported;
enum_value(201) -> mandatory_ie_incorrect;
enum_value(202) -> mandatory_ie_missing;
enum_value(203) -> optional_ie_incorrect;
enum_value(204) -> system_failure;
enum_value(205) -> roaming_restriction;
enum_value(206) -> p_tmsi_signature_mismatch;
enum_value(207) -> gprs_connection_suspended;
enum_value(208) -> authentication_failure;
enum_value(209) -> user_authentication_failed;
enum_value(210) -> context_not_found;
enum_value(211) -> all_dynamic_pdp_addresses_are_occupied;
enum_value(212) -> no_memory_is_available;
enum_value(213) -> relocation_failure;
enum_value(214) -> unknown_mandatory_extension_header;
enum_value(215) -> semantic_error_in_the_tft_operation;
enum_value(216) -> syntactic_error_in_the_tft_operation;
enum_value(217) -> semantic_errors_in_packet_filter;
enum_value(218) -> syntactic_errors_in_packet_filter;
enum_value(219) -> missing_or_unknown_apn;
enum_value(220) -> unknown_pdp_address_or_pdp_type;
enum_value(221) -> pdp_context_without_tft_already_activated;
enum_value(222) -> apn_access_denied___no_subscription;
enum_value(223) -> apn_restriction_type_incompatibility_with_currently_active_pdp_contexts;
enum_value(224) -> ms_mbms_capabilities_insufficient;
enum_value(225) -> invalid_correlation_id;
enum_value(226) -> mbms_bearer_context_superseded;
enum_value(227) -> bearer_control_mode_violation;
enum_value(228) -> collision_with_network_initiated_request;
enum_value(229) -> apn_congestion;
enum_value(230) -> bearer_handling_not_supported;
enum_value(231) -> target_access_restricted_for_the_subscriber.

decode_v1_element(1, <<M_value:8/integer>>) ->
    #cause{value = enum_value(M_value)};

decode_v1_element(2, <<M_imsi:64/bits>>) ->
    #international_mobile_subscriber_identity{imsi = decode_tbcd(M_imsi)};

decode_v1_element(3, <<M_mcc:24/bits,
                       M_mcn:24/bits,
                       M_lac:16/integer,
                       M_rac:8/integer>>) ->
    #routeing_area_identity{mcc = decode_tbcd(M_mcc),
                            mcn = decode_tbcd(M_mcn),
                            lac = M_lac,
                            rac = M_rac};

decode_v1_element(4, <<M_tlli:4/bytes>>) ->
    #temporary_logical_link_identity{tlli = M_tlli};

decode_v1_element(5, <<M_p_tmsi:4/bytes>>) ->
    #packet_tmsi{p_tmsi = M_p_tmsi};

decode_v1_element(8, <<_:7,
                       M_required:1/integer>>) ->
    #reordering_required{required = enum_required(M_required)};

decode_v1_element(9, <<M_rand:16/bytes,
                       M_sres:4/bytes,
                       M_kc:8/bytes>>) ->
    #authentication_triplet{rand = M_rand,
                            sres = M_sres,
                            kc = M_kc};

decode_v1_element(11, <<>>) ->
    #map_cause{};

decode_v1_element(12, <<>>) ->
    #p_tmsi_signature{};

decode_v1_element(13, <<>>) ->
    #ms_validated{};

decode_v1_element(14, <<M_restart_counter:8/integer>>) ->
    #recovery{restart_counter = M_restart_counter};

decode_v1_element(15, <<_:6,
                        M_selection_mode_value:2/integer>>) ->
    #selection_mode{selection_mode_value = enum_selection_mode_value(M_selection_mode_value)};

decode_v1_element(16, <<M_tei:32/integer>>) ->
    #tunnel_endpoint_identifier_data_i{tei = M_tei};

decode_v1_element(17, <<M_tei:32/integer>>) ->
    #tunnel_endpoint_identifier_control_plane{tei = M_tei};

decode_v1_element(18, <<_:4,
                        M_nsapi:4/integer,
                        M_tei:32/integer>>) ->
    #tunnel_endpoint_identifier_data_ii{nsapi = M_nsapi,
                                        tei = M_tei};

decode_v1_element(19, <<_:7,
                        M_value:1/integer>>) ->
    #teardown_ind{value = M_value};

decode_v1_element(20, <<_:4,
                        M_nsapi:4/integer>>) ->
    #nsapi{nsapi = M_nsapi};

decode_v1_element(21, <<>>) ->
    #ranap_cause{};

decode_v1_element(22, <<>>) ->
    #rab_context{};

decode_v1_element(23, <<>>) ->
    #radio_priority_sms{};

decode_v1_element(24, <<>>) ->
    #radio_priority{};

decode_v1_element(25, <<>>) ->
    #packet_flow_id{};

decode_v1_element(26, <<M_value:2/bytes>>) ->
    #charging_characteristics{value = M_value};

decode_v1_element(27, <<>>) ->
    #trace_reference{};

decode_v1_element(28, <<>>) ->
    #trace_type{};

decode_v1_element(29, <<>>) ->
    #ms_not_reachable_reason{};

decode_v1_element(127, <<M_id:4/bytes>>) ->
    #charging_id{id = M_id};

decode_v1_element(128, <<15:4,
                         M_pdp_type_organization:4/integer,
                         M_pdp_type_number:8/integer,
                         M_pdp_address/binary>>) ->
    #end_user_address{pdp_type_organization = M_pdp_type_organization,
                      pdp_type_number = M_pdp_type_number,
                      pdp_address = M_pdp_address};

decode_v1_element(129, <<_:4,
                         M_cksn:4/integer,
                         1:2,
                         M_no_of_vectors:3/integer,
                         M_used_cipher:3/integer,
                         M_kc:8/bytes,
                         M_tripple_Rest/binary>>) ->
    M_tripple_size = M_no_of_vectors * 8,
    <<M_tripple:M_tripple_size/bytes,
      M_drx_parameter:2/bytes,
      M_ms_network_capability_length:8/integer,
      M_ms_network_capability_Rest/binary>> = M_tripple_Rest,
    M_ms_network_capability_size = M_ms_network_capability_length * 1,
    <<M_ms_network_capability:M_ms_network_capability_size/bytes,
      M_container_length:16/integer,
      M_container_Rest/binary>> = M_ms_network_capability_Rest,
    M_container_size = M_container_length * 1,
    <<M_container:M_container_size/bytes>> = M_container_Rest,
    #mm_context_gsm{cksn = M_cksn,
                    no_of_vectors = M_no_of_vectors,
                    used_cipher = M_used_cipher,
                    kc = M_kc,
                    tripple = [X || <<X:8/bytes>> <= M_tripple],
                    drx_parameter = M_drx_parameter,
                    ms_network_capability_length = M_ms_network_capability_length,
                    ms_network_capability = [X || <<X:1/bytes>> <= M_ms_network_capability],
                    container_length = M_container_length,
                    container = [X || <<X:1/bytes>> <= M_container]};

decode_v1_element(129, <<_:4,
                         M_ksi:4/integer,
                         2:2,
                         M_no_of_vectors:3/integer,
                         _:3,
                         M_ck:16/bytes,
                         M_ik:16/bytes,
                         M_quintuplet_length:16/integer,
                         M_quintuplet_Rest/binary>>) ->
    M_quintuplet_size = M_quintuplet_length * 1,
    <<M_quintuplet:M_quintuplet_size/bytes,
      M_drx_parameter:2/bytes,
      M_ms_network_capability_length:8/integer,
      M_ms_network_capability_Rest/binary>> = M_quintuplet_Rest,
    M_ms_network_capability_size = M_ms_network_capability_length * 1,
    <<M_ms_network_capability:M_ms_network_capability_size/bytes,
      M_container_length:16/integer,
      M_container_Rest/binary>> = M_ms_network_capability_Rest,
    M_container_size = M_container_length * 1,
    <<M_container:M_container_size/bytes>> = M_container_Rest,
    #mm_context_umts{ksi = M_ksi,
                     no_of_vectors = M_no_of_vectors,
                     ck = M_ck,
                     ik = M_ik,
                     quintuplet_length = M_quintuplet_length,
                     quintuplet = [X || <<X:1/bytes>> <= M_quintuplet],
                     drx_parameter = M_drx_parameter,
                     ms_network_capability_length = M_ms_network_capability_length,
                     ms_network_capability = [X || <<X:1/bytes>> <= M_ms_network_capability],
                     container_length = M_container_length,
                     container = [X || <<X:1/bytes>> <= M_container]};

decode_v1_element(129, <<_:4,
                         M_cksn:4/integer,
                         3:2,
                         M_no_of_vectors:3/integer,
                         M_used_cipher:3/integer,
                         M_kc:8/bytes,
                         M_quintuplet_length:16/integer,
                         M_quintuplet_Rest/binary>>) ->
    M_quintuplet_size = M_quintuplet_length * 1,
    <<M_quintuplet:M_quintuplet_size/bytes,
      M_drx_parameter:2/bytes,
      M_ms_network_capability_length:8/integer,
      M_ms_network_capability_Rest/binary>> = M_quintuplet_Rest,
    M_ms_network_capability_size = M_ms_network_capability_length * 1,
    <<M_ms_network_capability:M_ms_network_capability_size/bytes,
      M_container_length:16/integer,
      M_container_Rest/binary>> = M_ms_network_capability_Rest,
    M_container_size = M_container_length * 1,
    <<M_container:M_container_size/bytes>> = M_container_Rest,
    #mm_context_gsm_and_umts{cksn = M_cksn,
                             no_of_vectors = M_no_of_vectors,
                             used_cipher = M_used_cipher,
                             kc = M_kc,
                             quintuplet_length = M_quintuplet_length,
                             quintuplet = [X || <<X:1/bytes>> <= M_quintuplet],
                             drx_parameter = M_drx_parameter,
                             ms_network_capability_length = M_ms_network_capability_length,
                             ms_network_capability = [X || <<X:1/bytes>> <= M_ms_network_capability],
                             container_length = M_container_length,
                             container = [X || <<X:1/bytes>> <= M_container]};

decode_v1_element(129, <<_:4,
                         M_ksi:4/integer,
                         0:2,
                         M_no_of_vectors:3/integer,
                         M_used_cipher:3/integer,
                         M_ck:16/bytes,
                         M_ik:16/bytes,
                         M_quintuplet_length:16/integer,
                         M_quintuplet_Rest/binary>>) ->
    M_quintuplet_size = M_quintuplet_length * 1,
    <<M_quintuplet:M_quintuplet_size/bytes,
      M_drx_parameter:2/bytes,
      M_ms_network_capability_length:8/integer,
      M_ms_network_capability_Rest/binary>> = M_quintuplet_Rest,
    M_ms_network_capability_size = M_ms_network_capability_length * 1,
    <<M_ms_network_capability:M_ms_network_capability_size/bytes,
      M_container_length:16/integer,
      M_container_Rest/binary>> = M_ms_network_capability_Rest,
    M_container_size = M_container_length * 1,
    <<M_container:M_container_size/bytes>> = M_container_Rest,
    #mm_context_umts_and_used_cipher{ksi = M_ksi,
                                     no_of_vectors = M_no_of_vectors,
                                     used_cipher = M_used_cipher,
                                     ck = M_ck,
                                     ik = M_ik,
                                     quintuplet_length = M_quintuplet_length,
                                     quintuplet = [X || <<X:1/bytes>> <= M_quintuplet],
                                     drx_parameter = M_drx_parameter,
                                     ms_network_capability_length = M_ms_network_capability_length,
                                     ms_network_capability = [X || <<X:1/bytes>> <= M_ms_network_capability],
                                     container_length = M_container_length,
                                     container = [X || <<X:1/bytes>> <= M_container]};

decode_v1_element(130, <<>>) ->
    #pdp_context{};

decode_v1_element(131, <<M_apn/binary>>) ->
    #access_point_name{apn = M_apn};

decode_v1_element(132, <<M_config/binary>>) ->
    #protocol_configuration_options{config = decode_protocol_config_opts(M_config)};

decode_v1_element(133, <<M_address/binary>>) ->
    #gsn_address{address = M_address};

decode_v1_element(134, <<M_msisdn/binary>>) ->
    #ms_international_pstn_isdn_number{msisdn = decode_isdn_address_string(M_msisdn)};

decode_v1_element(135, <<M_priority:8/integer,
                         M_data/binary>>) ->
    #quality_of_service_profile{priority = M_priority,
                                data = M_data};

decode_v1_element(136, <<>>) ->
    #authentication_quintuplet{};

decode_v1_element(137, <<>>) ->
    #traffic_flow_template{};

decode_v1_element(138, <<>>) ->
    #target_identification{};

decode_v1_element(139, <<>>) ->
    #utran_transparent_container{};

decode_v1_element(140, <<>>) ->
    #rab_setup_information{};

decode_v1_element(141, <<>>) ->
    #extension_header_type_list{};

decode_v1_element(142, <<>>) ->
    #trigger_id{};

decode_v1_element(143, <<>>) ->
    #omc_identity{};

decode_v1_element(144, <<>>) ->
    #ran_transparent_container{};

decode_v1_element(145, <<>>) ->
    #pdp_context_prioritization{};

decode_v1_element(146, <<>>) ->
    #additional_rab_setup_information{};

decode_v1_element(147, <<>>) ->
    #sgsn_number{};

decode_v1_element(148, <<>>) ->
    #common_flags{};

decode_v1_element(149, <<>>) ->
    #apn_restriction{};

decode_v1_element(150, <<>>) ->
    #radio_priority_lcs{};

decode_v1_element(151, <<>>) ->
    #rat_type{};

decode_v1_element(152, <<>>) ->
    #user_location_information{};

decode_v1_element(153, <<>>) ->
    #ms_time_zone{};

decode_v1_element(154, <<>>) ->
    #imei{};

decode_v1_element(155, <<>>) ->
    #camel_charging_information_container{};

decode_v1_element(156, <<>>) ->
    #mbms_ue_context{};

decode_v1_element(157, <<>>) ->
    #temporary_mobile_group_identity{};

decode_v1_element(158, <<>>) ->
    #rim_routing_address{};

decode_v1_element(159, <<>>) ->
    #mbms_protocol_configuration_options{};

decode_v1_element(160, <<>>) ->
    #mbms_service_area{};

decode_v1_element(161, <<>>) ->
    #source_rnc_pdcp_context_info{};

decode_v1_element(162, <<>>) ->
    #additional_trace_info{};

decode_v1_element(163, <<>>) ->
    #hop_counter{};

decode_v1_element(164, <<>>) ->
    #selected_plmn_id{};

decode_v1_element(165, <<>>) ->
    #mbms_session_identifier{};

decode_v1_element(166, <<>>) ->
    #mbms_2g_3g_indicator{};

decode_v1_element(167, <<>>) ->
    #enhanced_nsapi{};

decode_v1_element(168, <<>>) ->
    #mbms_session_duration{};

decode_v1_element(169, <<>>) ->
    #additional_mbms_trace_info{};

decode_v1_element(170, <<>>) ->
    #mbms_session_repetition_number{};

decode_v1_element(171, <<>>) ->
    #mbms_time_to_data_transfer{};

decode_v1_element(173, <<>>) ->
    #bss_container{};

decode_v1_element(174, <<>>) ->
    #cell_identification{};

decode_v1_element(175, <<>>) ->
    #pdu_numbers{};

decode_v1_element(176, <<>>) ->
    #bssgp_cause{};

decode_v1_element(177, <<>>) ->
    #required_mbms_bearer_capabilities{};

decode_v1_element(178, <<>>) ->
    #rim_routing_address_discriminator{};

decode_v1_element(179, <<>>) ->
    #list_of_set_up_pfcs{};

decode_v1_element(180, <<>>) ->
    #ps_handover_xid_parameters{};

decode_v1_element(181, <<>>) ->
    #ms_info_change_reporting_action{};

decode_v1_element(182, <<>>) ->
    #direct_tunnel_flags{};

decode_v1_element(183, <<>>) ->
    #correlation_id{};

decode_v1_element(184, <<>>) ->
    #bearer_control_mode{};

decode_v1_element(185, <<>>) ->
    #mbms_flow_identifier{};

decode_v1_element(186, <<>>) ->
    #mbms_ip_multicast_distribution{};

decode_v1_element(187, <<>>) ->
    #mbms_distribution_acknowledgement{};

decode_v1_element(188, <<>>) ->
    #reliable_inter_rat_handover_info{};

decode_v1_element(189, <<>>) ->
    #rfsp_index{};

decode_v1_element(190, <<>>) ->
    #fully_qualified_domain_name{};

decode_v1_element(191, <<>>) ->
    #evolved_allocation_retention_priority_i{};

decode_v1_element(192, <<>>) ->
    #evolved_allocation_retention_priority_ii{};

decode_v1_element(193, <<>>) ->
    #extended_common_flags{};

decode_v1_element(194, <<>>) ->
    #user_csg_information{};

decode_v1_element(195, <<>>) ->
    #csg_information_reporting_action{};

decode_v1_element(196, <<>>) ->
    #csg_id{};

decode_v1_element(197, <<>>) ->
    #csg_membership_indication{};

decode_v1_element(198, <<>>) ->
    #aggregate_maximum_bit_rate{};

decode_v1_element(199, <<>>) ->
    #ue_network_capability{};

decode_v1_element(200, <<>>) ->
    #ue_ambr{};

decode_v1_element(201, <<>>) ->
    #apn_ambr_with_nsapi{};

decode_v1_element(202, <<>>) ->
    #ggsn_back_off_time{};

decode_v1_element(203, <<>>) ->
    #signalling_priority_indication{};

decode_v1_element(204, <<>>) ->
    #signalling_priority_indication_with_nsapi{};

decode_v1_element(205, <<>>) ->
    #higher_bitrates_than_16_mbps_flag{};

decode_v1_element(207, <<>>) ->
    #additional_mm_context_for_srvcc{};

decode_v1_element(208, <<>>) ->
    #additional_flags_for_srvcc{};

decode_v1_element(209, <<>>) ->
    #stn_sr{};

decode_v1_element(210, <<>>) ->
    #c_msisdn{};

decode_v1_element(211, <<>>) ->
    #extended_ranap_cause{};

decode_v1_element(212, <<>>) ->
    #enodeb_id{};

decode_v1_element(213, <<>>) ->
    #selection_mode_with_nsapi{};

decode_v1_element(214, <<>>) ->
    #uli_timestamp{};

decode_v1_element(215, <<>>) ->
    #local_home_network_id_with_nsapi{};

decode_v1_element(216, <<>>) ->
    #cn_operator_selection_entity{};

decode_v1_element(251, <<>>) ->
    #charging_gateway_address{};

decode_v1_element(255, <<>>) ->
    #private_extension{};

decode_v1_element(Tag, Value) ->
        {Tag, Value}.

decode_v1(<<>>, IEs) ->
    lists:reverse(IEs);
decode_v1(<<1, Data:1/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(1, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<2, Data:8/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(2, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<3, Data:6/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(3, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<4, Data:4/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(4, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<5, Data:4/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(5, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<8, Data:1/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(8, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<9, Data:28/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(9, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<11, Data:1/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(11, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<12, Data:3/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(12, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<13, Data:1/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(13, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<14, Data:1/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(14, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<15, Data:1/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(15, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<16, Data:4/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(16, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<17, Data:4/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(17, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<18, Data:5/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(18, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<19, Data:1/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(19, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<20, Data:1/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(20, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<21, Data:1/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(21, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<22, Data:9/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(22, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<23, Data:1/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(23, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<24, Data:1/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(24, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<25, Data:2/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(25, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<26, Data:2/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(26, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<27, Data:2/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(27, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<28, Data:2/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(28, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<29, Data:1/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(29, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<127, Data:4/bytes, Next/binary>>, IEs) ->
    IE = decode_v1_element(127, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<Id, Length:16/integer, Rest/binary>>, IEs) when Id > 127 ->
    <<Data:Length/binary, Next/binary>> = Rest,
    IE = decode_v1_element(Id, Data),
    decode_v1(Next, [IE|IEs]);
decode_v1(<<Id, Rest/binary>>, IEs) ->
    IE = {Id, Rest},
    decode_v1(<<>>, [IE|IEs]).


encode_v1_element(#cause{
                       value = M_value}) ->
    encode_v1_element(1, <<(enum_value(M_value)):8/integer>>);

encode_v1_element(#international_mobile_subscriber_identity{
                       imsi = M_imsi}) ->
    encode_v1_element(2, <<(encode_tbcd(M_imsi)):64/bits>>);

encode_v1_element(#routeing_area_identity{
                       mcc = M_mcc,
                       mcn = M_mcn,
                       lac = M_lac,
                       rac = M_rac}) ->
    encode_v1_element(3, <<(encode_tbcd(M_mcc)):24/bits,
                           (encode_tbcd(M_mcn)):24/bits,
                           M_lac:16,
                           M_rac:8>>);

encode_v1_element(#temporary_logical_link_identity{
                       tlli = M_tlli}) ->
    encode_v1_element(4, <<M_tlli:4/bytes>>);

encode_v1_element(#packet_tmsi{
                       p_tmsi = M_p_tmsi}) ->
    encode_v1_element(5, <<M_p_tmsi:4/bytes>>);

encode_v1_element(#reordering_required{
                       required = M_required}) ->
    encode_v1_element(8, <<0:7,
                           (enum_required(M_required)):1/integer>>);

encode_v1_element(#authentication_triplet{
                       rand = M_rand,
                       sres = M_sres,
                       kc = M_kc}) ->
    encode_v1_element(9, <<M_rand:16/bytes,
                           M_sres:4/bytes,
                           M_kc:8/bytes>>);

encode_v1_element(#map_cause{
                       }) ->
    encode_v1_element(11, <<>>);

encode_v1_element(#p_tmsi_signature{
                       }) ->
    encode_v1_element(12, <<>>);

encode_v1_element(#ms_validated{
                       }) ->
    encode_v1_element(13, <<>>);

encode_v1_element(#recovery{
                       restart_counter = M_restart_counter}) ->
    encode_v1_element(14, <<M_restart_counter:8>>);

encode_v1_element(#selection_mode{
                       selection_mode_value = M_selection_mode_value}) ->
    encode_v1_element(15, <<0:6,
                            (enum_selection_mode_value(M_selection_mode_value)):2/integer>>);

encode_v1_element(#tunnel_endpoint_identifier_data_i{
                       tei = M_tei}) ->
    encode_v1_element(16, <<M_tei:32>>);

encode_v1_element(#tunnel_endpoint_identifier_control_plane{
                       tei = M_tei}) ->
    encode_v1_element(17, <<M_tei:32>>);

encode_v1_element(#tunnel_endpoint_identifier_data_ii{
                       nsapi = M_nsapi,
                       tei = M_tei}) ->
    encode_v1_element(18, <<0:4,
                            M_nsapi:4,
                            M_tei:32>>);

encode_v1_element(#teardown_ind{
                       value = M_value}) ->
    encode_v1_element(19, <<0:7,
                            M_value:1>>);

encode_v1_element(#nsapi{
                       nsapi = M_nsapi}) ->
    encode_v1_element(20, <<0:4,
                            M_nsapi:4>>);

encode_v1_element(#ranap_cause{
                       }) ->
    encode_v1_element(21, <<>>);

encode_v1_element(#rab_context{
                       }) ->
    encode_v1_element(22, <<>>);

encode_v1_element(#radio_priority_sms{
                       }) ->
    encode_v1_element(23, <<>>);

encode_v1_element(#radio_priority{
                       }) ->
    encode_v1_element(24, <<>>);

encode_v1_element(#packet_flow_id{
                       }) ->
    encode_v1_element(25, <<>>);

encode_v1_element(#charging_characteristics{
                       value = M_value}) ->
    encode_v1_element(26, <<M_value:2/bytes>>);

encode_v1_element(#trace_reference{
                       }) ->
    encode_v1_element(27, <<>>);

encode_v1_element(#trace_type{
                       }) ->
    encode_v1_element(28, <<>>);

encode_v1_element(#ms_not_reachable_reason{
                       }) ->
    encode_v1_element(29, <<>>);

encode_v1_element(#charging_id{
                       id = M_id}) ->
    encode_v1_element(127, <<M_id:4/bytes>>);

encode_v1_element(#end_user_address{
                       pdp_type_organization = M_pdp_type_organization,
                       pdp_type_number = M_pdp_type_number,
                       pdp_address = M_pdp_address}) ->
    encode_v1_element(128, <<15:4,
                             M_pdp_type_organization:4,
                             M_pdp_type_number:8,
                             M_pdp_address/binary>>);

encode_v1_element(#mm_context_gsm{
                       cksn = M_cksn,
                       no_of_vectors = M_no_of_vectors,
                       used_cipher = M_used_cipher,
                       kc = M_kc,
                       tripple = M_tripple,
                       drx_parameter = M_drx_parameter,
                       ms_network_capability_length = M_ms_network_capability_length,
                       ms_network_capability = M_ms_network_capability,
                       container_length = M_container_length,
                       container = M_container}) ->
    encode_v1_element(129, <<0:4,
                             M_cksn:4,
                             1:2,
                             M_no_of_vectors:3,
                             M_used_cipher:3,
                             M_kc:8/bytes,
                             (length(M_tripple)):8/integer, (<< <<X/binary>> || X <- M_tripple>>)/binary,
                             M_drx_parameter:2/bytes,
                             M_ms_network_capability_length:8,
                             (length(M_ms_network_capability)):1/integer, (<< <<X/binary>> || X <- M_ms_network_capability>>)/binary,
                             M_container_length:16,
                             (length(M_container)):1/integer, (<< <<X/binary>> || X <- M_container>>)/binary>>);

encode_v1_element(#mm_context_umts{
                       ksi = M_ksi,
                       no_of_vectors = M_no_of_vectors,
                       ck = M_ck,
                       ik = M_ik,
                       quintuplet_length = M_quintuplet_length,
                       quintuplet = M_quintuplet,
                       drx_parameter = M_drx_parameter,
                       ms_network_capability_length = M_ms_network_capability_length,
                       ms_network_capability = M_ms_network_capability,
                       container_length = M_container_length,
                       container = M_container}) ->
    encode_v1_element(129, <<0:4,
                             M_ksi:4,
                             2:2,
                             M_no_of_vectors:3,
                             0:3,
                             M_ck:16/bytes,
                             M_ik:16/bytes,
                             M_quintuplet_length:16,
                             (length(M_quintuplet)):1/integer, (<< <<X/binary>> || X <- M_quintuplet>>)/binary,
                             M_drx_parameter:2/bytes,
                             M_ms_network_capability_length:8,
                             (length(M_ms_network_capability)):1/integer, (<< <<X/binary>> || X <- M_ms_network_capability>>)/binary,
                             M_container_length:16,
                             (length(M_container)):1/integer, (<< <<X/binary>> || X <- M_container>>)/binary>>);

encode_v1_element(#mm_context_gsm_and_umts{
                       cksn = M_cksn,
                       no_of_vectors = M_no_of_vectors,
                       used_cipher = M_used_cipher,
                       kc = M_kc,
                       quintuplet_length = M_quintuplet_length,
                       quintuplet = M_quintuplet,
                       drx_parameter = M_drx_parameter,
                       ms_network_capability_length = M_ms_network_capability_length,
                       ms_network_capability = M_ms_network_capability,
                       container_length = M_container_length,
                       container = M_container}) ->
    encode_v1_element(129, <<0:4,
                             M_cksn:4,
                             3:2,
                             M_no_of_vectors:3,
                             M_used_cipher:3,
                             M_kc:8/bytes,
                             M_quintuplet_length:16,
                             (length(M_quintuplet)):1/integer, (<< <<X/binary>> || X <- M_quintuplet>>)/binary,
                             M_drx_parameter:2/bytes,
                             M_ms_network_capability_length:8,
                             (length(M_ms_network_capability)):1/integer, (<< <<X/binary>> || X <- M_ms_network_capability>>)/binary,
                             M_container_length:16,
                             (length(M_container)):1/integer, (<< <<X/binary>> || X <- M_container>>)/binary>>);

encode_v1_element(#mm_context_umts_and_used_cipher{
                       ksi = M_ksi,
                       no_of_vectors = M_no_of_vectors,
                       used_cipher = M_used_cipher,
                       ck = M_ck,
                       ik = M_ik,
                       quintuplet_length = M_quintuplet_length,
                       quintuplet = M_quintuplet,
                       drx_parameter = M_drx_parameter,
                       ms_network_capability_length = M_ms_network_capability_length,
                       ms_network_capability = M_ms_network_capability,
                       container_length = M_container_length,
                       container = M_container}) ->
    encode_v1_element(129, <<0:4,
                             M_ksi:4,
                             0:2,
                             M_no_of_vectors:3,
                             M_used_cipher:3,
                             M_ck:16/bytes,
                             M_ik:16/bytes,
                             M_quintuplet_length:16,
                             (length(M_quintuplet)):1/integer, (<< <<X/binary>> || X <- M_quintuplet>>)/binary,
                             M_drx_parameter:2/bytes,
                             M_ms_network_capability_length:8,
                             (length(M_ms_network_capability)):1/integer, (<< <<X/binary>> || X <- M_ms_network_capability>>)/binary,
                             M_container_length:16,
                             (length(M_container)):1/integer, (<< <<X/binary>> || X <- M_container>>)/binary>>);

encode_v1_element(#pdp_context{
                       }) ->
    encode_v1_element(130, <<>>);

encode_v1_element(#access_point_name{
                       apn = M_apn}) ->
    encode_v1_element(131, <<M_apn/binary>>);

encode_v1_element(#protocol_configuration_options{
                       config = M_config}) ->
    encode_v1_element(132, <<(encode_protocol_config_opts(M_config))/binary>>);

encode_v1_element(#gsn_address{
                       address = M_address}) ->
    encode_v1_element(133, <<M_address/binary>>);

encode_v1_element(#ms_international_pstn_isdn_number{
                       msisdn = M_msisdn}) ->
    encode_v1_element(134, <<(encode_isdn_address_string(M_msisdn))/binary>>);

encode_v1_element(#quality_of_service_profile{
                       priority = M_priority,
                       data = M_data}) ->
    encode_v1_element(135, <<M_priority:8,
                             M_data/binary>>);

encode_v1_element(#authentication_quintuplet{
                       }) ->
    encode_v1_element(136, <<>>);

encode_v1_element(#traffic_flow_template{
                       }) ->
    encode_v1_element(137, <<>>);

encode_v1_element(#target_identification{
                       }) ->
    encode_v1_element(138, <<>>);

encode_v1_element(#utran_transparent_container{
                       }) ->
    encode_v1_element(139, <<>>);

encode_v1_element(#rab_setup_information{
                       }) ->
    encode_v1_element(140, <<>>);

encode_v1_element(#extension_header_type_list{
                       }) ->
    encode_v1_element(141, <<>>);

encode_v1_element(#trigger_id{
                       }) ->
    encode_v1_element(142, <<>>);

encode_v1_element(#omc_identity{
                       }) ->
    encode_v1_element(143, <<>>);

encode_v1_element(#ran_transparent_container{
                       }) ->
    encode_v1_element(144, <<>>);

encode_v1_element(#pdp_context_prioritization{
                       }) ->
    encode_v1_element(145, <<>>);

encode_v1_element(#additional_rab_setup_information{
                       }) ->
    encode_v1_element(146, <<>>);

encode_v1_element(#sgsn_number{
                       }) ->
    encode_v1_element(147, <<>>);

encode_v1_element(#common_flags{
                       }) ->
    encode_v1_element(148, <<>>);

encode_v1_element(#apn_restriction{
                       }) ->
    encode_v1_element(149, <<>>);

encode_v1_element(#radio_priority_lcs{
                       }) ->
    encode_v1_element(150, <<>>);

encode_v1_element(#rat_type{
                       }) ->
    encode_v1_element(151, <<>>);

encode_v1_element(#user_location_information{
                       }) ->
    encode_v1_element(152, <<>>);

encode_v1_element(#ms_time_zone{
                       }) ->
    encode_v1_element(153, <<>>);

encode_v1_element(#imei{
                       }) ->
    encode_v1_element(154, <<>>);

encode_v1_element(#camel_charging_information_container{
                       }) ->
    encode_v1_element(155, <<>>);

encode_v1_element(#mbms_ue_context{
                       }) ->
    encode_v1_element(156, <<>>);

encode_v1_element(#temporary_mobile_group_identity{
                       }) ->
    encode_v1_element(157, <<>>);

encode_v1_element(#rim_routing_address{
                       }) ->
    encode_v1_element(158, <<>>);

encode_v1_element(#mbms_protocol_configuration_options{
                       }) ->
    encode_v1_element(159, <<>>);

encode_v1_element(#mbms_service_area{
                       }) ->
    encode_v1_element(160, <<>>);

encode_v1_element(#source_rnc_pdcp_context_info{
                       }) ->
    encode_v1_element(161, <<>>);

encode_v1_element(#additional_trace_info{
                       }) ->
    encode_v1_element(162, <<>>);

encode_v1_element(#hop_counter{
                       }) ->
    encode_v1_element(163, <<>>);

encode_v1_element(#selected_plmn_id{
                       }) ->
    encode_v1_element(164, <<>>);

encode_v1_element(#mbms_session_identifier{
                       }) ->
    encode_v1_element(165, <<>>);

encode_v1_element(#mbms_2g_3g_indicator{
                       }) ->
    encode_v1_element(166, <<>>);

encode_v1_element(#enhanced_nsapi{
                       }) ->
    encode_v1_element(167, <<>>);

encode_v1_element(#mbms_session_duration{
                       }) ->
    encode_v1_element(168, <<>>);

encode_v1_element(#additional_mbms_trace_info{
                       }) ->
    encode_v1_element(169, <<>>);

encode_v1_element(#mbms_session_repetition_number{
                       }) ->
    encode_v1_element(170, <<>>);

encode_v1_element(#mbms_time_to_data_transfer{
                       }) ->
    encode_v1_element(171, <<>>);

encode_v1_element(#bss_container{
                       }) ->
    encode_v1_element(173, <<>>);

encode_v1_element(#cell_identification{
                       }) ->
    encode_v1_element(174, <<>>);

encode_v1_element(#pdu_numbers{
                       }) ->
    encode_v1_element(175, <<>>);

encode_v1_element(#bssgp_cause{
                       }) ->
    encode_v1_element(176, <<>>);

encode_v1_element(#required_mbms_bearer_capabilities{
                       }) ->
    encode_v1_element(177, <<>>);

encode_v1_element(#rim_routing_address_discriminator{
                       }) ->
    encode_v1_element(178, <<>>);

encode_v1_element(#list_of_set_up_pfcs{
                       }) ->
    encode_v1_element(179, <<>>);

encode_v1_element(#ps_handover_xid_parameters{
                       }) ->
    encode_v1_element(180, <<>>);

encode_v1_element(#ms_info_change_reporting_action{
                       }) ->
    encode_v1_element(181, <<>>);

encode_v1_element(#direct_tunnel_flags{
                       }) ->
    encode_v1_element(182, <<>>);

encode_v1_element(#correlation_id{
                       }) ->
    encode_v1_element(183, <<>>);

encode_v1_element(#bearer_control_mode{
                       }) ->
    encode_v1_element(184, <<>>);

encode_v1_element(#mbms_flow_identifier{
                       }) ->
    encode_v1_element(185, <<>>);

encode_v1_element(#mbms_ip_multicast_distribution{
                       }) ->
    encode_v1_element(186, <<>>);

encode_v1_element(#mbms_distribution_acknowledgement{
                       }) ->
    encode_v1_element(187, <<>>);

encode_v1_element(#reliable_inter_rat_handover_info{
                       }) ->
    encode_v1_element(188, <<>>);

encode_v1_element(#rfsp_index{
                       }) ->
    encode_v1_element(189, <<>>);

encode_v1_element(#fully_qualified_domain_name{
                       }) ->
    encode_v1_element(190, <<>>);

encode_v1_element(#evolved_allocation_retention_priority_i{
                       }) ->
    encode_v1_element(191, <<>>);

encode_v1_element(#evolved_allocation_retention_priority_ii{
                       }) ->
    encode_v1_element(192, <<>>);

encode_v1_element(#extended_common_flags{
                       }) ->
    encode_v1_element(193, <<>>);

encode_v1_element(#user_csg_information{
                       }) ->
    encode_v1_element(194, <<>>);

encode_v1_element(#csg_information_reporting_action{
                       }) ->
    encode_v1_element(195, <<>>);

encode_v1_element(#csg_id{
                       }) ->
    encode_v1_element(196, <<>>);

encode_v1_element(#csg_membership_indication{
                       }) ->
    encode_v1_element(197, <<>>);

encode_v1_element(#aggregate_maximum_bit_rate{
                       }) ->
    encode_v1_element(198, <<>>);

encode_v1_element(#ue_network_capability{
                       }) ->
    encode_v1_element(199, <<>>);

encode_v1_element(#ue_ambr{
                       }) ->
    encode_v1_element(200, <<>>);

encode_v1_element(#apn_ambr_with_nsapi{
                       }) ->
    encode_v1_element(201, <<>>);

encode_v1_element(#ggsn_back_off_time{
                       }) ->
    encode_v1_element(202, <<>>);

encode_v1_element(#signalling_priority_indication{
                       }) ->
    encode_v1_element(203, <<>>);

encode_v1_element(#signalling_priority_indication_with_nsapi{
                       }) ->
    encode_v1_element(204, <<>>);

encode_v1_element(#higher_bitrates_than_16_mbps_flag{
                       }) ->
    encode_v1_element(205, <<>>);

encode_v1_element(#additional_mm_context_for_srvcc{
                       }) ->
    encode_v1_element(207, <<>>);

encode_v1_element(#additional_flags_for_srvcc{
                       }) ->
    encode_v1_element(208, <<>>);

encode_v1_element(#stn_sr{
                       }) ->
    encode_v1_element(209, <<>>);

encode_v1_element(#c_msisdn{
                       }) ->
    encode_v1_element(210, <<>>);

encode_v1_element(#extended_ranap_cause{
                       }) ->
    encode_v1_element(211, <<>>);

encode_v1_element(#enodeb_id{
                       }) ->
    encode_v1_element(212, <<>>);

encode_v1_element(#selection_mode_with_nsapi{
                       }) ->
    encode_v1_element(213, <<>>);

encode_v1_element(#uli_timestamp{
                       }) ->
    encode_v1_element(214, <<>>);

encode_v1_element(#local_home_network_id_with_nsapi{
                       }) ->
    encode_v1_element(215, <<>>);

encode_v1_element(#cn_operator_selection_entity{
                       }) ->
    encode_v1_element(216, <<>>);

encode_v1_element(#charging_gateway_address{
                       }) ->
    encode_v1_element(251, <<>>);

encode_v1_element(#private_extension{
                       }) ->
    encode_v1_element(255, <<>>);

encode_v1_element({Tag, Value}) when is_integer(Tag), is_binary(Value) ->
    encode_v1_element(Tag, Value).
