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

decode_v1_element(1, Instance, <<M_value:8/integer>>) ->
    #cause{instance = Instance,
           value = enum_value(M_value)};

decode_v1_element(2, Instance, <<M_imsi:64/bits>>) ->
    #international_mobile_subscriber_identity{instance = Instance,
                                              imsi = decode_tbcd(M_imsi)};

decode_v1_element(3, Instance, <<M_mcc:24/bits,
                                 M_mcn:24/bits,
                                 M_lac:16/integer,
                                 M_rac:8/integer>>) ->
    #routeing_area_identity{instance = Instance,
                            mcc = decode_tbcd(M_mcc),
                            mcn = decode_tbcd(M_mcn),
                            lac = M_lac,
                            rac = M_rac};

decode_v1_element(4, Instance, <<M_tlli:4/bytes>>) ->
    #temporary_logical_link_identity{instance = Instance,
                                     tlli = M_tlli};

decode_v1_element(5, Instance, <<M_p_tmsi:4/bytes>>) ->
    #packet_tmsi{instance = Instance,
                 p_tmsi = M_p_tmsi};

decode_v1_element(8, Instance, <<_:7,
                                 M_required:1/integer>>) ->
    #reordering_required{instance = Instance,
                         required = enum_required(M_required)};

decode_v1_element(9, Instance, <<M_rand:16/bytes,
                                 M_sres:4/bytes,
                                 M_kc:8/bytes>>) ->
    #authentication_triplet{instance = Instance,
                            rand = M_rand,
                            sres = M_sres,
                            kc = M_kc};

decode_v1_element(11, Instance, <<>>) ->
    #map_cause{instance = Instance};

decode_v1_element(12, Instance, <<>>) ->
    #p_tmsi_signature{instance = Instance};

decode_v1_element(13, Instance, <<>>) ->
    #ms_validated{instance = Instance};

decode_v1_element(14, Instance, <<M_restart_counter:8/integer>>) ->
    #recovery{instance = Instance,
              restart_counter = M_restart_counter};

decode_v1_element(15, Instance, <<_:6,
                                  M_selection_mode_value:2/integer>>) ->
    #selection_mode{instance = Instance,
                    selection_mode_value = enum_selection_mode_value(M_selection_mode_value)};

decode_v1_element(16, Instance, <<M_tei:32/integer>>) ->
    #tunnel_endpoint_identifier_data_i{instance = Instance,
                                       tei = M_tei};

decode_v1_element(17, Instance, <<M_tei:32/integer>>) ->
    #tunnel_endpoint_identifier_control_plane{instance = Instance,
                                              tei = M_tei};

decode_v1_element(18, Instance, <<_:4,
                                  M_nsapi:4/integer,
                                  M_tei:32/integer>>) ->
    #tunnel_endpoint_identifier_data_ii{instance = Instance,
                                        nsapi = M_nsapi,
                                        tei = M_tei};

decode_v1_element(19, Instance, <<_:7,
                                  M_value:1/integer>>) ->
    #teardown_ind{instance = Instance,
                  value = M_value};

decode_v1_element(20, Instance, <<_:4,
                                  M_nsapi:4/integer>>) ->
    #nsapi{instance = Instance,
           nsapi = M_nsapi};

decode_v1_element(21, Instance, <<>>) ->
    #ranap_cause{instance = Instance};

decode_v1_element(22, Instance, <<>>) ->
    #rab_context{instance = Instance};

decode_v1_element(23, Instance, <<>>) ->
    #radio_priority_sms{instance = Instance};

decode_v1_element(24, Instance, <<>>) ->
    #radio_priority{instance = Instance};

decode_v1_element(25, Instance, <<>>) ->
    #packet_flow_id{instance = Instance};

decode_v1_element(26, Instance, <<M_value:2/bytes>>) ->
    #charging_characteristics{instance = Instance,
                              value = M_value};

decode_v1_element(27, Instance, <<>>) ->
    #trace_reference{instance = Instance};

decode_v1_element(28, Instance, <<>>) ->
    #trace_type{instance = Instance};

decode_v1_element(29, Instance, <<>>) ->
    #ms_not_reachable_reason{instance = Instance};

decode_v1_element(127, Instance, <<M_id:4/bytes>>) ->
    #charging_id{instance = Instance,
                 id = M_id};

decode_v1_element(128, Instance, <<15:4,
                                   M_pdp_type_organization:4/integer,
                                   M_pdp_type_number:8/integer,
                                   M_pdp_address/binary>>) ->
    #end_user_address{instance = Instance,
                      pdp_type_organization = M_pdp_type_organization,
                      pdp_type_number = M_pdp_type_number,
                      pdp_address = M_pdp_address};

decode_v1_element(129, Instance, <<_:4,
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
    #mm_context_gsm{instance = Instance,
                    cksn = M_cksn,
                    no_of_vectors = M_no_of_vectors,
                    used_cipher = M_used_cipher,
                    kc = M_kc,
                    tripple = [X || <<X:8/bytes>> <= M_tripple],
                    drx_parameter = M_drx_parameter,
                    ms_network_capability_length = M_ms_network_capability_length,
                    ms_network_capability = [X || <<X:1/bytes>> <= M_ms_network_capability],
                    container_length = M_container_length,
                    container = [X || <<X:1/bytes>> <= M_container]};

decode_v1_element(129, Instance, <<_:4,
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
    #mm_context_umts{instance = Instance,
                     ksi = M_ksi,
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

decode_v1_element(129, Instance, <<_:4,
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
    #mm_context_gsm_and_umts{instance = Instance,
                             cksn = M_cksn,
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

decode_v1_element(129, Instance, <<_:4,
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
    #mm_context_umts_and_used_cipher{instance = Instance,
                                     ksi = M_ksi,
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

decode_v1_element(130, Instance, <<>>) ->
    #pdp_context{instance = Instance};

decode_v1_element(131, Instance, <<M_apn/binary>>) ->
    #access_point_name{instance = Instance,
                       apn = decode_apn(M_apn)};

decode_v1_element(132, Instance, <<M_config/binary>>) ->
    #protocol_configuration_options{instance = Instance,
                                    config = decode_protocol_config_opts(M_config)};

decode_v1_element(133, Instance, <<M_address/binary>>) ->
    #gsn_address{instance = Instance,
                 address = M_address};

decode_v1_element(134, Instance, <<M_msisdn/binary>>) ->
    #ms_international_pstn_isdn_number{instance = Instance,
                                       msisdn = decode_isdn_address_string(M_msisdn)};

decode_v1_element(135, Instance, <<M_priority:8/integer,
                                   M_data/binary>>) ->
    #quality_of_service_profile{instance = Instance,
                                priority = M_priority,
                                data = M_data};

decode_v1_element(136, Instance, <<>>) ->
    #authentication_quintuplet{instance = Instance};

decode_v1_element(137, Instance, <<>>) ->
    #traffic_flow_template{instance = Instance};

decode_v1_element(138, Instance, <<>>) ->
    #target_identification{instance = Instance};

decode_v1_element(139, Instance, <<>>) ->
    #utran_transparent_container{instance = Instance};

decode_v1_element(140, Instance, <<>>) ->
    #rab_setup_information{instance = Instance};

decode_v1_element(141, Instance, <<>>) ->
    #extension_header_type_list{instance = Instance};

decode_v1_element(142, Instance, <<>>) ->
    #trigger_id{instance = Instance};

decode_v1_element(143, Instance, <<>>) ->
    #omc_identity{instance = Instance};

decode_v1_element(144, Instance, <<>>) ->
    #ran_transparent_container{instance = Instance};

decode_v1_element(145, Instance, <<>>) ->
    #pdp_context_prioritization{instance = Instance};

decode_v1_element(146, Instance, <<>>) ->
    #additional_rab_setup_information{instance = Instance};

decode_v1_element(147, Instance, <<>>) ->
    #sgsn_number{instance = Instance};

decode_v1_element(148, Instance, <<>>) ->
    #common_flags{instance = Instance};

decode_v1_element(149, Instance, <<>>) ->
    #apn_restriction{instance = Instance};

decode_v1_element(150, Instance, <<>>) ->
    #radio_priority_lcs{instance = Instance};

decode_v1_element(151, Instance, <<>>) ->
    #rat_type{instance = Instance};

decode_v1_element(152, Instance, <<>>) ->
    #user_location_information{instance = Instance};

decode_v1_element(153, Instance, <<>>) ->
    #ms_time_zone{instance = Instance};

decode_v1_element(154, Instance, <<>>) ->
    #imei{instance = Instance};

decode_v1_element(155, Instance, <<>>) ->
    #camel_charging_information_container{instance = Instance};

decode_v1_element(156, Instance, <<>>) ->
    #mbms_ue_context{instance = Instance};

decode_v1_element(157, Instance, <<>>) ->
    #temporary_mobile_group_identity{instance = Instance};

decode_v1_element(158, Instance, <<>>) ->
    #rim_routing_address{instance = Instance};

decode_v1_element(159, Instance, <<>>) ->
    #mbms_protocol_configuration_options{instance = Instance};

decode_v1_element(160, Instance, <<>>) ->
    #mbms_service_area{instance = Instance};

decode_v1_element(161, Instance, <<>>) ->
    #source_rnc_pdcp_context_info{instance = Instance};

decode_v1_element(162, Instance, <<>>) ->
    #additional_trace_info{instance = Instance};

decode_v1_element(163, Instance, <<>>) ->
    #hop_counter{instance = Instance};

decode_v1_element(164, Instance, <<>>) ->
    #selected_plmn_id{instance = Instance};

decode_v1_element(165, Instance, <<>>) ->
    #mbms_session_identifier{instance = Instance};

decode_v1_element(166, Instance, <<>>) ->
    #mbms_2g_3g_indicator{instance = Instance};

decode_v1_element(167, Instance, <<>>) ->
    #enhanced_nsapi{instance = Instance};

decode_v1_element(168, Instance, <<>>) ->
    #mbms_session_duration{instance = Instance};

decode_v1_element(169, Instance, <<>>) ->
    #additional_mbms_trace_info{instance = Instance};

decode_v1_element(170, Instance, <<>>) ->
    #mbms_session_repetition_number{instance = Instance};

decode_v1_element(171, Instance, <<>>) ->
    #mbms_time_to_data_transfer{instance = Instance};

decode_v1_element(173, Instance, <<>>) ->
    #bss_container{instance = Instance};

decode_v1_element(174, Instance, <<>>) ->
    #cell_identification{instance = Instance};

decode_v1_element(175, Instance, <<>>) ->
    #pdu_numbers{instance = Instance};

decode_v1_element(176, Instance, <<>>) ->
    #bssgp_cause{instance = Instance};

decode_v1_element(177, Instance, <<>>) ->
    #required_mbms_bearer_capabilities{instance = Instance};

decode_v1_element(178, Instance, <<>>) ->
    #rim_routing_address_discriminator{instance = Instance};

decode_v1_element(179, Instance, <<>>) ->
    #list_of_set_up_pfcs{instance = Instance};

decode_v1_element(180, Instance, <<>>) ->
    #ps_handover_xid_parameters{instance = Instance};

decode_v1_element(181, Instance, <<>>) ->
    #ms_info_change_reporting_action{instance = Instance};

decode_v1_element(182, Instance, <<>>) ->
    #direct_tunnel_flags{instance = Instance};

decode_v1_element(183, Instance, <<>>) ->
    #correlation_id{instance = Instance};

decode_v1_element(184, Instance, <<>>) ->
    #bearer_control_mode{instance = Instance};

decode_v1_element(185, Instance, <<>>) ->
    #mbms_flow_identifier{instance = Instance};

decode_v1_element(186, Instance, <<>>) ->
    #mbms_ip_multicast_distribution{instance = Instance};

decode_v1_element(187, Instance, <<>>) ->
    #mbms_distribution_acknowledgement{instance = Instance};

decode_v1_element(188, Instance, <<>>) ->
    #reliable_inter_rat_handover_info{instance = Instance};

decode_v1_element(189, Instance, <<>>) ->
    #rfsp_index{instance = Instance};

decode_v1_element(190, Instance, <<>>) ->
    #fully_qualified_domain_name{instance = Instance};

decode_v1_element(191, Instance, <<>>) ->
    #evolved_allocation_retention_priority_i{instance = Instance};

decode_v1_element(192, Instance, <<>>) ->
    #evolved_allocation_retention_priority_ii{instance = Instance};

decode_v1_element(193, Instance, <<>>) ->
    #extended_common_flags{instance = Instance};

decode_v1_element(194, Instance, <<>>) ->
    #user_csg_information{instance = Instance};

decode_v1_element(195, Instance, <<>>) ->
    #csg_information_reporting_action{instance = Instance};

decode_v1_element(196, Instance, <<>>) ->
    #csg_id{instance = Instance};

decode_v1_element(197, Instance, <<>>) ->
    #csg_membership_indication{instance = Instance};

decode_v1_element(198, Instance, <<>>) ->
    #aggregate_maximum_bit_rate{instance = Instance};

decode_v1_element(199, Instance, <<>>) ->
    #ue_network_capability{instance = Instance};

decode_v1_element(200, Instance, <<>>) ->
    #ue_ambr{instance = Instance};

decode_v1_element(201, Instance, <<>>) ->
    #apn_ambr_with_nsapi{instance = Instance};

decode_v1_element(202, Instance, <<>>) ->
    #ggsn_back_off_time{instance = Instance};

decode_v1_element(203, Instance, <<>>) ->
    #signalling_priority_indication{instance = Instance};

decode_v1_element(204, Instance, <<>>) ->
    #signalling_priority_indication_with_nsapi{instance = Instance};

decode_v1_element(205, Instance, <<>>) ->
    #higher_bitrates_than_16_mbps_flag{instance = Instance};

decode_v1_element(207, Instance, <<>>) ->
    #additional_mm_context_for_srvcc{instance = Instance};

decode_v1_element(208, Instance, <<>>) ->
    #additional_flags_for_srvcc{instance = Instance};

decode_v1_element(209, Instance, <<>>) ->
    #stn_sr{instance = Instance};

decode_v1_element(210, Instance, <<>>) ->
    #c_msisdn{instance = Instance};

decode_v1_element(211, Instance, <<>>) ->
    #extended_ranap_cause{instance = Instance};

decode_v1_element(212, Instance, <<>>) ->
    #enodeb_id{instance = Instance};

decode_v1_element(213, Instance, <<>>) ->
    #selection_mode_with_nsapi{instance = Instance};

decode_v1_element(214, Instance, <<>>) ->
    #uli_timestamp{instance = Instance};

decode_v1_element(215, Instance, <<>>) ->
    #local_home_network_id_with_nsapi{instance = Instance};

decode_v1_element(216, Instance, <<>>) ->
    #cn_operator_selection_entity{instance = Instance};

decode_v1_element(251, Instance, <<>>) ->
    #charging_gateway_address{instance = Instance};

decode_v1_element(255, Instance, <<>>) ->
    #private_extension{instance = Instance};

decode_v1_element(Tag, Instance, Value) ->
        {Tag, Instance, Value}.

decode_v1(<<>>, _PrevId, _PrevInst, IEs) ->
    lists:reverse(IEs);
decode_v1(<<1, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(1, PrevId, PrevInst),
    IE = decode_v1_element(1, Instance, Data),
    decode_v1(Next, 1, Instance, [IE|IEs]);
decode_v1(<<2, Data:8/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(2, PrevId, PrevInst),
    IE = decode_v1_element(2, Instance, Data),
    decode_v1(Next, 2, Instance, [IE|IEs]);
decode_v1(<<3, Data:6/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(3, PrevId, PrevInst),
    IE = decode_v1_element(3, Instance, Data),
    decode_v1(Next, 3, Instance, [IE|IEs]);
decode_v1(<<4, Data:4/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(4, PrevId, PrevInst),
    IE = decode_v1_element(4, Instance, Data),
    decode_v1(Next, 4, Instance, [IE|IEs]);
decode_v1(<<5, Data:4/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(5, PrevId, PrevInst),
    IE = decode_v1_element(5, Instance, Data),
    decode_v1(Next, 5, Instance, [IE|IEs]);
decode_v1(<<8, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(8, PrevId, PrevInst),
    IE = decode_v1_element(8, Instance, Data),
    decode_v1(Next, 8, Instance, [IE|IEs]);
decode_v1(<<9, Data:28/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(9, PrevId, PrevInst),
    IE = decode_v1_element(9, Instance, Data),
    decode_v1(Next, 9, Instance, [IE|IEs]);
decode_v1(<<11, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(11, PrevId, PrevInst),
    IE = decode_v1_element(11, Instance, Data),
    decode_v1(Next, 11, Instance, [IE|IEs]);
decode_v1(<<12, Data:3/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(12, PrevId, PrevInst),
    IE = decode_v1_element(12, Instance, Data),
    decode_v1(Next, 12, Instance, [IE|IEs]);
decode_v1(<<13, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(13, PrevId, PrevInst),
    IE = decode_v1_element(13, Instance, Data),
    decode_v1(Next, 13, Instance, [IE|IEs]);
decode_v1(<<14, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(14, PrevId, PrevInst),
    IE = decode_v1_element(14, Instance, Data),
    decode_v1(Next, 14, Instance, [IE|IEs]);
decode_v1(<<15, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(15, PrevId, PrevInst),
    IE = decode_v1_element(15, Instance, Data),
    decode_v1(Next, 15, Instance, [IE|IEs]);
decode_v1(<<16, Data:4/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(16, PrevId, PrevInst),
    IE = decode_v1_element(16, Instance, Data),
    decode_v1(Next, 16, Instance, [IE|IEs]);
decode_v1(<<17, Data:4/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(17, PrevId, PrevInst),
    IE = decode_v1_element(17, Instance, Data),
    decode_v1(Next, 17, Instance, [IE|IEs]);
decode_v1(<<18, Data:5/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(18, PrevId, PrevInst),
    IE = decode_v1_element(18, Instance, Data),
    decode_v1(Next, 18, Instance, [IE|IEs]);
decode_v1(<<19, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(19, PrevId, PrevInst),
    IE = decode_v1_element(19, Instance, Data),
    decode_v1(Next, 19, Instance, [IE|IEs]);
decode_v1(<<20, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(20, PrevId, PrevInst),
    IE = decode_v1_element(20, Instance, Data),
    decode_v1(Next, 20, Instance, [IE|IEs]);
decode_v1(<<21, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(21, PrevId, PrevInst),
    IE = decode_v1_element(21, Instance, Data),
    decode_v1(Next, 21, Instance, [IE|IEs]);
decode_v1(<<22, Data:9/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(22, PrevId, PrevInst),
    IE = decode_v1_element(22, Instance, Data),
    decode_v1(Next, 22, Instance, [IE|IEs]);
decode_v1(<<23, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(23, PrevId, PrevInst),
    IE = decode_v1_element(23, Instance, Data),
    decode_v1(Next, 23, Instance, [IE|IEs]);
decode_v1(<<24, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(24, PrevId, PrevInst),
    IE = decode_v1_element(24, Instance, Data),
    decode_v1(Next, 24, Instance, [IE|IEs]);
decode_v1(<<25, Data:2/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(25, PrevId, PrevInst),
    IE = decode_v1_element(25, Instance, Data),
    decode_v1(Next, 25, Instance, [IE|IEs]);
decode_v1(<<26, Data:2/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(26, PrevId, PrevInst),
    IE = decode_v1_element(26, Instance, Data),
    decode_v1(Next, 26, Instance, [IE|IEs]);
decode_v1(<<27, Data:2/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(27, PrevId, PrevInst),
    IE = decode_v1_element(27, Instance, Data),
    decode_v1(Next, 27, Instance, [IE|IEs]);
decode_v1(<<28, Data:2/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(28, PrevId, PrevInst),
    IE = decode_v1_element(28, Instance, Data),
    decode_v1(Next, 28, Instance, [IE|IEs]);
decode_v1(<<29, Data:1/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(29, PrevId, PrevInst),
    IE = decode_v1_element(29, Instance, Data),
    decode_v1(Next, 29, Instance, [IE|IEs]);
decode_v1(<<127, Data:4/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->
    Instance = v1_instance(127, PrevId, PrevInst),
    IE = decode_v1_element(127, Instance, Data),
    decode_v1(Next, 127, Instance, [IE|IEs]);
decode_v1(<<Id, Length:16/integer, Rest/binary>>, PrevId, PrevInst, IEs) when Id > 127 ->
    <<Data:Length/binary, Next/binary>> = Rest,
    Instance = v1_instance(Id, PrevId, PrevInst),
    IE = decode_v1_element(Id, Instance, Data),
    decode_v1(Next, Id, Instance, [IE|IEs]);
decode_v1(<<Id, Rest/binary>>, PrevId, PrevInst, IEs) ->
    Instance = v1_instance(Id, PrevId, PrevInst),
    IE = {Id, Instance, Rest},
    decode_v1(<<>>, Id, Instance, [IE|IEs]).


encode_v1_element(#cause{
                       instance = Instance,
                       value = M_value}) ->
    encode_v1_element(1, Instance, <<(enum_value(M_value)):8/integer>>);

encode_v1_element(#international_mobile_subscriber_identity{
                       instance = Instance,
                       imsi = M_imsi}) ->
    encode_v1_element(2, Instance, <<(encode_tbcd(M_imsi)):64/bits>>);

encode_v1_element(#routeing_area_identity{
                       instance = Instance,
                       mcc = M_mcc,
                       mcn = M_mcn,
                       lac = M_lac,
                       rac = M_rac}) ->
    encode_v1_element(3, Instance, <<(encode_tbcd(M_mcc)):24/bits,
                                     (encode_tbcd(M_mcn)):24/bits,
                                     M_lac:16,
                                     M_rac:8>>);

encode_v1_element(#temporary_logical_link_identity{
                       instance = Instance,
                       tlli = M_tlli}) ->
    encode_v1_element(4, Instance, <<M_tlli:4/bytes>>);

encode_v1_element(#packet_tmsi{
                       instance = Instance,
                       p_tmsi = M_p_tmsi}) ->
    encode_v1_element(5, Instance, <<M_p_tmsi:4/bytes>>);

encode_v1_element(#reordering_required{
                       instance = Instance,
                       required = M_required}) ->
    encode_v1_element(8, Instance, <<0:7,
                                     (enum_required(M_required)):1/integer>>);

encode_v1_element(#authentication_triplet{
                       instance = Instance,
                       rand = M_rand,
                       sres = M_sres,
                       kc = M_kc}) ->
    encode_v1_element(9, Instance, <<M_rand:16/bytes,
                                     M_sres:4/bytes,
                                     M_kc:8/bytes>>);

encode_v1_element(#map_cause{
                       instance = Instance}) ->
    encode_v1_element(11, Instance, <<>>);

encode_v1_element(#p_tmsi_signature{
                       instance = Instance}) ->
    encode_v1_element(12, Instance, <<>>);

encode_v1_element(#ms_validated{
                       instance = Instance}) ->
    encode_v1_element(13, Instance, <<>>);

encode_v1_element(#recovery{
                       instance = Instance,
                       restart_counter = M_restart_counter}) ->
    encode_v1_element(14, Instance, <<M_restart_counter:8>>);

encode_v1_element(#selection_mode{
                       instance = Instance,
                       selection_mode_value = M_selection_mode_value}) ->
    encode_v1_element(15, Instance, <<0:6,
                                      (enum_selection_mode_value(M_selection_mode_value)):2/integer>>);

encode_v1_element(#tunnel_endpoint_identifier_data_i{
                       instance = Instance,
                       tei = M_tei}) ->
    encode_v1_element(16, Instance, <<M_tei:32>>);

encode_v1_element(#tunnel_endpoint_identifier_control_plane{
                       instance = Instance,
                       tei = M_tei}) ->
    encode_v1_element(17, Instance, <<M_tei:32>>);

encode_v1_element(#tunnel_endpoint_identifier_data_ii{
                       instance = Instance,
                       nsapi = M_nsapi,
                       tei = M_tei}) ->
    encode_v1_element(18, Instance, <<0:4,
                                      M_nsapi:4,
                                      M_tei:32>>);

encode_v1_element(#teardown_ind{
                       instance = Instance,
                       value = M_value}) ->
    encode_v1_element(19, Instance, <<0:7,
                                      M_value:1>>);

encode_v1_element(#nsapi{
                       instance = Instance,
                       nsapi = M_nsapi}) ->
    encode_v1_element(20, Instance, <<0:4,
                                      M_nsapi:4>>);

encode_v1_element(#ranap_cause{
                       instance = Instance}) ->
    encode_v1_element(21, Instance, <<>>);

encode_v1_element(#rab_context{
                       instance = Instance}) ->
    encode_v1_element(22, Instance, <<>>);

encode_v1_element(#radio_priority_sms{
                       instance = Instance}) ->
    encode_v1_element(23, Instance, <<>>);

encode_v1_element(#radio_priority{
                       instance = Instance}) ->
    encode_v1_element(24, Instance, <<>>);

encode_v1_element(#packet_flow_id{
                       instance = Instance}) ->
    encode_v1_element(25, Instance, <<>>);

encode_v1_element(#charging_characteristics{
                       instance = Instance,
                       value = M_value}) ->
    encode_v1_element(26, Instance, <<M_value:2/bytes>>);

encode_v1_element(#trace_reference{
                       instance = Instance}) ->
    encode_v1_element(27, Instance, <<>>);

encode_v1_element(#trace_type{
                       instance = Instance}) ->
    encode_v1_element(28, Instance, <<>>);

encode_v1_element(#ms_not_reachable_reason{
                       instance = Instance}) ->
    encode_v1_element(29, Instance, <<>>);

encode_v1_element(#charging_id{
                       instance = Instance,
                       id = M_id}) ->
    encode_v1_element(127, Instance, <<M_id:4/bytes>>);

encode_v1_element(#end_user_address{
                       instance = Instance,
                       pdp_type_organization = M_pdp_type_organization,
                       pdp_type_number = M_pdp_type_number,
                       pdp_address = M_pdp_address}) ->
    encode_v1_element(128, Instance, <<15:4,
                                       M_pdp_type_organization:4,
                                       M_pdp_type_number:8,
                                       M_pdp_address/binary>>);

encode_v1_element(#mm_context_gsm{
                       instance = Instance,
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
    encode_v1_element(129, Instance, <<0:4,
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
                       instance = Instance,
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
    encode_v1_element(129, Instance, <<0:4,
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
                       instance = Instance,
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
    encode_v1_element(129, Instance, <<0:4,
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
                       instance = Instance,
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
    encode_v1_element(129, Instance, <<0:4,
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
                       instance = Instance}) ->
    encode_v1_element(130, Instance, <<>>);

encode_v1_element(#access_point_name{
                       instance = Instance,
                       apn = M_apn}) ->
    encode_v1_element(131, Instance, <<(encode_apn(M_apn)):0/bits>>);

encode_v1_element(#protocol_configuration_options{
                       instance = Instance,
                       config = M_config}) ->
    encode_v1_element(132, Instance, <<(encode_protocol_config_opts(M_config))/binary>>);

encode_v1_element(#gsn_address{
                       instance = Instance,
                       address = M_address}) ->
    encode_v1_element(133, Instance, <<M_address/binary>>);

encode_v1_element(#ms_international_pstn_isdn_number{
                       instance = Instance,
                       msisdn = M_msisdn}) ->
    encode_v1_element(134, Instance, <<(encode_isdn_address_string(M_msisdn))/binary>>);

encode_v1_element(#quality_of_service_profile{
                       instance = Instance,
                       priority = M_priority,
                       data = M_data}) ->
    encode_v1_element(135, Instance, <<M_priority:8,
                                       M_data/binary>>);

encode_v1_element(#authentication_quintuplet{
                       instance = Instance}) ->
    encode_v1_element(136, Instance, <<>>);

encode_v1_element(#traffic_flow_template{
                       instance = Instance}) ->
    encode_v1_element(137, Instance, <<>>);

encode_v1_element(#target_identification{
                       instance = Instance}) ->
    encode_v1_element(138, Instance, <<>>);

encode_v1_element(#utran_transparent_container{
                       instance = Instance}) ->
    encode_v1_element(139, Instance, <<>>);

encode_v1_element(#rab_setup_information{
                       instance = Instance}) ->
    encode_v1_element(140, Instance, <<>>);

encode_v1_element(#extension_header_type_list{
                       instance = Instance}) ->
    encode_v1_element(141, Instance, <<>>);

encode_v1_element(#trigger_id{
                       instance = Instance}) ->
    encode_v1_element(142, Instance, <<>>);

encode_v1_element(#omc_identity{
                       instance = Instance}) ->
    encode_v1_element(143, Instance, <<>>);

encode_v1_element(#ran_transparent_container{
                       instance = Instance}) ->
    encode_v1_element(144, Instance, <<>>);

encode_v1_element(#pdp_context_prioritization{
                       instance = Instance}) ->
    encode_v1_element(145, Instance, <<>>);

encode_v1_element(#additional_rab_setup_information{
                       instance = Instance}) ->
    encode_v1_element(146, Instance, <<>>);

encode_v1_element(#sgsn_number{
                       instance = Instance}) ->
    encode_v1_element(147, Instance, <<>>);

encode_v1_element(#common_flags{
                       instance = Instance}) ->
    encode_v1_element(148, Instance, <<>>);

encode_v1_element(#apn_restriction{
                       instance = Instance}) ->
    encode_v1_element(149, Instance, <<>>);

encode_v1_element(#radio_priority_lcs{
                       instance = Instance}) ->
    encode_v1_element(150, Instance, <<>>);

encode_v1_element(#rat_type{
                       instance = Instance}) ->
    encode_v1_element(151, Instance, <<>>);

encode_v1_element(#user_location_information{
                       instance = Instance}) ->
    encode_v1_element(152, Instance, <<>>);

encode_v1_element(#ms_time_zone{
                       instance = Instance}) ->
    encode_v1_element(153, Instance, <<>>);

encode_v1_element(#imei{
                       instance = Instance}) ->
    encode_v1_element(154, Instance, <<>>);

encode_v1_element(#camel_charging_information_container{
                       instance = Instance}) ->
    encode_v1_element(155, Instance, <<>>);

encode_v1_element(#mbms_ue_context{
                       instance = Instance}) ->
    encode_v1_element(156, Instance, <<>>);

encode_v1_element(#temporary_mobile_group_identity{
                       instance = Instance}) ->
    encode_v1_element(157, Instance, <<>>);

encode_v1_element(#rim_routing_address{
                       instance = Instance}) ->
    encode_v1_element(158, Instance, <<>>);

encode_v1_element(#mbms_protocol_configuration_options{
                       instance = Instance}) ->
    encode_v1_element(159, Instance, <<>>);

encode_v1_element(#mbms_service_area{
                       instance = Instance}) ->
    encode_v1_element(160, Instance, <<>>);

encode_v1_element(#source_rnc_pdcp_context_info{
                       instance = Instance}) ->
    encode_v1_element(161, Instance, <<>>);

encode_v1_element(#additional_trace_info{
                       instance = Instance}) ->
    encode_v1_element(162, Instance, <<>>);

encode_v1_element(#hop_counter{
                       instance = Instance}) ->
    encode_v1_element(163, Instance, <<>>);

encode_v1_element(#selected_plmn_id{
                       instance = Instance}) ->
    encode_v1_element(164, Instance, <<>>);

encode_v1_element(#mbms_session_identifier{
                       instance = Instance}) ->
    encode_v1_element(165, Instance, <<>>);

encode_v1_element(#mbms_2g_3g_indicator{
                       instance = Instance}) ->
    encode_v1_element(166, Instance, <<>>);

encode_v1_element(#enhanced_nsapi{
                       instance = Instance}) ->
    encode_v1_element(167, Instance, <<>>);

encode_v1_element(#mbms_session_duration{
                       instance = Instance}) ->
    encode_v1_element(168, Instance, <<>>);

encode_v1_element(#additional_mbms_trace_info{
                       instance = Instance}) ->
    encode_v1_element(169, Instance, <<>>);

encode_v1_element(#mbms_session_repetition_number{
                       instance = Instance}) ->
    encode_v1_element(170, Instance, <<>>);

encode_v1_element(#mbms_time_to_data_transfer{
                       instance = Instance}) ->
    encode_v1_element(171, Instance, <<>>);

encode_v1_element(#bss_container{
                       instance = Instance}) ->
    encode_v1_element(173, Instance, <<>>);

encode_v1_element(#cell_identification{
                       instance = Instance}) ->
    encode_v1_element(174, Instance, <<>>);

encode_v1_element(#pdu_numbers{
                       instance = Instance}) ->
    encode_v1_element(175, Instance, <<>>);

encode_v1_element(#bssgp_cause{
                       instance = Instance}) ->
    encode_v1_element(176, Instance, <<>>);

encode_v1_element(#required_mbms_bearer_capabilities{
                       instance = Instance}) ->
    encode_v1_element(177, Instance, <<>>);

encode_v1_element(#rim_routing_address_discriminator{
                       instance = Instance}) ->
    encode_v1_element(178, Instance, <<>>);

encode_v1_element(#list_of_set_up_pfcs{
                       instance = Instance}) ->
    encode_v1_element(179, Instance, <<>>);

encode_v1_element(#ps_handover_xid_parameters{
                       instance = Instance}) ->
    encode_v1_element(180, Instance, <<>>);

encode_v1_element(#ms_info_change_reporting_action{
                       instance = Instance}) ->
    encode_v1_element(181, Instance, <<>>);

encode_v1_element(#direct_tunnel_flags{
                       instance = Instance}) ->
    encode_v1_element(182, Instance, <<>>);

encode_v1_element(#correlation_id{
                       instance = Instance}) ->
    encode_v1_element(183, Instance, <<>>);

encode_v1_element(#bearer_control_mode{
                       instance = Instance}) ->
    encode_v1_element(184, Instance, <<>>);

encode_v1_element(#mbms_flow_identifier{
                       instance = Instance}) ->
    encode_v1_element(185, Instance, <<>>);

encode_v1_element(#mbms_ip_multicast_distribution{
                       instance = Instance}) ->
    encode_v1_element(186, Instance, <<>>);

encode_v1_element(#mbms_distribution_acknowledgement{
                       instance = Instance}) ->
    encode_v1_element(187, Instance, <<>>);

encode_v1_element(#reliable_inter_rat_handover_info{
                       instance = Instance}) ->
    encode_v1_element(188, Instance, <<>>);

encode_v1_element(#rfsp_index{
                       instance = Instance}) ->
    encode_v1_element(189, Instance, <<>>);

encode_v1_element(#fully_qualified_domain_name{
                       instance = Instance}) ->
    encode_v1_element(190, Instance, <<>>);

encode_v1_element(#evolved_allocation_retention_priority_i{
                       instance = Instance}) ->
    encode_v1_element(191, Instance, <<>>);

encode_v1_element(#evolved_allocation_retention_priority_ii{
                       instance = Instance}) ->
    encode_v1_element(192, Instance, <<>>);

encode_v1_element(#extended_common_flags{
                       instance = Instance}) ->
    encode_v1_element(193, Instance, <<>>);

encode_v1_element(#user_csg_information{
                       instance = Instance}) ->
    encode_v1_element(194, Instance, <<>>);

encode_v1_element(#csg_information_reporting_action{
                       instance = Instance}) ->
    encode_v1_element(195, Instance, <<>>);

encode_v1_element(#csg_id{
                       instance = Instance}) ->
    encode_v1_element(196, Instance, <<>>);

encode_v1_element(#csg_membership_indication{
                       instance = Instance}) ->
    encode_v1_element(197, Instance, <<>>);

encode_v1_element(#aggregate_maximum_bit_rate{
                       instance = Instance}) ->
    encode_v1_element(198, Instance, <<>>);

encode_v1_element(#ue_network_capability{
                       instance = Instance}) ->
    encode_v1_element(199, Instance, <<>>);

encode_v1_element(#ue_ambr{
                       instance = Instance}) ->
    encode_v1_element(200, Instance, <<>>);

encode_v1_element(#apn_ambr_with_nsapi{
                       instance = Instance}) ->
    encode_v1_element(201, Instance, <<>>);

encode_v1_element(#ggsn_back_off_time{
                       instance = Instance}) ->
    encode_v1_element(202, Instance, <<>>);

encode_v1_element(#signalling_priority_indication{
                       instance = Instance}) ->
    encode_v1_element(203, Instance, <<>>);

encode_v1_element(#signalling_priority_indication_with_nsapi{
                       instance = Instance}) ->
    encode_v1_element(204, Instance, <<>>);

encode_v1_element(#higher_bitrates_than_16_mbps_flag{
                       instance = Instance}) ->
    encode_v1_element(205, Instance, <<>>);

encode_v1_element(#additional_mm_context_for_srvcc{
                       instance = Instance}) ->
    encode_v1_element(207, Instance, <<>>);

encode_v1_element(#additional_flags_for_srvcc{
                       instance = Instance}) ->
    encode_v1_element(208, Instance, <<>>);

encode_v1_element(#stn_sr{
                       instance = Instance}) ->
    encode_v1_element(209, Instance, <<>>);

encode_v1_element(#c_msisdn{
                       instance = Instance}) ->
    encode_v1_element(210, Instance, <<>>);

encode_v1_element(#extended_ranap_cause{
                       instance = Instance}) ->
    encode_v1_element(211, Instance, <<>>);

encode_v1_element(#enodeb_id{
                       instance = Instance}) ->
    encode_v1_element(212, Instance, <<>>);

encode_v1_element(#selection_mode_with_nsapi{
                       instance = Instance}) ->
    encode_v1_element(213, Instance, <<>>);

encode_v1_element(#uli_timestamp{
                       instance = Instance}) ->
    encode_v1_element(214, Instance, <<>>);

encode_v1_element(#local_home_network_id_with_nsapi{
                       instance = Instance}) ->
    encode_v1_element(215, Instance, <<>>);

encode_v1_element(#cn_operator_selection_entity{
                       instance = Instance}) ->
    encode_v1_element(216, Instance, <<>>);

encode_v1_element(#charging_gateway_address{
                       instance = Instance}) ->
    encode_v1_element(251, Instance, <<>>);

encode_v1_element(#private_extension{
                       instance = Instance}) ->
    encode_v1_element(255, Instance, <<>>);

encode_v1_element({Tag, Instance, Value}) when is_integer(Tag), is_integer(Instance), is_binary(Value) ->
    encode_v1_element(Tag, Instance, Value).
