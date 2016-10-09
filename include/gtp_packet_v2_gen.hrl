%% This file is auto-generated. DO NOT EDIT

-record(v2_international_mobile_subscriber_identity, {
        instance = 0,
        imsi
}).

-record(v2_cause, {
        instance = 0,
        v2_cause = [82,101,115,101,114,118,101,100],
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
        type = [73,80,118,52],
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
        instance = 0
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
        pdn_type = [73,80,118,52]
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
        instance = 0
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
        instance = 0
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
        instance = 0
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

