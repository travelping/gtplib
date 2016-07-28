%% This file is auto-generated. DO NOT EDIT

-record(cause, {
        instance = 0,
        value = [82,101,113,117,101,115,116,32,73,77,83,73]
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
        instance = 0
}).

-record(p_tmsi_signature, {
        instance = 0
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
        instance = 0
}).

-record(rab_context, {
        instance = 0
}).

-record(radio_priority_sms, {
        instance = 0
}).

-record(radio_priority, {
        instance = 0
}).

-record(packet_flow_id, {
        instance = 0
}).

-record(charging_characteristics, {
        instance = 0,
        value = <<0,0>>
}).

-record(trace_reference, {
        instance = 0
}).

-record(trace_type, {
        instance = 0
}).

-record(ms_not_reachable_reason, {
        instance = 0
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
        dual_address_bearer_flag = 0,
        upgrade_qos_supported = 0,
        nrsn = 0,
        no_qos_negotiation = 0,
        mbms_counting_information = 0,
        ran_procedures_ready = 0,
        mbms_service_type = 0,
        prohibit_payload_compression = 0
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
        instance = 0
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
        instance = 0
}).

-record(evolved_allocation_retention_priority_i, {
        instance = 0
}).

-record(evolved_allocation_retention_priority_ii, {
        instance = 0
}).

-record(extended_common_flags, {
        instance = 0
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

