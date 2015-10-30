%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% Copyright 2015, Travelping GmbH <info@travelping.com>

%% This file is auto-generated. DO NOT EDIT

-record(cause, {
        value = [82,101,113,117,101,115,116,32,73,77,83,73]
}).

-record(international_mobile_subscriber_identity, {
        imsi
}).

-record(routeing_area_identity, {
        mcc,
        mcn,
        lac = 0,
        rac = 0
}).

-record(temporary_logical_link_identity, {
        tlli = <<0,0,0,0>>
}).

-record(packet_tmsi, {
        p_tmsi = <<0,0,0,0>>
}).

-record(reordering_required, {
        required = no
}).

-record(authentication_triplet, {
        rand = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
        sres = <<0,0,0,0>>,
        kc = <<0,0,0,0,0,0,0,0>>
}).

-record(map_cause, {
        
}).

-record(p_tmsi_signature, {
        
}).

-record(ms_validated, {
        
}).

-record(recovery, {
        restart_counter = 0
}).

-record(selection_mode, {
        selection_mode_value = [77,83,32,111,114,32,110,101,116,119,111,114,107,32,112,114,111,118,105,100,101,100,32,65,80,78,44,32,115,117,98,115,99,114,105,98,101,100,32,118,101,114,105,102,105,101,100]
}).

-record(tunnel_endpoint_identifier_data_i, {
        tei = 0
}).

-record(tunnel_endpoint_identifier_control_plane, {
        tei = 0
}).

-record(tunnel_endpoint_identifier_data_ii, {
        nsapi = 0,
        tei = 0
}).

-record(teardown_ind, {
        value = 0
}).

-record(nsapi, {
        nsapi = 0
}).

-record(ranap_cause, {
        
}).

-record(rab_context, {
        
}).

-record(radio_priority_sms, {
        
}).

-record(radio_priority, {
        
}).

-record(packet_flow_id, {
        
}).

-record(charging_characteristics, {
        value = <<0,0>>
}).

-record(trace_reference, {
        
}).

-record(trace_type, {
        
}).

-record(ms_not_reachable_reason, {
        
}).

-record(charging_id, {
        id = <<0,0,0,0>>
}).

-record(end_user_address, {
        pdp_type_organization = 0,
        pdp_type_number = 0,
        pdp_address = <<>>
}).

-record(mm_context_gsm, {
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
        
}).

-record(access_point_name, {
        apn = <<>>
}).

-record(protocol_configuration_options, {
        config
}).

-record(gsn_address, {
        address = <<>>
}).

-record(ms_international_pstn_isdn_number, {
        msisdn
}).

-record(quality_of_service_profile, {
        priority = 0,
        data = <<>>
}).

-record(authentication_quintuplet, {
        
}).

-record(traffic_flow_template, {
        
}).

-record(target_identification, {
        
}).

-record(utran_transparent_container, {
        
}).

-record(rab_setup_information, {
        
}).

-record(extension_header_type_list, {
        
}).

-record(trigger_id, {
        
}).

-record(omc_identity, {
        
}).

-record(ran_transparent_container, {
        
}).

-record(pdp_context_prioritization, {
        
}).

-record(additional_rab_setup_information, {
        
}).

-record(sgsn_number, {
        
}).

-record(common_flags, {
        
}).

-record(apn_restriction, {
        
}).

-record(radio_priority_lcs, {
        
}).

-record(rat_type, {
        
}).

-record(user_location_information, {
        
}).

-record(ms_time_zone, {
        
}).

-record(imei, {
        
}).

-record(camel_charging_information_container, {
        
}).

-record(mbms_ue_context, {
        
}).

-record(temporary_mobile_group_identity, {
        
}).

-record(rim_routing_address, {
        
}).

-record(mbms_protocol_configuration_options, {
        
}).

-record(mbms_service_area, {
        
}).

-record(source_rnc_pdcp_context_info, {
        
}).

-record(additional_trace_info, {
        
}).

-record(hop_counter, {
        
}).

-record(selected_plmn_id, {
        
}).

-record(mbms_session_identifier, {
        
}).

-record(mbms_2g_3g_indicator, {
        
}).

-record(enhanced_nsapi, {
        
}).

-record(mbms_session_duration, {
        
}).

-record(additional_mbms_trace_info, {
        
}).

-record(mbms_session_repetition_number, {
        
}).

-record(mbms_time_to_data_transfer, {
        
}).

-record(bss_container, {
        
}).

-record(cell_identification, {
        
}).

-record(pdu_numbers, {
        
}).

-record(bssgp_cause, {
        
}).

-record(required_mbms_bearer_capabilities, {
        
}).

-record(rim_routing_address_discriminator, {
        
}).

-record(list_of_set_up_pfcs, {
        
}).

-record(ps_handover_xid_parameters, {
        
}).

-record(ms_info_change_reporting_action, {
        
}).

-record(direct_tunnel_flags, {
        
}).

-record(correlation_id, {
        
}).

-record(bearer_control_mode, {
        
}).

-record(mbms_flow_identifier, {
        
}).

-record(mbms_ip_multicast_distribution, {
        
}).

-record(mbms_distribution_acknowledgement, {
        
}).

-record(reliable_inter_rat_handover_info, {
        
}).

-record(rfsp_index, {
        
}).

-record(fully_qualified_domain_name, {
        
}).

-record(evolved_allocation_retention_priority_i, {
        
}).

-record(evolved_allocation_retention_priority_ii, {
        
}).

-record(extended_common_flags, {
        
}).

-record(user_csg_information, {
        
}).

-record(csg_information_reporting_action, {
        
}).

-record(csg_id, {
        
}).

-record(csg_membership_indication, {
        
}).

-record(aggregate_maximum_bit_rate, {
        
}).

-record(ue_network_capability, {
        
}).

-record(ue_ambr, {
        
}).

-record(apn_ambr_with_nsapi, {
        
}).

-record(ggsn_back_off_time, {
        
}).

-record(signalling_priority_indication, {
        
}).

-record(signalling_priority_indication_with_nsapi, {
        
}).

-record(higher_bitrates_than_16_mbps_flag, {
        
}).

-record(additional_mm_context_for_srvcc, {
        
}).

-record(additional_flags_for_srvcc, {
        
}).

-record(stn_sr, {
        
}).

-record(c_msisdn, {
        
}).

-record(extended_ranap_cause, {
        
}).

-record(enodeb_id, {
        
}).

-record(selection_mode_with_nsapi, {
        
}).

-record(uli_timestamp, {
        
}).

-record(local_home_network_id_with_nsapi, {
        
}).

-record(cn_operator_selection_entity, {
        
}).

-record(charging_gateway_address, {
        
}).

-record(private_extension, {
        
}).

