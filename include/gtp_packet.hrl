%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% Copyright 2015, Travelping GmbH <info@travelping.com>

-define('3GPP_RAT-TYPE_RESERVED',       0).
-define('3GPP_RAT-TYPE_UTRAN',          1).
-define('3GPP_RAT-TYPE_GERAN',          2).
-define('3GPP_RAT-TYPE_WLAN',           3).
-define('3GPP_RAT-TYPE_GAN',            4).
-define('3GPP_RAT-TYPE_HSPA EVOLUTION', 5).
-define('3GPP_RAT-TYPE_EUTRAN',         6).

-define('PCO-P-CSCF-IPv6-Address',                      16#01).
-define('PCO-IM-CN-Subsystem-Signaling-Flag',           16#02).
-define('PCO-DNS-Server-IPv6-Address',                  16#03).
-define('PCO-Policy-Control-Rejection-Code',            16#04).
-define('PCO-Bearer-Control-Mode',                      16#05).
-define('PCO-DSMIPv6-Home-Agent-Address',               16#07).
-define('PCO-DSMIPv6-Home-Network-Prefix',              16#08).
-define('PCO-DSMIPv6-IPv4-Home-Agent-Address',          16#09).
-define('PCO-IP-Address-Allocation-Via-NAS-Signalling', 16#0A).
-define('PCO-IPv4-Address-Allocation-Via-DHCPv4',       16#0B).
-define('PCO-P-CSCF-IPv4-Address',                      16#0C).
-define('PCO-DNS-Server-IPv4-Address',                  16#0D).
-define('PCO-MSISDN',                                   16#0E).
-define('PCO-IFOM-Support',                             16#0F).
-define('PCO-IPv4-Link-MTU',                            16#10).
-define('PCO-Local-Address-In-TFT-Indicator',           16#11).
-define('PCO-P-CSCF-Re-Selection-Support',              16#12).
-define('PCO-NBIFOM-Indicator',                         16#13).
-define('PCO-NBIFOM-Mode',                              16#14).
-define('PCO-Non-IP-Link-MTU',                          16#15).
-define('PCO-APN-Rate-Control',                         16#16).

-record(gtp, {
              version       :: 'undefined' | 'v1' | 'v2' |
                               'prime_v0' | 'prime_v0s' | 'prime_v1' | 'prime_v2',
              type          :: atom(),
              tei           :: 0..16#ffffffff | undefined,
              seq_no        :: 0..16#ffffff | undefined,
              n_pdu         :: 0..16#ff | undefined,
              ext_hdr = []  :: [term()],
              ie            :: [term()] | map() | binary()
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

-define(GTP_V1_IE_CAUSE, 1).

-define(GTP_V1_IE_INTERNATIONAL_MOBILE_SUBSCRIBER_IDENTITY,
        2).

-define(GTP_V1_IE_ROUTEING_AREA_IDENTITY, 3).

-define(GTP_V1_IE_TEMPORARY_LOGICAL_LINK_IDENTITY, 4).

-define(GTP_V1_IE_PACKET_TMSI, 5).

-define(GTP_V1_IE_REORDERING_REQUIRED, 8).

-define(GTP_V1_IE_AUTHENTICATION_TRIPLET, 9).

-define(GTP_V1_IE_MAP_CAUSE, 11).

-define(GTP_V1_IE_P_TMSI_SIGNATURE, 12).

-define(GTP_V1_IE_MS_VALIDATED, 13).

-define(GTP_V1_IE_RECOVERY, 14).

-define(GTP_V1_IE_SELECTION_MODE, 15).

-define(GTP_V1_IE_TUNNEL_ENDPOINT_IDENTIFIER_DATA_I,
        16).

-define(GTP_V1_IE_TUNNEL_ENDPOINT_IDENTIFIER_CONTROL_PLANE,
        17).

-define(GTP_V1_IE_TUNNEL_ENDPOINT_IDENTIFIER_DATA_II,
        18).

-define(GTP_V1_IE_TEARDOWN_IND, 19).

-define(GTP_V1_IE_NSAPI, 20).

-define(GTP_V1_IE_RANAP_CAUSE, 21).

-define(GTP_V1_IE_RAB_CONTEXT, 22).

-define(GTP_V1_IE_RADIO_PRIORITY_SMS, 23).

-define(GTP_V1_IE_RADIO_PRIORITY, 24).

-define(GTP_V1_IE_PACKET_FLOW_ID, 25).

-define(GTP_V1_IE_CHARGING_CHARACTERISTICS, 26).

-define(GTP_V1_IE_TRACE_REFERENCE, 27).

-define(GTP_V1_IE_TRACE_TYPE, 28).

-define(GTP_V1_IE_MS_NOT_REACHABLE_REASON, 29).

-define(GTP_V1_IE_PACKET_TRANSFER_COMMAND, 126).

-define(GTP_V1_IE_CHARGING_ID, 127).

-define(GTP_V1_IE_END_USER_ADDRESS, 128).

-define(GTP_V1_IE_MM_CONTEXT_GSM, 129).

-define(GTP_V1_IE_MM_CONTEXT_UMTS, 129).

-define(GTP_V1_IE_MM_CONTEXT_GSM_AND_UMTS, 129).

-define(GTP_V1_IE_MM_CONTEXT_UMTS_AND_USED_CIPHER, 129).

-define(GTP_V1_IE_PDP_CONTEXT, 130).

-define(GTP_V1_IE_ACCESS_POINT_NAME, 131).

-define(GTP_V1_IE_PROTOCOL_CONFIGURATION_OPTIONS, 132).

-define(GTP_V1_IE_GSN_ADDRESS, 133).

-define(GTP_V1_IE_MS_INTERNATIONAL_PSTN_ISDN_NUMBER,
        134).

-define(GTP_V1_IE_QUALITY_OF_SERVICE_PROFILE, 135).

-define(GTP_V1_IE_AUTHENTICATION_QUINTUPLET, 136).

-define(GTP_V1_IE_TRAFFIC_FLOW_TEMPLATE, 137).

-define(GTP_V1_IE_TARGET_IDENTIFICATION, 138).

-define(GTP_V1_IE_UTRAN_TRANSPARENT_CONTAINER, 139).

-define(GTP_V1_IE_RAB_SETUP_INFORMATION, 140).

-define(GTP_V1_IE_EXTENSION_HEADER_TYPE_LIST, 141).

-define(GTP_V1_IE_TRIGGER_ID, 142).

-define(GTP_V1_IE_OMC_IDENTITY, 143).

-define(GTP_V1_IE_RAN_TRANSPARENT_CONTAINER, 144).

-define(GTP_V1_IE_PDP_CONTEXT_PRIORITIZATION, 145).

-define(GTP_V1_IE_ADDITIONAL_RAB_SETUP_INFORMATION,
        146).

-define(GTP_V1_IE_SGSN_NUMBER, 147).

-define(GTP_V1_IE_COMMON_FLAGS, 148).

-define(GTP_V1_IE_APN_RESTRICTION, 149).

-define(GTP_V1_IE_RADIO_PRIORITY_LCS, 150).

-define(GTP_V1_IE_RAT_TYPE, 151).

-define(GTP_V1_IE_USER_LOCATION_INFORMATION, 152).

-define(GTP_V1_IE_MS_TIME_ZONE, 153).

-define(GTP_V1_IE_IMEI, 154).

-define(GTP_V1_IE_CAMEL_CHARGING_INFORMATION_CONTAINER,
        155).

-define(GTP_V1_IE_MBMS_UE_CONTEXT, 156).

-define(GTP_V1_IE_TEMPORARY_MOBILE_GROUP_IDENTITY, 157).

-define(GTP_V1_IE_RIM_ROUTING_ADDRESS, 158).

-define(GTP_V1_IE_MBMS_PROTOCOL_CONFIGURATION_OPTIONS,
        159).

-define(GTP_V1_IE_MBMS_SERVICE_AREA, 160).

-define(GTP_V1_IE_SOURCE_RNC_PDCP_CONTEXT_INFO, 161).

-define(GTP_V1_IE_ADDITIONAL_TRACE_INFO, 162).

-define(GTP_V1_IE_HOP_COUNTER, 163).

-define(GTP_V1_IE_SELECTED_PLMN_ID, 164).

-define(GTP_V1_IE_MBMS_SESSION_IDENTIFIER, 165).

-define(GTP_V1_IE_MBMS_2G_3G_INDICATOR, 166).

-define(GTP_V1_IE_ENHANCED_NSAPI, 167).

-define(GTP_V1_IE_MBMS_SESSION_DURATION, 168).

-define(GTP_V1_IE_ADDITIONAL_MBMS_TRACE_INFO, 169).

-define(GTP_V1_IE_MBMS_SESSION_REPETITION_NUMBER, 170).

-define(GTP_V1_IE_MBMS_TIME_TO_DATA_TRANSFER, 171).

-define(GTP_V1_IE_BSS_CONTAINER, 173).

-define(GTP_V1_IE_CELL_IDENTIFICATION, 174).

-define(GTP_V1_IE_PDU_NUMBERS, 175).

-define(GTP_V1_IE_BSSGP_CAUSE, 176).

-define(GTP_V1_IE_REQUIRED_MBMS_BEARER_CAPABILITIES,
        177).

-define(GTP_V1_IE_RIM_ROUTING_ADDRESS_DISCRIMINATOR,
        178).

-define(GTP_V1_IE_LIST_OF_SET_UP_PFCS, 179).

-define(GTP_V1_IE_PS_HANDOVER_XID_PARAMETERS, 180).

-define(GTP_V1_IE_MS_INFO_CHANGE_REPORTING_ACTION, 181).

-define(GTP_V1_IE_DIRECT_TUNNEL_FLAGS, 182).

-define(GTP_V1_IE_CORRELATION_ID, 183).

-define(GTP_V1_IE_BEARER_CONTROL_MODE, 184).

-define(GTP_V1_IE_MBMS_FLOW_IDENTIFIER, 185).

-define(GTP_V1_IE_MBMS_IP_MULTICAST_DISTRIBUTION, 186).

-define(GTP_V1_IE_MBMS_DISTRIBUTION_ACKNOWLEDGEMENT,
        187).

-define(GTP_V1_IE_RELIABLE_INTER_RAT_HANDOVER_INFO,
        188).

-define(GTP_V1_IE_RFSP_INDEX, 189).

-define(GTP_V1_IE_FULLY_QUALIFIED_DOMAIN_NAME, 190).

-define(GTP_V1_IE_EVOLVED_ALLOCATION_RETENTION_PRIORITY_I,
        191).

-define(GTP_V1_IE_EVOLVED_ALLOCATION_RETENTION_PRIORITY_II,
        192).

-define(GTP_V1_IE_EXTENDED_COMMON_FLAGS, 193).

-define(GTP_V1_IE_USER_CSG_INFORMATION, 194).

-define(GTP_V1_IE_CSG_INFORMATION_REPORTING_ACTION,
        195).

-define(GTP_V1_IE_CSG_ID, 196).

-define(GTP_V1_IE_CSG_MEMBERSHIP_INDICATION, 197).

-define(GTP_V1_IE_AGGREGATE_MAXIMUM_BIT_RATE, 198).

-define(GTP_V1_IE_UE_NETWORK_CAPABILITY, 199).

-define(GTP_V1_IE_UE_AMBR, 200).

-define(GTP_V1_IE_APN_AMBR_WITH_NSAPI, 201).

-define(GTP_V1_IE_GGSN_BACK_OFF_TIME, 202).

-define(GTP_V1_IE_SIGNALLING_PRIORITY_INDICATION, 203).

-define(GTP_V1_IE_SIGNALLING_PRIORITY_INDICATION_WITH_NSAPI,
        204).

-define(GTP_V1_IE_HIGHER_BITRATES_THAN_16_MBPS_FLAG,
        205).

-define(GTP_V1_IE_ADDITIONAL_MM_CONTEXT_FOR_SRVCC, 207).

-define(GTP_V1_IE_ADDITIONAL_FLAGS_FOR_SRVCC, 208).

-define(GTP_V1_IE_STN_SR, 209).

-define(GTP_V1_IE_C_MSISDN, 210).

-define(GTP_V1_IE_EXTENDED_RANAP_CAUSE, 211).

-define(GTP_V1_IE_ENODEB_ID, 212).

-define(GTP_V1_IE_SELECTION_MODE_WITH_NSAPI, 213).

-define(GTP_V1_IE_ULI_TIMESTAMP, 214).

-define(GTP_V1_IE_LOCAL_HOME_NETWORK_ID_WITH_NSAPI,
        215).

-define(GTP_V1_IE_CN_OPERATOR_SELECTION_ENTITY, 216).

-define(GTP_V1_IE_SEQUENCE_NUMBERS_OF_RELEASED_PACKETS,
        249).

-define(GTP_V1_IE_SEQUENCE_NUMBERS_OF_CANCELLED_PACKETS,
        250).

-define(GTP_V1_IE_CHARGING_GATEWAY_ADDRESS, 251).

-define(GTP_V1_IE_DATA_RECORD_PACKET, 252).

-define(GTP_V1_IE_REQUESTS_RESPONDED, 253).

-define(GTP_V1_IE_ADDRESS_OF_RECOMMENDED_NODE, 254).

-define(GTP_V1_IE_PRIVATE_EXTENSION, 255).

-define(GTP_V1_RECORDS,
        [cause,
         international_mobile_subscriber_identity,
         routeing_area_identity,
         temporary_logical_link_identity,
         packet_tmsi,
         reordering_required,
         authentication_triplet,
         map_cause,
         p_tmsi_signature,
         ms_validated,
         recovery,
         selection_mode,
         tunnel_endpoint_identifier_data_i,
         tunnel_endpoint_identifier_control_plane,
         tunnel_endpoint_identifier_data_ii,
         teardown_ind,
         nsapi,
         ranap_cause,
         rab_context,
         radio_priority_sms,
         radio_priority,
         packet_flow_id,
         charging_characteristics,
         trace_reference,
         trace_type,
         ms_not_reachable_reason,
         packet_transfer_command,
         charging_id,
         end_user_address,
         mm_context_gsm,
         mm_context_umts,
         mm_context_gsm_and_umts,
         mm_context_umts_and_used_cipher,
         pdp_context,
         access_point_name,
         protocol_configuration_options,
         gsn_address,
         ms_international_pstn_isdn_number,
         quality_of_service_profile,
         authentication_quintuplet,
         traffic_flow_template,
         target_identification,
         utran_transparent_container,
         rab_setup_information,
         extension_header_type_list,
         trigger_id,
         omc_identity,
         ran_transparent_container,
         pdp_context_prioritization,
         additional_rab_setup_information,
         sgsn_number,
         common_flags,
         apn_restriction,
         radio_priority_lcs,
         rat_type,
         user_location_information,
         ms_time_zone,
         imei,
         camel_charging_information_container,
         mbms_ue_context,
         temporary_mobile_group_identity,
         rim_routing_address,
         mbms_protocol_configuration_options,
         mbms_service_area,
         source_rnc_pdcp_context_info,
         additional_trace_info,
         hop_counter,
         selected_plmn_id,
         mbms_session_identifier,
         mbms_2g_3g_indicator,
         enhanced_nsapi,
         mbms_session_duration,
         additional_mbms_trace_info,
         mbms_session_repetition_number,
         mbms_time_to_data_transfer,
         bss_container,
         cell_identification,
         pdu_numbers,
         bssgp_cause,
         required_mbms_bearer_capabilities,
         rim_routing_address_discriminator,
         list_of_set_up_pfcs,
         ps_handover_xid_parameters,
         ms_info_change_reporting_action,
         direct_tunnel_flags,
         correlation_id,
         bearer_control_mode,
         mbms_flow_identifier,
         mbms_ip_multicast_distribution,
         mbms_distribution_acknowledgement,
         reliable_inter_rat_handover_info,
         rfsp_index,
         fully_qualified_domain_name,
         evolved_allocation_retention_priority_i,
         evolved_allocation_retention_priority_ii,
         extended_common_flags,
         user_csg_information,
         csg_information_reporting_action,
         csg_id,
         csg_membership_indication,
         aggregate_maximum_bit_rate,
         ue_network_capability,
         ue_ambr,
         apn_ambr_with_nsapi,
         ggsn_back_off_time,
         signalling_priority_indication,
         signalling_priority_indication_with_nsapi,
         higher_bitrates_than_16_mbps_flag,
         additional_mm_context_for_srvcc,
         additional_flags_for_srvcc,
         stn_sr,
         c_msisdn,
         extended_ranap_cause,
         enodeb_id,
         selection_mode_with_nsapi,
         uli_timestamp,
         local_home_network_id_with_nsapi,
         cn_operator_selection_entity,
         sequence_numbers_of_released_packets,
         sequence_numbers_of_cancelled_packets,
         charging_gateway_address,
         data_record_packet,
         requests_responded,
         address_of_recommended_node,
         private_extension]).

-record(cause,
        {instance = 0 :: non_neg_integer(),
         value = request_imsi}).

-record(international_mobile_subscriber_identity,
        {instance = 0 :: non_neg_integer(), imsi}).

-record(temporary_logical_link_identity,
        {instance = 0 :: non_neg_integer(),
         tlli = <<0:4/unit:8>>}).

-record(packet_tmsi,
        {instance = 0 :: non_neg_integer(),
         p_tmsi = <<0:4/unit:8>>}).

-record(reordering_required,
        {instance = 0 :: non_neg_integer(), required = no}).

-record(authentication_triplet,
        {instance = 0 :: non_neg_integer(),
         rand = <<0:16/unit:8>>,
         sres = <<0:4/unit:8>>,
         kc = <<0:8/unit:8>>}).

-record(map_cause,
        {instance = 0 :: non_neg_integer(),
         value = <<0:1/unit:8>>}).

-record(p_tmsi_signature,
        {instance = 0 :: non_neg_integer(),
         value = <<0:3/unit:8>>}).

-record(ms_validated,
        {instance = 0 :: non_neg_integer(), validated = no}).

-record(recovery,
        {instance = 0 :: non_neg_integer(),
         restart_counter = 0 :: non_neg_integer()}).

-record(selection_mode,
        {instance = 0 :: non_neg_integer(),
         mode = 0 :: non_neg_integer()}).

-record(tunnel_endpoint_identifier_data_i,
        {instance = 0 :: non_neg_integer(),
         tei = 0 :: non_neg_integer()}).

-record(tunnel_endpoint_identifier_control_plane,
        {instance = 0 :: non_neg_integer(),
         tei = 0 :: non_neg_integer()}).

-record(tunnel_endpoint_identifier_data_ii,
        {instance = 0 :: non_neg_integer(),
         nsapi = 0 :: non_neg_integer(),
         tei = 0 :: non_neg_integer()}).

-record(teardown_ind,
        {instance = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(nsapi,
        {instance = 0 :: non_neg_integer(),
         nsapi = 0 :: non_neg_integer()}).

-record(ranap_cause,
        {instance = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(rab_context,
        {instance = 0 :: non_neg_integer(),
         nsapi = 0 :: non_neg_integer(),
         dl_gtp_u_sequence_number = 0 :: non_neg_integer(),
         ul_gtp_u_sequence_number = 0 :: non_neg_integer(),
         dl_pdcp_sequence_number = 0 :: non_neg_integer(),
         ul_pdcp_sequence_number = 0 :: non_neg_integer()}).

-record(radio_priority_sms,
        {instance = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(radio_priority,
        {instance = 0 :: non_neg_integer(),
         nsapi = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(packet_flow_id,
        {instance = 0 :: non_neg_integer(),
         nsapi = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(charging_characteristics,
        {instance = 0 :: non_neg_integer(),
         value = <<0:2/unit:8>>}).

-record(trace_reference,
        {instance = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(trace_type,
        {instance = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(ms_not_reachable_reason,
        {instance = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(packet_transfer_command,
        {instance = 0 :: non_neg_integer(),
         command = send_data_record_packet}).

-record(charging_id,
        {instance = 0 :: non_neg_integer(),
         id = <<0:4/unit:8>>}).

-record(end_user_address,
        {instance = 0 :: non_neg_integer(),
         pdp_type_organization = 0 :: non_neg_integer(),
         pdp_type_number = 0 :: non_neg_integer(),
         pdp_address = <<>>}).

-record(mm_context_gsm,
        {instance = 0 :: non_neg_integer(),
         cksn = 0 :: non_neg_integer(),
         no_of_vectors = 0 :: non_neg_integer(),
         used_cipher = 0 :: non_neg_integer(),
         kc = <<0:8/unit:8>>,
         tripple = [],
         drx_parameter = <<0:2/unit:8>>,
         ms_network_capability_length = 0 :: non_neg_integer(),
         ms_network_capability = [],
         container_length = 0 :: non_neg_integer(),
         container = []}).

-record(mm_context_umts,
        {instance = 0 :: non_neg_integer(),
         ksi = 0 :: non_neg_integer(),
         no_of_vectors = 0 :: non_neg_integer(),
         ck = <<0:16/unit:8>>,
         ik = <<0:16/unit:8>>,
         quintuplet_length = 0 :: non_neg_integer(),
         quintuplet = [],
         drx_parameter = <<0:2/unit:8>>,
         ms_network_capability_length = 0 :: non_neg_integer(),
         ms_network_capability = [],
         container_length = 0 :: non_neg_integer(),
         container = []}).

-record(mm_context_gsm_and_umts,
        {instance = 0 :: non_neg_integer(),
         cksn = 0 :: non_neg_integer(),
         no_of_vectors = 0 :: non_neg_integer(),
         used_cipher = 0 :: non_neg_integer(),
         kc = <<0:8/unit:8>>,
         quintuplet_length = 0 :: non_neg_integer(),
         quintuplet = [],
         drx_parameter = <<0:2/unit:8>>,
         ms_network_capability_length = 0 :: non_neg_integer(),
         ms_network_capability = [],
         container_length = 0 :: non_neg_integer(),
         container = []}).

-record(mm_context_umts_and_used_cipher,
        {instance = 0 :: non_neg_integer(),
         ksi = 0 :: non_neg_integer(),
         no_of_vectors = 0 :: non_neg_integer(),
         used_cipher = 0 :: non_neg_integer(),
         ck = <<0:16/unit:8>>,
         ik = <<0:16/unit:8>>,
         quintuplet_length = 0 :: non_neg_integer(),
         quintuplet = [],
         drx_parameter = <<0:2/unit:8>>,
         ms_network_capability_length = 0 :: non_neg_integer(),
         ms_network_capability = [],
         container_length = 0 :: non_neg_integer(),
         container = []}).

-record(pdp_context,
        {instance = 0 :: non_neg_integer()}).

-record(access_point_name,
        {instance = 0 :: non_neg_integer(), apn}).

-record(protocol_configuration_options,
        {instance = 0 :: non_neg_integer(), config}).

-record(gsn_address,
        {instance = 0 :: non_neg_integer(), address = <<>>}).

-record(ms_international_pstn_isdn_number,
        {instance = 0 :: non_neg_integer(), msisdn}).

-record(quality_of_service_profile,
        {instance = 0 :: non_neg_integer(),
         priority = 0 :: non_neg_integer(),
         data = <<>>}).

-record(authentication_quintuplet,
        {instance = 0 :: non_neg_integer()}).

-record(traffic_flow_template,
        {instance = 0 :: non_neg_integer()}).

-record(target_identification,
        {instance = 0 :: non_neg_integer()}).

-record(utran_transparent_container,
        {instance = 0 :: non_neg_integer()}).

-record(rab_setup_information,
        {instance = 0 :: non_neg_integer()}).

-record(extension_header_type_list,
        {instance = 0 :: non_neg_integer()}).

-record(trigger_id,
        {instance = 0 :: non_neg_integer()}).

-record(omc_identity,
        {instance = 0 :: non_neg_integer()}).

-record(ran_transparent_container,
        {instance = 0 :: non_neg_integer()}).

-record(pdp_context_prioritization,
        {instance = 0 :: non_neg_integer()}).

-record(additional_rab_setup_information,
        {instance = 0 :: non_neg_integer()}).

-record(sgsn_number,
        {instance = 0 :: non_neg_integer()}).

-record(common_flags,
        {instance = 0 :: non_neg_integer(), flags = #{}}).

-record(apn_restriction,
        {instance = 0 :: non_neg_integer(),
         restriction_type_value = 0 :: non_neg_integer()}).

-record(radio_priority_lcs,
        {instance = 0 :: non_neg_integer()}).

-record(rat_type,
        {instance = 0 :: non_neg_integer(),
         rat_type = 0 :: non_neg_integer()}).

-record(ms_time_zone,
        {instance = 0 :: non_neg_integer(),
         timezone = 0 :: non_neg_integer(),
         dst = 0 :: non_neg_integer()}).

-record(imei,
        {instance = 0 :: non_neg_integer(), imei}).

-record(camel_charging_information_container,
        {instance = 0 :: non_neg_integer()}).

-record(mbms_ue_context,
        {instance = 0 :: non_neg_integer()}).

-record(temporary_mobile_group_identity,
        {instance = 0 :: non_neg_integer()}).

-record(rim_routing_address,
        {instance = 0 :: non_neg_integer()}).

-record(mbms_protocol_configuration_options,
        {instance = 0 :: non_neg_integer()}).

-record(mbms_service_area,
        {instance = 0 :: non_neg_integer()}).

-record(source_rnc_pdcp_context_info,
        {instance = 0 :: non_neg_integer()}).

-record(additional_trace_info,
        {instance = 0 :: non_neg_integer()}).

-record(hop_counter,
        {instance = 0 :: non_neg_integer()}).

-record(selected_plmn_id,
        {instance = 0 :: non_neg_integer()}).

-record(mbms_session_identifier,
        {instance = 0 :: non_neg_integer()}).

-record(mbms_2g_3g_indicator,
        {instance = 0 :: non_neg_integer()}).

-record(enhanced_nsapi,
        {instance = 0 :: non_neg_integer()}).

-record(mbms_session_duration,
        {instance = 0 :: non_neg_integer()}).

-record(additional_mbms_trace_info,
        {instance = 0 :: non_neg_integer()}).

-record(mbms_session_repetition_number,
        {instance = 0 :: non_neg_integer()}).

-record(mbms_time_to_data_transfer,
        {instance = 0 :: non_neg_integer()}).

-record(bss_container,
        {instance = 0 :: non_neg_integer()}).

-record(cell_identification,
        {instance = 0 :: non_neg_integer()}).

-record(pdu_numbers,
        {instance = 0 :: non_neg_integer()}).

-record(bssgp_cause,
        {instance = 0 :: non_neg_integer()}).

-record(required_mbms_bearer_capabilities,
        {instance = 0 :: non_neg_integer()}).

-record(rim_routing_address_discriminator,
        {instance = 0 :: non_neg_integer()}).

-record(list_of_set_up_pfcs,
        {instance = 0 :: non_neg_integer()}).

-record(ps_handover_xid_parameters,
        {instance = 0 :: non_neg_integer()}).

-record(ms_info_change_reporting_action,
        {instance = 0 :: non_neg_integer(),
         action = stop_reporting}).

-record(direct_tunnel_flags,
        {instance = 0 :: non_neg_integer()}).

-record(correlation_id,
        {instance = 0 :: non_neg_integer()}).

-record(bearer_control_mode,
        {instance = 0 :: non_neg_integer()}).

-record(mbms_flow_identifier,
        {instance = 0 :: non_neg_integer()}).

-record(mbms_ip_multicast_distribution,
        {instance = 0 :: non_neg_integer()}).

-record(mbms_distribution_acknowledgement,
        {instance = 0 :: non_neg_integer()}).

-record(reliable_inter_rat_handover_info,
        {instance = 0 :: non_neg_integer()}).

-record(rfsp_index,
        {instance = 0 :: non_neg_integer()}).

-record(fully_qualified_domain_name,
        {instance = 0 :: non_neg_integer(), fqdn}).

-record(evolved_allocation_retention_priority_i,
        {instance = 0 :: non_neg_integer(),
         pci = 0 :: non_neg_integer(),
         pl = 0 :: non_neg_integer(),
         pvi = 0 :: non_neg_integer()}).

-record(evolved_allocation_retention_priority_ii,
        {instance = 0 :: non_neg_integer()}).

-record(extended_common_flags,
        {instance = 0 :: non_neg_integer(), flags = #{}}).

-record(user_csg_information,
        {instance = 0 :: non_neg_integer()}).

-record(csg_information_reporting_action,
        {instance = 0 :: non_neg_integer()}).

-record(csg_id, {instance = 0 :: non_neg_integer()}).

-record(csg_membership_indication,
        {instance = 0 :: non_neg_integer()}).

-record(aggregate_maximum_bit_rate,
        {instance = 0 :: non_neg_integer(),
         uplink = 0 :: non_neg_integer(),
         downlink = 0 :: non_neg_integer()}).

-record(ue_network_capability,
        {instance = 0 :: non_neg_integer()}).

-record(ue_ambr, {instance = 0 :: non_neg_integer()}).

-record(apn_ambr_with_nsapi,
        {instance = 0 :: non_neg_integer()}).

-record(ggsn_back_off_time,
        {instance = 0 :: non_neg_integer()}).

-record(signalling_priority_indication,
        {instance = 0 :: non_neg_integer()}).

-record(signalling_priority_indication_with_nsapi,
        {instance = 0 :: non_neg_integer()}).

-record(higher_bitrates_than_16_mbps_flag,
        {instance = 0 :: non_neg_integer()}).

-record(additional_mm_context_for_srvcc,
        {instance = 0 :: non_neg_integer()}).

-record(additional_flags_for_srvcc,
        {instance = 0 :: non_neg_integer()}).

-record(stn_sr, {instance = 0 :: non_neg_integer()}).

-record(c_msisdn, {instance = 0 :: non_neg_integer()}).

-record(extended_ranap_cause,
        {instance = 0 :: non_neg_integer()}).

-record(enodeb_id, {instance = 0 :: non_neg_integer()}).

-record(selection_mode_with_nsapi,
        {instance = 0 :: non_neg_integer()}).

-record(uli_timestamp,
        {instance = 0 :: non_neg_integer()}).

-record(local_home_network_id_with_nsapi,
        {instance = 0 :: non_neg_integer()}).

-record(cn_operator_selection_entity,
        {instance = 0 :: non_neg_integer()}).

-record(sequence_numbers_of_released_packets,
        {instance = 0 :: non_neg_integer(), sequence_numbers}).

-record(sequence_numbers_of_cancelled_packets,
        {instance = 0 :: non_neg_integer(), sequence_numbers}).

-record(charging_gateway_address,
        {instance = 0 :: non_neg_integer(), address = <<>>}).

-record(requests_responded,
        {instance = 0 :: non_neg_integer(), sequence_numbers}).

-record(address_of_recommended_node,
        {instance = 0 :: non_neg_integer(), address = <<>>}).

-record(private_extension,
        {instance = 0 :: non_neg_integer(),
         enterprise_id = 0 :: non_neg_integer(),
         value = <<>>}).

%% -include("gtp_packet_v2_gen.hrl").

-define(GTP_V2_IE_V2_INTERNATIONAL_MOBILE_SUBSCRIBER_IDENTITY,
        1).

-define(GTP_V2_IE_V2_CAUSE, 2).

-define(GTP_V2_IE_V2_RECOVERY, 3).

-define(GTP_V2_IE_V2_STN_SR, 51).

-define(GTP_V2_IE_V2_ACCESS_POINT_NAME, 71).

-define(GTP_V2_IE_V2_AGGREGATE_MAXIMUM_BIT_RATE, 72).

-define(GTP_V2_IE_V2_EPS_BEARER_ID, 73).

-define(GTP_V2_IE_V2_IP_ADDRESS, 74).

-define(GTP_V2_IE_V2_MOBILE_EQUIPMENT_IDENTITY, 75).

-define(GTP_V2_IE_V2_MSISDN, 76).

-define(GTP_V2_IE_V2_INDICATION, 77).

-define(GTP_V2_IE_V2_PROTOCOL_CONFIGURATION_OPTIONS,
        78).

-define(GTP_V2_IE_V2_PDN_ADDRESS_ALLOCATION, 79).

-define(GTP_V2_IE_V2_BEARER_LEVEL_QUALITY_OF_SERVICE,
        80).

-define(GTP_V2_IE_V2_FLOW_QUALITY_OF_SERVICE, 81).

-define(GTP_V2_IE_V2_RAT_TYPE, 82).

-define(GTP_V2_IE_V2_SERVING_NETWORK, 83).

-define(GTP_V2_IE_V2_EPS_BEARER_LEVEL_TRAFFIC_FLOW_TEMPLATE,
        84).

-define(GTP_V2_IE_V2_TRAFFIC_AGGREGATION_DESCRIPTION,
        85).

-define(GTP_V2_IE_V2_USER_LOCATION_INFORMATION, 86).

-define(GTP_V2_IE_V2_FULLY_QUALIFIED_TUNNEL_ENDPOINT_IDENTIFIER,
        87).

-define(GTP_V2_IE_V2_TMSI, 88).

-define(GTP_V2_IE_V2_GLOBAL_CN_ID, 89).

-define(GTP_V2_IE_V2_S103_PDN_DATA_FORWARDING_INFO, 90).

-define(GTP_V2_IE_V2_S1_U_DATA_FORWARDING_INFO, 91).

-define(GTP_V2_IE_V2_DELAY_VALUE, 92).

-define(GTP_V2_IE_V2_BEARER_CONTEXT, 93).

-define(GTP_V2_IE_V2_CHARGING_ID, 94).

-define(GTP_V2_IE_V2_CHARGING_CHARACTERISTICS, 95).

-define(GTP_V2_IE_V2_TRACE_INFORMATION, 96).

-define(GTP_V2_IE_V2_BEARER_FLAGS, 97).

-define(GTP_V2_IE_V2_PDN_TYPE, 99).

-define(GTP_V2_IE_V2_PROCEDURE_TRANSACTION_ID, 100).

-define(GTP_V2_IE_V2_MM_CONTEXT_1, 103).

-define(GTP_V2_IE_V2_MM_CONTEXT_2, 104).

-define(GTP_V2_IE_V2_MM_CONTEXT_3, 105).

-define(GTP_V2_IE_V2_MM_CONTEXT_4, 106).

-define(GTP_V2_IE_V2_MM_CONTEXT_5, 107).

-define(GTP_V2_IE_V2_MM_CONTEXT_6, 108).

-define(GTP_V2_IE_V2_PDN_CONNECTION, 109).

-define(GTP_V2_IE_V2_PDU_NUMBERS, 110).

-define(GTP_V2_IE_V2_P_TMSI, 111).

-define(GTP_V2_IE_V2_P_TMSI_SIGNATURE, 112).

-define(GTP_V2_IE_V2_HOP_COUNTER, 113).

-define(GTP_V2_IE_V2_UE_TIME_ZONE, 114).

-define(GTP_V2_IE_V2_TRACE_REFERENCE, 115).

-define(GTP_V2_IE_V2_COMPLETE_REQUEST_MESSAGE, 116).

-define(GTP_V2_IE_V2_GUTI, 117).

-define(GTP_V2_IE_V2_F_CONTAINER, 118).

-define(GTP_V2_IE_V2_F_CAUSE, 119).

-define(GTP_V2_IE_V2_PLMN_ID, 120).

-define(GTP_V2_IE_V2_TARGET_IDENTIFICATION, 121).

-define(GTP_V2_IE_V2_PACKET_FLOW_ID, 123).

-define(GTP_V2_IE_V2_RAB_CONTEXT, 124).

-define(GTP_V2_IE_V2_SOURCE_RNC_PDCP_CONTEXT_INFO, 125).

-define(GTP_V2_IE_V2_UDP_SOURCE_PORT_NUMBER, 126).

-define(GTP_V2_IE_V2_APN_RESTRICTION, 127).

-define(GTP_V2_IE_V2_SELECTION_MODE, 128).

-define(GTP_V2_IE_V2_SOURCE_IDENTIFICATION, 129).

-define(GTP_V2_IE_V2_CHANGE_REPORTING_ACTION, 131).

-define(GTP_V2_IE_V2_FULLY_QUALIFIED_PDN_CONNECTION_SET_IDENTIFIER,
        132).

-define(GTP_V2_IE_V2_CHANNEL_NEEDED, 133).

-define(GTP_V2_IE_V2_EMLPP_PRIORITY, 134).

-define(GTP_V2_IE_V2_NODE_TYPE, 135).

-define(GTP_V2_IE_V2_FULLY_QUALIFIED_DOMAIN_NAME, 136).

-define(GTP_V2_IE_V2_TRANSACTION_IDENTIFIER, 137).

-define(GTP_V2_IE_V2_MBMS_SESSION_DURATION, 138).

-define(GTP_V2_IE_V2_MBMS_SERVICE_AREA, 139).

-define(GTP_V2_IE_V2_MBMS_SESSION_IDENTIFIER, 140).

-define(GTP_V2_IE_V2_MBMS_FLOW_IDENTIFIER, 141).

-define(GTP_V2_IE_V2_MBMS_IP_MULTICAST_DISTRIBUTION,
        142).

-define(GTP_V2_IE_V2_MBMS_DISTRIBUTION_ACKNOWLEDGE,
        143).

-define(GTP_V2_IE_V2_RFSP_INDEX, 144).

-define(GTP_V2_IE_V2_USER_CSG_INFORMATION, 145).

-define(GTP_V2_IE_V2_CSG_INFORMATION_REPORTING_ACTION,
        146).

-define(GTP_V2_IE_V2_CSG_ID, 147).

-define(GTP_V2_IE_V2_CSG_MEMBERSHIP_INDICATION, 148).

-define(GTP_V2_IE_V2_SERVICE_INDICATOR, 149).

-define(GTP_V2_IE_V2_DETACH_TYPE, 150).

-define(GTP_V2_IE_V2_LOCAL_DISTIGUISHED_NAME, 151).

-define(GTP_V2_IE_V2_NODE_FEATURES, 152).

-define(GTP_V2_IE_V2_MBMS_TIME_TO_DATA_TRANSFER, 153).

-define(GTP_V2_IE_V2_THROTTLING, 154).

-define(GTP_V2_IE_V2_ALLOCATION_RETENTION_PRIORITY,
        155).

-define(GTP_V2_IE_V2_EPC_TIMER, 156).

-define(GTP_V2_IE_V2_SIGNALLING_PRIORITY_INDICATION,
        157).

-define(GTP_V2_IE_V2_TEMPORARY_MOBILE_GROUP_IDENTITY,
        158).

-define(GTP_V2_IE_V2_ADDITIONAL_MM_CONTEXT_FOR_SRVCC,
        159).

-define(GTP_V2_IE_V2_ADDITIONAL_FLAGS_FOR_SRVCC, 160).

-define(GTP_V2_IE_V2_MDT_CONFIGURATION, 162).

-define(GTP_V2_IE_V2_ADDITIONAL_PROTOCOL_CONFIGURATION_OPTIONS,
        163).

-define(GTP_V2_IE_V2_ABSOLUTE_TIME_OF_MBMS_DATA_TRANSFER,
        164).

-define(GTP_V2_IE_V2_HENB_INFORMATION_REPORTING_, 165).

-define(GTP_V2_IE_V2_IPV4_CONFIGURATION_PARAMETERS,
        166).

-define(GTP_V2_IE_V2_CHANGE_TO_REPORT_FLAGS_, 167).

-define(GTP_V2_IE_V2_ACTION_INDICATION, 168).

-define(GTP_V2_IE_V2_TWAN_IDENTIFIER, 169).

-define(GTP_V2_IE_V2_ULI_TIMESTAMP, 170).

-define(GTP_V2_IE_V2_MBMS_FLAGS, 171).

-define(GTP_V2_IE_V2_RAN_NAS_CAUSE, 172).

-define(GTP_V2_IE_V2_CN_OPERATOR_SELECTION_ENTITY, 173).

-define(GTP_V2_IE_V2_TRUSTED_WLAN_MODE_INDICATION, 174).

-define(GTP_V2_IE_V2_NODE_NUMBER, 175).

-define(GTP_V2_IE_V2_NODE_IDENTIFIER, 176).

-define(GTP_V2_IE_V2_PRESENCE_REPORTING_AREA_ACTION,
        177).

-define(GTP_V2_IE_V2_PRESENCE_REPORTING_AREA_INFORMATION,
        178).

-define(GTP_V2_IE_V2_TWAN_IDENTIFIER_TIMESTAMP, 179).

-define(GTP_V2_IE_V2_OVERLOAD_CONTROL_INFORMATION, 180).

-define(GTP_V2_IE_V2_LOAD_CONTROL_INFORMATION, 181).

-define(GTP_V2_IE_V2_METRIC, 182).

-define(GTP_V2_IE_V2_SEQUENCE_NUMBER, 183).

-define(GTP_V2_IE_V2_APN_AND_RELATIVE_CAPACITY, 184).

-define(GTP_V2_IE_V2_WLAN_OFFLOADABILITY_INDICATION,
        185).

-define(GTP_V2_IE_V2_PAGING_AND_SERVICE_INFORMATION,
        186).

-define(GTP_V2_IE_V2_INTEGER_NUMBER, 187).

-define(GTP_V2_IE_V2_MILLISECOND_TIME_STAMP, 188).

-define(GTP_V2_IE_V2_MONITORING_EVENT_INFORMATION, 189).

-define(GTP_V2_IE_V2_ECGI_LIST, 190).

-define(GTP_V2_IE_V2_REMOTE_UE_CONTEXT, 191).

-define(GTP_V2_IE_V2_REMOTE_USER_ID, 192).

-define(GTP_V2_IE_V2_REMOTE_UE_IP_INFORMATION, 193).

-define(GTP_V2_IE_V2_CIOT_OPTIMIZATIONS_SUPPORT_INDICATION,
        194).

-define(GTP_V2_IE_V2_SCEF_PDN_CONNECTION, 195).

-define(GTP_V2_IE_V2_HEADER_COMPRESSION_CONFIGURATION,
        196).

-define(GTP_V2_IE_V2_EXTENDED_PROTOCOL_CONFIGURATION_OPTIONS,
        197).

-define(GTP_V2_IE_V2_SERVING_PLMN_RATE_CONTROL, 198).

-define(GTP_V2_IE_V2_COUNTER, 199).

-define(GTP_V2_IE_V2_MAPPED_UE_USAGE_TYPE, 200).

-define(GTP_V2_IE_V2_SECONDARY_RAT_USAGE_DATA_REPORT,
        201).

-define(GTP_V2_IE_V2_UP_FUNCTION_SELECTION_INDICATION_FLAGS,
        202).

-define(GTP_V2_IE_V2_MAXIMUM_PACKET_LOSS_RATE, 203).

-define(GTP_V2_IE_V2_APN_RATE_CONTROL_STATUS, 204).

-define(GTP_V2_IE_V2_EXTENDED_TRACE_INFORMATION, 205).

-define(GTP_V2_IE_V2_MONITORING_EVENT_EXTENSION_INFORMATION,
        206).

-define(GTP_V2_IE_V2_ADDITIONAL_RRM_POLICY_INDEX, 207).

-define(GTP_V2_IE_V2_PRIVATE_EXTENSION, 255).

-define(GTP_V2_RECORDS,
        [v2_international_mobile_subscriber_identity,
         v2_cause,
         v2_recovery,
         v2_stn_sr,
         v2_access_point_name,
         v2_aggregate_maximum_bit_rate,
         v2_eps_bearer_id,
         v2_ip_address,
         v2_mobile_equipment_identity,
         v2_msisdn,
         v2_indication,
         v2_protocol_configuration_options,
         v2_pdn_address_allocation,
         v2_bearer_level_quality_of_service,
         v2_flow_quality_of_service,
         v2_rat_type,
         v2_serving_network,
         v2_eps_bearer_level_traffic_flow_template,
         v2_traffic_aggregation_description,
         v2_user_location_information,
         v2_fully_qualified_tunnel_endpoint_identifier,
         v2_tmsi,
         v2_global_cn_id,
         v2_s103_pdn_data_forwarding_info,
         v2_s1_u_data_forwarding_info,
         v2_delay_value,
         v2_bearer_context,
         v2_charging_id,
         v2_charging_characteristics,
         v2_trace_information,
         v2_bearer_flags,
         v2_pdn_type,
         v2_procedure_transaction_id,
         v2_mm_context_1,
         v2_mm_context_2,
         v2_mm_context_3,
         v2_mm_context_4,
         v2_mm_context_5,
         v2_mm_context_6,
         v2_pdn_connection,
         v2_pdu_numbers,
         v2_p_tmsi,
         v2_p_tmsi_signature,
         v2_hop_counter,
         v2_ue_time_zone,
         v2_trace_reference,
         v2_complete_request_message,
         v2_guti,
         v2_f_container,
         v2_f_cause,
         v2_plmn_id,
         v2_target_identification,
         v2_packet_flow_id,
         v2_rab_context,
         v2_source_rnc_pdcp_context_info,
         v2_udp_source_port_number,
         v2_apn_restriction,
         v2_selection_mode,
         v2_source_identification,
         v2_change_reporting_action,
         v2_fully_qualified_pdn_connection_set_identifier,
         v2_channel_needed,
         v2_emlpp_priority,
         v2_node_type,
         v2_fully_qualified_domain_name,
         v2_transaction_identifier,
         v2_mbms_session_duration,
         v2_mbms_service_area,
         v2_mbms_session_identifier,
         v2_mbms_flow_identifier,
         v2_mbms_ip_multicast_distribution,
         v2_mbms_distribution_acknowledge,
         v2_rfsp_index,
         v2_user_csg_information,
         v2_csg_information_reporting_action,
         v2_csg_id,
         v2_csg_membership_indication,
         v2_service_indicator,
         v2_detach_type,
         v2_local_distiguished_name,
         v2_node_features,
         v2_mbms_time_to_data_transfer,
         v2_throttling,
         v2_allocation_retention_priority,
         v2_epc_timer,
         v2_signalling_priority_indication,
         v2_temporary_mobile_group_identity,
         v2_additional_mm_context_for_srvcc,
         v2_additional_flags_for_srvcc,
         v2_mdt_configuration,
         v2_additional_protocol_configuration_options,
         v2_absolute_time_of_mbms_data_transfer,
         v2_henb_information_reporting_,
         v2_ipv4_configuration_parameters,
         v2_change_to_report_flags_,
         v2_action_indication,
         v2_twan_identifier,
         v2_uli_timestamp,
         v2_mbms_flags,
         v2_ran_nas_cause,
         v2_cn_operator_selection_entity,
         v2_trusted_wlan_mode_indication,
         v2_node_number,
         v2_node_identifier,
         v2_presence_reporting_area_action,
         v2_presence_reporting_area_information,
         v2_twan_identifier_timestamp,
         v2_overload_control_information,
         v2_load_control_information,
         v2_metric,
         v2_sequence_number,
         v2_apn_and_relative_capacity,
         v2_wlan_offloadability_indication,
         v2_paging_and_service_information,
         v2_integer_number,
         v2_millisecond_time_stamp,
         v2_monitoring_event_information,
         v2_ecgi_list,
         v2_remote_ue_context,
         v2_remote_user_id,
         v2_remote_ue_ip_information,
         v2_ciot_optimizations_support_indication,
         v2_scef_pdn_connection,
         v2_header_compression_configuration,
         v2_extended_protocol_configuration_options,
         v2_serving_plmn_rate_control,
         v2_counter,
         v2_mapped_ue_usage_type,
         v2_secondary_rat_usage_data_report,
         v2_up_function_selection_indication_flags,
         v2_maximum_packet_loss_rate,
         v2_apn_rate_control_status,
         v2_extended_trace_information,
         v2_monitoring_event_extension_information,
         v2_additional_rrm_policy_index,
         v2_private_extension]).

-record(v2_international_mobile_subscriber_identity,
        {instance = 0 :: non_neg_integer(), imsi}).

-record(v2_cause,
        {instance = 0 :: non_neg_integer(),
         v2_cause = reserved,
         pce = 0 :: non_neg_integer(),
         bce = 0 :: non_neg_integer(),
         cs = 0 :: non_neg_integer(),
         offending_ie}).

-record(v2_recovery,
        {instance = 0 :: non_neg_integer(),
         restart_counter = 0 :: non_neg_integer()}).

-record(v2_stn_sr, {instance = 0 :: non_neg_integer()}).

-record(v2_access_point_name,
        {instance = 0 :: non_neg_integer(), apn}).

-record(v2_aggregate_maximum_bit_rate,
        {instance = 0 :: non_neg_integer(),
         uplink = 0 :: non_neg_integer(),
         downlink = 0 :: non_neg_integer()}).

-record(v2_eps_bearer_id,
        {instance = 0 :: non_neg_integer(),
         eps_bearer_id = 0 :: non_neg_integer()}).

-record(v2_ip_address,
        {instance = 0 :: non_neg_integer(), ip = <<>>}).

-record(v2_mobile_equipment_identity,
        {instance = 0 :: non_neg_integer(), mei}).

-record(v2_msisdn,
        {instance = 0 :: non_neg_integer(), msisdn}).

-record(v2_indication,
        {instance = 0 :: non_neg_integer(), flags = #{}}).

-record(v2_protocol_configuration_options,
        {instance = 0 :: non_neg_integer(), config}).

-record(v2_pdn_address_allocation,
        {instance = 0 :: non_neg_integer(),
         type = ipv4,
         address = <<>>}).

-record(v2_bearer_level_quality_of_service,
        {instance = 0 :: non_neg_integer(),
         pci = 0 :: non_neg_integer(),
         pl = 0 :: non_neg_integer(),
         pvi = 0 :: non_neg_integer(),
         label = 0 :: non_neg_integer(),
         maximum_bit_rate_for_uplink = 0 :: non_neg_integer(),
         maximum_bit_rate_for_downlink = 0 :: non_neg_integer(),
         guaranteed_bit_rate_for_uplink = 0 :: non_neg_integer(),
         guaranteed_bit_rate_for_downlink = 0 ::
           non_neg_integer()}).

-record(v2_flow_quality_of_service,
        {instance = 0 :: non_neg_integer(),
         label = 0 :: non_neg_integer(),
         maximum_bit_rate_for_uplink = 0 :: non_neg_integer(),
         maximum_bit_rate_for_downlink = 0 :: non_neg_integer(),
         guaranteed_bit_rate_for_uplink = 0 :: non_neg_integer(),
         guaranteed_bit_rate_for_downlink = 0 ::
           non_neg_integer()}).

-record(v2_rat_type,
        {instance = 0 :: non_neg_integer(),
         rat_type = 0 :: non_neg_integer()}).

-record(v2_serving_network,
        {instance = 0 :: non_neg_integer(),
         plmn_id = {<<"001">>, <<"001">>}}).

-record(v2_eps_bearer_level_traffic_flow_template,
        {instance = 0 :: non_neg_integer(), value = <<>>}).

-record(v2_traffic_aggregation_description,
        {instance = 0 :: non_neg_integer(), value = <<>>}).

-record(v2_tmsi,
        {instance = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(v2_global_cn_id,
        {instance = 0 :: non_neg_integer(),
         plmn_id = {<<"001">>, <<"001">>},
         value = <<>>}).

-record(v2_s103_pdn_data_forwarding_info,
        {instance = 0 :: non_neg_integer(),
         hsgw_address = <<>>,
         gre_key = 0 :: non_neg_integer(),
         eps_bearer_id = []}).

-record(v2_s1_u_data_forwarding_info,
        {instance = 0 :: non_neg_integer(),
         service_gw_address = <<>>,
         teid = 0 :: non_neg_integer()}).

-record(v2_delay_value,
        {instance = 0 :: non_neg_integer(),
         delay = 0 :: non_neg_integer()}).

-record(v2_bearer_context,
        {instance = 0 :: non_neg_integer(), group}).

-record(v2_charging_id,
        {instance = 0 :: non_neg_integer(),
         id = <<0:4/unit:8>>}).

-record(v2_charging_characteristics,
        {instance = 0 :: non_neg_integer(),
         value = <<0:2/unit:8>>}).

-record(v2_trace_information,
        {instance = 0 :: non_neg_integer(),
         plmn_id = {<<"001">>, <<"001">>},
         trace_id = 0 :: non_neg_integer(),
         triggering_events = <<0:9/unit:8>>,
         list_of_ne_types = 0 :: non_neg_integer(),
         session_trace_depth = 0 :: non_neg_integer(),
         list_of_interfaces = <<0:12/unit:8>>,
         ip_address_of_trace_collection_entity = <<>>}).

-record(v2_bearer_flags,
        {instance = 0 :: non_neg_integer(), flags = #{}}).

-record(v2_pdn_type,
        {instance = 0 :: non_neg_integer(), pdn_type = ipv4}).

-record(v2_procedure_transaction_id,
        {instance = 0 :: non_neg_integer(),
         pti = 0 :: non_neg_integer()}).

-record(v2_mm_context_1,
        {instance = 0 :: non_neg_integer()}).

-record(v2_mm_context_2,
        {instance = 0 :: non_neg_integer()}).

-record(v2_mm_context_3,
        {instance = 0 :: non_neg_integer()}).

-record(v2_mm_context_4,
        {instance = 0 :: non_neg_integer()}).

-record(v2_mm_context_5,
        {instance = 0 :: non_neg_integer()}).

-record(v2_mm_context_6,
        {instance = 0 :: non_neg_integer()}).

-record(v2_pdn_connection,
        {instance = 0 :: non_neg_integer(), group}).

-record(v2_pdu_numbers,
        {instance = 0 :: non_neg_integer(),
         nsapi = 0 :: non_neg_integer(),
         dl_gtp_u_sequence_number = 0 :: non_neg_integer(),
         ul_gtp_u_sequence_number = 0 :: non_neg_integer(),
         send_n_pdu_number = 0 :: non_neg_integer(),
         receive_n_pdu_number = 0 :: non_neg_integer()}).

-record(v2_p_tmsi,
        {instance = 0 :: non_neg_integer(), value = <<>>}).

-record(v2_p_tmsi_signature,
        {instance = 0 :: non_neg_integer(), value = <<>>}).

-record(v2_hop_counter,
        {instance = 0 :: non_neg_integer(),
         hop_counter = 0 :: non_neg_integer()}).

-record(v2_ue_time_zone,
        {instance = 0 :: non_neg_integer(),
         timezone = 0 :: non_neg_integer(),
         dst = 0 :: non_neg_integer()}).

-record(v2_trace_reference,
        {instance = 0 :: non_neg_integer(),
         plmn_id = {<<"001">>, <<"001">>},
         id = 0 :: non_neg_integer()}).

-record(v2_complete_request_message,
        {instance = 0 :: non_neg_integer(),
         type = 0 :: non_neg_integer(),
         message = <<>>}).

-record(v2_guti,
        {instance = 0 :: non_neg_integer(),
         plmn_id = {<<"001">>, <<"001">>},
         group_id = 0 :: non_neg_integer(),
         code = 0 :: non_neg_integer(),
         m_tmsi = <<>>}).

-record(v2_f_container,
        {instance = 0 :: non_neg_integer(),
         type = 0 :: non_neg_integer(),
         data = <<>>}).

-record(v2_f_cause,
        {instance = 0 :: non_neg_integer(),
         type = 0 :: non_neg_integer(),
         data = <<>>}).

-record(v2_plmn_id,
        {instance = 0 :: non_neg_integer(),
         id = <<0:3/unit:8>>}).

-record(v2_target_identification,
        {instance = 0 :: non_neg_integer(),
         type = 0 :: non_neg_integer(),
         data = <<>>}).

-record(v2_packet_flow_id,
        {instance = 0 :: non_neg_integer(),
         ebi = 0 :: non_neg_integer(),
         flow_id = <<>>}).

-record(v2_rab_context,
        {instance = 0 :: non_neg_integer(),
         ulpsi = 0 :: non_neg_integer(),
         dlpsi = 0 :: non_neg_integer(),
         ulgsi = 0 :: non_neg_integer(),
         dlgsi = 0 :: non_neg_integer(),
         nsapi = 0 :: non_neg_integer(),
         dl_gtp_u_sequence_number = 0 :: non_neg_integer(),
         ul_gtp_u_sequence_number = 0 :: non_neg_integer(),
         dl_pdcp_number = 0 :: non_neg_integer(),
         ul_pdcp_number = 0 :: non_neg_integer()}).

-record(v2_source_rnc_pdcp_context_info,
        {instance = 0 :: non_neg_integer(),
         rrc_container = <<>>}).

-record(v2_udp_source_port_number,
        {instance = 0 :: non_neg_integer(),
         port = 0 :: non_neg_integer()}).

-record(v2_apn_restriction,
        {instance = 0 :: non_neg_integer(),
         restriction_type_value = 0 :: non_neg_integer()}).

-record(v2_selection_mode,
        {instance = 0 :: non_neg_integer(),
         mode = 0 :: non_neg_integer()}).

-record(v2_source_identification,
        {instance = 0 :: non_neg_integer(),
         target_cell_id = <<>>,
         source_type = 0 :: non_neg_integer(),
         source_id = <<>>}).

-record(v2_change_reporting_action,
        {instance = 0 :: non_neg_integer(),
         action = stop_reporting}).

-record(v2_channel_needed,
        {instance = 0 :: non_neg_integer(), value = <<>>}).

-record(v2_emlpp_priority,
        {instance = 0 :: non_neg_integer(), value = <<>>}).

-record(v2_node_type,
        {instance = 0 :: non_neg_integer(),
         node_type = 0 :: non_neg_integer()}).

-record(v2_fully_qualified_domain_name,
        {instance = 0 :: non_neg_integer(), fqdn}).

-record(v2_transaction_identifier,
        {instance = 0 :: non_neg_integer(), value = <<>>}).

-record(v2_mbms_session_duration,
        {instance = 0 :: non_neg_integer()}).

-record(v2_mbms_service_area,
        {instance = 0 :: non_neg_integer()}).

-record(v2_mbms_session_identifier,
        {instance = 0 :: non_neg_integer()}).

-record(v2_mbms_flow_identifier,
        {instance = 0 :: non_neg_integer()}).

-record(v2_mbms_ip_multicast_distribution,
        {instance = 0 :: non_neg_integer()}).

-record(v2_mbms_distribution_acknowledge,
        {instance = 0 :: non_neg_integer()}).

-record(v2_rfsp_index,
        {instance = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(v2_user_csg_information,
        {instance = 0 :: non_neg_integer(),
         plmn_id = {<<"001">>, <<"001">>},
         csg_id = <<0:27>>,
         access_mode = 0 :: non_neg_integer(),
         lcsg = false :: boolean(),
         cmi = 0 :: non_neg_integer()}).

-record(v2_csg_information_reporting_action,
        {instance = 0 :: non_neg_integer(), actions = #{}}).

-record(v2_csg_id,
        {instance = 0 :: non_neg_integer(), id = <<0:27>>}).

-record(v2_csg_membership_indication,
        {instance = 0 :: non_neg_integer(),
         cmi = 0 :: non_neg_integer()}).

-record(v2_service_indicator,
        {instance = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(v2_detach_type,
        {instance = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(v2_local_distiguished_name,
        {instance = 0 :: non_neg_integer(), value = <<>>}).

-record(v2_node_features,
        {instance = 0 :: non_neg_integer(), features = #{}}).

-record(v2_mbms_time_to_data_transfer,
        {instance = 0 :: non_neg_integer()}).

-record(v2_throttling,
        {instance = 0 :: non_neg_integer(),
         unit = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer(),
         factor = 0 :: non_neg_integer()}).

-record(v2_allocation_retention_priority,
        {instance = 0 :: non_neg_integer(),
         pci = false :: boolean(),
         pl = 0 :: non_neg_integer(),
         pvi = false :: boolean()}).

-record(v2_epc_timer,
        {instance = 0 :: non_neg_integer(),
         unit = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(v2_signalling_priority_indication,
        {instance = 0 :: non_neg_integer(), indication = #{}}).

-record(v2_temporary_mobile_group_identity,
        {instance = 0 :: non_neg_integer()}).

-record(v2_additional_mm_context_for_srvcc,
        {instance = 0 :: non_neg_integer(),
         classmark_2 = <<>>,
         classmark_3 = <<>>,
         codec_list = <<>>}).

-record(v2_additional_flags_for_srvcc,
        {instance = 0 :: non_neg_integer(), flags = #{}}).

-record(v2_mdt_configuration,
        {instance = 0 :: non_neg_integer()}).

-record(v2_additional_protocol_configuration_options,
        {instance = 0 :: non_neg_integer(), config}).

-record(v2_absolute_time_of_mbms_data_transfer,
        {instance = 0 :: non_neg_integer()}).

-record(v2_henb_information_reporting_,
        {instance = 0 :: non_neg_integer(), flags = #{}}).

-record(v2_ipv4_configuration_parameters,
        {instance = 0 :: non_neg_integer(),
         prefix_length = 0 :: non_neg_integer(),
         default_route = <<0:4/unit:8>>}).

-record(v2_change_to_report_flags_,
        {instance = 0 :: non_neg_integer(), flags = #{}}).

-record(v2_action_indication,
        {instance = 0 :: non_neg_integer(),
         indication = 0 :: non_neg_integer()}).

-record(v2_uli_timestamp,
        {instance = 0 :: non_neg_integer(),
         timestamp = 0 :: non_neg_integer()}).

-record(v2_mbms_flags,
        {instance = 0 :: non_neg_integer()}).

-record(v2_ran_nas_cause,
        {instance = 0 :: non_neg_integer(),
         protocol = 0 :: non_neg_integer(),
         type = 0 :: non_neg_integer(),
         cause = <<>>}).

-record(v2_cn_operator_selection_entity,
        {instance = 0 :: non_neg_integer(),
         entity = 0 :: non_neg_integer()}).

-record(v2_trusted_wlan_mode_indication,
        {instance = 0 :: non_neg_integer(), indication = #{}}).

-record(v2_node_number,
        {instance = 0 :: non_neg_integer(), number = <<>>}).

-record(v2_node_identifier,
        {instance = 0 :: non_neg_integer(),
         name = <<>>,
         realm = <<>>}).

-record(v2_presence_reporting_area_action,
        {instance = 0 :: non_neg_integer()}).

-record(v2_presence_reporting_area_information,
        {instance = 0 :: non_neg_integer()}).

-record(v2_twan_identifier_timestamp,
        {instance = 0 :: non_neg_integer(),
         timestamp = 0 :: non_neg_integer()}).

-record(v2_overload_control_information,
        {instance = 0 :: non_neg_integer(), group}).

-record(v2_load_control_information,
        {instance = 0 :: non_neg_integer(), group}).

-record(v2_metric,
        {instance = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(v2_sequence_number,
        {instance = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

-record(v2_apn_and_relative_capacity,
        {instance = 0 :: non_neg_integer(),
         capacity = 0 :: non_neg_integer(),
         apn = <<>>}).

-record(v2_wlan_offloadability_indication,
        {instance = 0 :: non_neg_integer(), indication = #{}}).

-record(v2_millisecond_time_stamp,
        {instance = 0 :: non_neg_integer(),
         timestamp = 0 :: non_neg_integer()}).

-record(v2_monitoring_event_information,
        {instance = 0 :: non_neg_integer()}).

-record(v2_ecgi_list,
        {instance = 0 :: non_neg_integer(), ecgis = []}).

-record(v2_remote_ue_context,
        {instance = 0 :: non_neg_integer(), group}).

-record(v2_remote_ue_ip_information,
        {instance = 0 :: non_neg_integer(), ip = <<>>}).

-record(v2_ciot_optimizations_support_indication,
        {instance = 0 :: non_neg_integer(), indication = #{}}).

-record(v2_scef_pdn_connection,
        {instance = 0 :: non_neg_integer(), group}).

-record(v2_header_compression_configuration,
        {instance = 0 :: non_neg_integer(),
         rohc_profiles = 0 :: non_neg_integer(),
         max_cid = 0 :: non_neg_integer()}).

-record(v2_extended_protocol_configuration_options,
        {instance = 0 :: non_neg_integer(), config}).

-record(v2_serving_plmn_rate_control,
        {instance = 0 :: non_neg_integer(),
         uplink = 0 :: non_neg_integer(),
         downlink = 0 :: non_neg_integer()}).

-record(v2_counter,
        {instance = 0 :: non_neg_integer(),
         timestamp = 0 :: non_neg_integer(),
         counter = 0 :: non_neg_integer()}).

-record(v2_mapped_ue_usage_type,
        {instance = 0 :: non_neg_integer(),
         usage_type = 0 :: non_neg_integer()}).

-record(v2_secondary_rat_usage_data_report,
        {instance = 0 :: non_neg_integer(),
         irsgw = false :: boolean(),
         irpgw = false :: boolean(),
         rat_type = 0 :: non_neg_integer(),
         ebi = 0 :: non_neg_integer(),
         start_time = 0 :: non_neg_integer(),
         end_time = 0 :: non_neg_integer(),
         dl = 0 :: non_neg_integer(),
         ul = 0 :: non_neg_integer()}).

-record(v2_up_function_selection_indication_flags,
        {instance = 0 :: non_neg_integer(), indication = #{}}).

-record(v2_apn_rate_control_status,
        {instance = 0 :: non_neg_integer(),
         number_of_uplink_packets_allowed = 0 ::
           non_neg_integer(),
         number_of_additional_exception_reports = 0 ::
           non_neg_integer(),
         number_of_downlink_packets_allowed = 0 ::
           non_neg_integer(),
         apn_rate_control_status_validity_time = 0 ::
           non_neg_integer()}).

-record(v2_extended_trace_information,
        {instance = 0 :: non_neg_integer(),
         plmn_id = {<<"001">>, <<"001">>},
         trace_id = 0 :: non_neg_integer(),
         triggering_events = <<>>,
         list_of_ne_types = <<>>,
         session_trace_depth = 0 :: non_neg_integer(),
         list_of_interfaces = <<>>,
         ip_address_of_trace_collection_entity = <<>>}).

-record(v2_additional_rrm_policy_index,
        {instance = 0 :: non_neg_integer(),
         value = 0 :: non_neg_integer()}).

