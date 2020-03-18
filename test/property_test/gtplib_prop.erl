%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

%% Copyright 2018, Travelping GmbH <info@travelping.com>

-module(gtplib_prop).

-compile([export_all, nowarn_export_all]).

-include_lib("gtplib/include/gtp_packet.hrl").

-proptest(proper).
-proptest([triq,eqc]).

-ifndef(EQC).
-ifndef(PROPER).
-ifndef(TRIQ).
-define(PROPER,true).
%%-define(EQC,true).
%%-define(TRIQ,true).
-endif.
-endif.
-endif.

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-define(MOD_eqc,eqc).

-else.
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).

-else.
-ifdef(TRIQ).
-define(MOD_eqc,triq).
-include_lib("triq/include/triq.hrl").

-endif.
-endif.
-endif.

-define(equal(Expected, Actual),
	(fun (Expected@@@, Expected@@@) -> true;
	     (Expected@@@, Actual@@@) ->
		 ct:pal("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
			[?FILE, ?LINE, ??Actual, Expected@@@, Actual@@@]),
		 false
	 end)(Expected, Actual) orelse error(badmatch)).

%%%===================================================================
%%% Tests
%%%===================================================================

%%--------------------------------------------------------------------
enc_dec_prop(_Config) ->
    numtests(1000,
	     ?FORALL(Msg, msg_gen(),
		     begin
			 %% ct:pal("Msg: ~p", [Msg]),
			 Enc = gtp_packet:encode(Msg),
			 ?equal(Enc, gtp_packet:encode(gtp_packet:decode(Enc)))
		     end)).

%%%===================================================================
%%% Generate PCAP with random (but valid GTP packets)
%%%===================================================================

-define(PCAPNG_VERSION_MAJOR, 1).
-define(PCAPNG_VERSION_MINOR, 0).
-define(LINKTYPE_ETHERNET, 1).
-define(LINKTYPE_RAW, 101).

make_udp(NwSrc, NwDst, TpSrc, TpDst, PayLoad) ->
    Id = 0,
    Proto = gen_socket:protocol(udp),

    UDPLength = 8 + size(PayLoad),
    UDPCSum = flower_tools:ip_csum(<<NwSrc:4/bytes-unit:8, NwDst:4/bytes-unit:8,
				     0:8, Proto:8, UDPLength:16,
				     TpSrc:16, TpDst:16, UDPLength:16, 0:16,
				     PayLoad/binary>>),
    UDP = <<TpSrc:16, TpDst:16, UDPLength:16, UDPCSum:16, PayLoad/binary>>,

    TotLen = 20 + size(UDP),
    HdrCSum = flower_tools:ip_csum(<<4:4, 5:4, 0:8, TotLen:16,
				     Id:16, 0:16, 64:8, Proto:8,
				     0:16/integer, NwSrc:4/bytes-unit:8, NwDst:4/bytes-unit:8>>),
    IP = <<4:4, 5:4, 0:8, TotLen:16,
	   Id:16, 0:16, 64:8, Proto:8,
	   HdrCSum:16/integer, NwSrc:4/bytes-unit:8, NwDst:4/bytes-unit:8>>,
    list_to_binary([IP, UDP]).

format_pcapng(Data) ->
    TStamp = os:system_time(micro_seconds),
    Len = size(Data),
    pcapng:encode({epb, 0, TStamp, Len, [], Data}).

pcapng_shb() ->
    pcapng:encode({shb, {?PCAPNG_VERSION_MAJOR, ?PCAPNG_VERSION_MINOR},
		   [{os, <<"CAROS">>}, {userappl, <<"CAPWAP">>}]}).

pcapng_ifd(Name) ->
    pcapng:encode({ifd, ?LINKTYPE_RAW, 65535,
		   [{name,    Name},
		    {tsresol, <<6>>},
		    {os,      <<"CAROS">>}]}).

pcap_msg(Msg, Io) ->
    Data = gtp_packet:encode(Msg),
    Packet = make_udp(<<127,0,0,1>>, <<127,0,0,2>>, 2152, 2152, Data),
    Dump = format_pcapng(Packet),
    ok = file:write(Io, Dump).

gen_pcap(0, _Io) ->
    ok;
gen_pcap(Cnt, Io) ->
    {ok, Msg} = proper_gen:pick(msg_gen()),
    pcap_msg(Msg, Io),
    gen_pcap(Cnt - 1, Io).

gen_pcap(Cnt) ->
    {ok, Io} = file:open("gtp.pcap", [write, raw]),
    Header = << (pcapng_shb())/binary, (pcapng_ifd(<<"GTP">>))/binary >>,
    file:write(Io, Header),
    gen_pcap(Cnt, Io),
    file:close(Io).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% proper generates a random value for integers. That does not
%% guarantee that the full range of the integer value is tested.
%% Include Min and Max explicitly to ensure the full range is covered.
int_range(Min, Max) ->
    oneof([integer(Min, Max), Min, Max]).

%% from the proper manual
list_no_dupls(T) ->
    ?LET(L, list(T), remove_duplicates(L)).

%% better versions of remove_duplicates/1 exist ...
remove_duplicates([]) -> [];
remove_duplicates([A|T]) ->
    case lists:member(A, T) of
	true -> remove_duplicates(T);
	false -> [A|remove_duplicates(T)]
    end.

flags(Flags) ->
    list_no_dupls(oneof(Flags)).

flag() ->
    oneof([0,1]).

dns_label() ->
    ?LET(I, int_range(1,64),
	 vector(I,
		oneof(
		  lists:seq($A, $Z) ++ lists:seq($a, $z) ++ lists:seq($0, $9) ++ [$-]))).

dns_name_list() ->
    ?SUCHTHAT(N,
	      ?LET(I, int_range(1,7), vector(I, dns_label())),
	      length(lists:flatten(N)) < 100).

dns_name() ->
    ?LET(L, dns_name_list(),
	 [list_to_binary(X) || X <- L]).

mcc() ->
    ?LET(I, int_range(1,999), integer_to_binary(I)).

mcc_label() ->
    ?LET(M, mcc(), list_to_binary(io_lib:format("mcc~3..0s", [M]))).

mnc() ->
    ?LET(M, int_range(1,999), integer_to_binary(M)).

mnc_label() ->
    ?LET(M, mnc(), list_to_binary(io_lib:format("mnc~3..0s", [M]))).

apn() ->
    ?LET(L, [dns_name(), mnc_label(), mcc_label(), <<"gprs">>], lists:flatten(L)).

uint4() ->
    int_range(0,16#0f).

uint8() ->
    int_range(0,16#ff).

uint16() ->
    int_range(0,16#ffff).

uint24() ->
    int_range(0,16#ffffff).

uint32() ->
    int_range(0,16#ffffffff).

uint40() ->
    int_range(0,16#ffffffffff).

uint64() ->
    int_range(0,16#ffffffffffffffff).

ip4_address() ->
    binary(4).

ip6_address() ->
    binary(16).

ip46_address() ->
    binary(20).

instance() ->
    uint4().

tei() ->
    uint32().

uint16_array() ->
    ?LET(I, int_range(1,10), vector(I, uint16())).

binstr_number(Min, Max) ->
    ?LET(X,
	 ?LET(I, int_range(Min,Max), vector(I, integer($0, $9))), list_to_binary(X)).

binary(Min, Max) ->
    ?LET(I, int_range(Min,Max), binary(I)).

imsi() ->
    binstr_number(7,15).

msisdn() ->
    {isdn_address, 1, 1, 1, binstr_number(7,20)}.

imei() ->
    binstr_number(15, 15).

imeisv() ->
    binstr_number(16, 16).

msg_gen() ->
    oneof(
      [#gtp{
	  version = v1,
	  type = v1_msg_type(),
	  tei = uint32(),
	  seq_no = uint16(),
	  ie = v1_ie()
	 },
       #gtp{
	  version = v2,
	  type = v2_msg_type(),
	  tei = uint32(),
	  seq_no = uint16(),
	  ie = v2_ie()
	 },
       #gtp{
	  version = oneof([prime_v0, prime_v0s, prime_v1, prime_v2]),
	  type = prime_msg_type(),
	  seq_no = uint16(),
	  ie = prime_ie()
	 }
      ]).

v1_msg_type() ->
    oneof([
	   echo_request,
	   echo_response,
	   version_not_supported,
	   create_pdp_context_request,
	   create_pdp_context_response,
	   update_pdp_context_request,
	   update_pdp_context_response,
	   delete_pdp_context_request,
	   delete_pdp_context_response,
	   initiate_pdp_context_activation_request,
	   initiate_pdp_context_activation_response,
	   error_indication,
	   pdu_notification_request,
	   pdu_notification_response,
	   pdu_notification_reject_request,
	   pdu_notification_reject_response,
	   supported_extension_headers_notification,
	   send_routeing_information_for_gprs_request,
	   send_routeing_information_for_gprs_response,
	   failure_report_request,
	   failure_report_response,
	   note_ms_gprs_present_request,
	   note_ms_gprs_present_response,
	   identification_request,
	   identification_response,
	   sgsn_context_request,
	   sgsn_context_response,
	   sgsn_context_acknowledge,
	   forward_relocation_request,
	   forward_relocation_response,
	   forward_relocation_complete,
	   relocation_cancel_request,
	   relocation_cancel_response,
	   forward_srns_context,
	   forward_relocation_complete_acknowledge,
	   forward_srns_context_acknowledge,
	   ran_information_relay,
	   mbms_notification_request,
	   mbms_notification_response,
	   mbms_notification_reject_request,
	   mbms_notification_reject_response,
	   create_mbms_context_request,
	   create_mbms_context_response,
	   update_mbms_context_request,
	   update_mbms_context_response,
	   delete_mbms_context_request,
	   delete_mbms_context_response,
	   mbms_registration_request,
	   mbms_registration_response,
	   mbms_de_registration_request,
	   mbms_de_registration_response,
	   mbms_session_start_request,
	   mbms_session_start_response,
	   mbms_session_stop_request,
	   mbms_session_stop_response,
	   mbms_session_update_request,
	   mbms_session_update_response,
	   ms_info_change_notification_request,
	   ms_info_change_notification_response
	  ]).

prime_msg_type() ->
    oneof([
	   version_not_supported,
	   node_alive_request,
	   node_alive_response,
	   redirection_request,
	   redirection_response,
	   error_indication,
	   data_record_transfer_request,
	   data_record_transfer_response
	  ]).

v2_msg_type() ->
    oneof([
	   echo_request,
	   echo_response,
	   version_not_supported,
	   create_session_request,
	   create_session_response,
	   delete_session_request,
	   delete_session_response,
	   modify_bearer_request,
	   modify_bearer_response,
	   change_notification_request,
	   change_notification_response,
	   modify_bearer_command,
	   modify_bearer_failure_indication,
	   delete_bearer_command,
	   delete_bearer_failure_indication,
	   bearer_resource_command,
	   bearer_resource_failure_indication,
	   downlink_data_notification_failure_indication,
	   trace_session_activation,
	   trace_session_deactivation,
	   stop_paging_indication,
	   create_bearer_request,
	   create_bearer_response,
	   update_bearer_request,
	   update_bearer_response,
	   delete_bearer_request,
	   delete_bearer_response,
	   delete_pdn_connection_set_request,
	   delete_pdn_connection_set_response,
	   pgw_downlink_triggering_notification,
	   pgw_downlink_triggering_acknowledge,
	   identification_request,
	   identification_response,
	   context_request,
	   context_response,
	   context_acknowledge,
	   forward_relocation_request,
	   forward_relocation_response,
	   forward_relocation_complete_notification,
	   forward_relocation_complete_acknowledge,
	   forward_access_context_notification,
	   forward_access_context_acknowledge,
	   relocation_cancel_request,
	   relocation_cancel_response,
	   configuration_transfer_tunnel,
	   detach_notification,
	   detach_acknowledge,
	   cs_paging_indication,
	   ran_information_relay,
	   alert_mme_notification,
	   alert_mme_acknowledge,
	   ue_activity_notification,
	   ue_activity_acknowledge,
	   isr_status_indication,
	   create_forwarding_tunnel_request,
	   create_forwarding_tunnel_response,
	   suspend_notification,
	   suspend_acknowledge,
	   resume_notification,
	   resume_acknowledge,
	   create_indirect_data_forwarding_tunnel_request,
	   create_indirect_data_forwarding_tunnel_response,
	   delete_indirect_data_forwarding_tunnel_request,
	   delete_indirect_data_forwarding_tunnel_response,
	   release_access_bearers_request,
	   release_access_bearers_response,
	   downlink_data_notification,
	   downlink_data_notification_acknowledge,
	   pgw_restart_notification,
	   pgw_restart_notification_acknowledge,
	   update_pdn_connection_set_request,
	   update_pdn_connection_set_response,
	   mbms_session_start_response,
	   mbms_session_update_request,
	   mbms_session_update_response,
	   mbms_session_stop_request,
	   mbms_session_stop_response
	  ]).

v1_simple_ie() ->
    oneof(
      [gen_routeing_area_identity(),
       gen_user_location_information(),
       gen_cause(),
       gen_international_mobile_subscriber_identity(),
       gen_temporary_logical_link_identity(),
       gen_packet_tmsi(),
       gen_reordering_required(),
       gen_authentication_triplet(),
       gen_map_cause(),
       gen_p_tmsi_signature(),
       gen_ms_validated(),
       gen_recovery(),
       gen_selection_mode(),
       gen_tunnel_endpoint_identifier_data_i(),
       gen_tunnel_endpoint_identifier_control_plane(),
       gen_tunnel_endpoint_identifier_data_ii(),
       gen_teardown_ind(),
       gen_nsapi(),
       gen_ranap_cause(),
       gen_rab_context(),
       gen_radio_priority_sms(),
       gen_radio_priority(),
       gen_packet_flow_id(),
       gen_charging_characteristics(),
       gen_trace_reference(),
       gen_trace_type(),
       gen_ms_not_reachable_reason(),
       gen_charging_id(),
       gen_end_user_address(),
       %% gen_mm_context_gsm(),
       %% gen_mm_context_umts(),
       %% gen_mm_context_gsm_and_umts(),
       %% gen_mm_context_umts_and_used_cipher(),
       gen_pdp_context(),
       gen_access_point_name(),
       gen_protocol_configuration_options(),
       gen_gsn_address(),
       gen_ms_international_pstn_isdn_number(),
       gen_quality_of_service_profile(),
       gen_authentication_quintuplet(),
       gen_traffic_flow_template(),
       gen_target_identification(),
       gen_utran_transparent_container(),
       gen_rab_setup_information(),
       gen_extension_header_type_list(),
       gen_trigger_id(),
       gen_omc_identity(),
       gen_ran_transparent_container(),
       gen_pdp_context_prioritization(),
       gen_additional_rab_setup_information(),
       gen_sgsn_number(),
       gen_common_flags(),
       gen_apn_restriction(),
       gen_radio_priority_lcs(),
       gen_rat_type(),
       gen_ms_time_zone(),
       gen_imei(),
       gen_camel_charging_information_container(),
       gen_mbms_ue_context(),
       gen_temporary_mobile_group_identity(),
       gen_rim_routing_address(),
       gen_mbms_protocol_configuration_options(),
       gen_mbms_service_area(),
       gen_source_rnc_pdcp_context_info(),
       gen_additional_trace_info(),
       gen_hop_counter(),
       gen_selected_plmn_id(),
       gen_mbms_session_identifier(),
       gen_mbms_2g_3g_indicator(),
       gen_enhanced_nsapi(),
       gen_mbms_session_duration(),
       gen_additional_mbms_trace_info(),
       gen_mbms_session_repetition_number(),
       gen_mbms_time_to_data_transfer(),
       gen_bss_container(),
       gen_cell_identification(),
       gen_pdu_numbers(),
       gen_bssgp_cause(),
       gen_required_mbms_bearer_capabilities(),
       gen_rim_routing_address_discriminator(),
       gen_list_of_set_up_pfcs(),
       gen_ps_handover_xid_parameters(),
       gen_ms_info_change_reporting_action(),
       gen_direct_tunnel_flags(),
       gen_correlation_id(),
       gen_bearer_control_mode(),
       gen_mbms_flow_identifier(),
       gen_mbms_ip_multicast_distribution(),
       gen_mbms_distribution_acknowledgement(),
       gen_reliable_inter_rat_handover_info(),
       gen_rfsp_index(),
       gen_fully_qualified_domain_name(),
       gen_evolved_allocation_retention_priority_i(),
       gen_evolved_allocation_retention_priority_ii(),
       gen_extended_common_flags(),
       gen_user_csg_information(),
       gen_csg_information_reporting_action(),
       gen_csg_id(),
       gen_csg_membership_indication(),
       gen_aggregate_maximum_bit_rate(),
       gen_ue_network_capability(),
       gen_ue_ambr(),
       gen_apn_ambr_with_nsapi(),
       gen_ggsn_back_off_time(),
       gen_signalling_priority_indication(),
       gen_signalling_priority_indication_with_nsapi(),
       gen_higher_bitrates_than_16_mbps_flag(),
       gen_additional_mm_context_for_srvcc(),
       gen_additional_flags_for_srvcc(),
       gen_stn_sr(),
       gen_c_msisdn(),
       gen_extended_ranap_cause(),
       gen_enodeb_id(),
       gen_selection_mode_with_nsapi(),
       gen_uli_timestamp(),
       gen_local_home_network_id_with_nsapi(),
       gen_cn_operator_selection_entity(),
       gen_charging_gateway_address(),
       gen_private_extension()]).

v2_grouped_ie() ->
    [gen_v2_bearer_context(),
     gen_v2_pdn_connection(),
     gen_v2_overload_control_information(),
     gen_v2_load_control_information()].

v2_simple_ie() ->
    [gen_v2_international_mobile_subscriber_identity(),
     gen_v2_cause(),
     gen_v2_recovery(),
     gen_v2_stn_sr(),
     gen_v2_access_point_name(),
     gen_v2_aggregate_maximum_bit_rate(),
     gen_v2_eps_bearer_id(),
     gen_v2_ip_address(),
     gen_v2_mobile_equipment_identity(),
     gen_v2_msisdn(),
     gen_v2_indication(),
     gen_v2_protocol_configuration_options(),
     gen_v2_pdn_address_allocation(),
     gen_v2_bearer_level_quality_of_service(),
     gen_v2_flow_quality_of_service(),
     gen_v2_rat_type(),
     gen_v2_eps_bearer_level_traffic_flow_template(),
     gen_v2_traffic_aggregation_description(),
     gen_v2_user_location_information(),
     gen_v2_fully_qualified_tunnel_endpoint_identifier(),
     gen_v2_tmsi(),
     gen_v2_global_cn_id(),
     gen_v2_s103_pdn_data_forwarding_info(),
     gen_v2_s1_u_data_forwarding_info(),
     gen_v2_delay_value(),
     gen_v2_charging_id(),
     gen_v2_charging_characteristics(),
     gen_v2_trace_information(),
     gen_v2_bearer_flags(),
     gen_v2_pdn_type(),
     gen_v2_procedure_transaction_id(),
     gen_v2_mm_context_1(),
     gen_v2_mm_context_2(),
     gen_v2_mm_context_3(),
     gen_v2_mm_context_4(),
     gen_v2_mm_context_5(),
     gen_v2_mm_context_6(),
     gen_v2_pdu_numbers(),
     gen_v2_p_tmsi(),
     gen_v2_p_tmsi_signature(),
     gen_v2_hop_counter(),
     gen_v2_ue_time_zone(),
     gen_v2_trace_reference(),
     gen_v2_complete_request_message(),
     gen_v2_guti(),
     gen_v2_f_container(),
     gen_v2_f_cause(),
     gen_v2_plmn_id(),
     gen_v2_target_identification(),
     gen_v2_packet_flow_id(),
     gen_v2_rab_context(),
     gen_v2_source_rnc_pdcp_context_info(),
     gen_v2_udp_source_port_number(),
     gen_v2_apn_restriction(),
     gen_v2_selection_mode(),
     gen_v2_source_identification(),
     gen_v2_change_reporting_action(),
     gen_v2_fully_qualified_pdn_connection_set_identifier(),
     gen_v2_channel_needed(),
     gen_v2_emlpp_priority(),
     gen_v2_node_type(),
     gen_v2_fully_qualified_domain_name(),
     gen_v2_transaction_identifier(),
     gen_v2_mbms_session_duration(),
     gen_v2_mbms_service_area(),
     gen_v2_mbms_session_identifier(),
     gen_v2_mbms_flow_identifier(),
     gen_v2_mbms_ip_multicast_distribution(),
     gen_v2_mbms_distribution_acknowledge(),
     gen_v2_rfsp_index(),
     gen_v2_user_csg_information(),
     gen_v2_csg_information_reporting_action(),
     gen_v2_csg_id(),
     gen_v2_csg_membership_indication(),
     gen_v2_service_indicator(),
     gen_v2_detach_type(),
     gen_v2_local_distiguished_name(),
     gen_v2_node_features(),
     gen_v2_mbms_time_to_data_transfer(),
     gen_v2_throttling(),
     gen_v2_allocation_retention_priority(),
     gen_v2_epc_timer(),
     gen_v2_signalling_priority_indication(),
     gen_v2_temporary_mobile_group_identity(),
     gen_v2_additional_mm_context_for_srvcc(),
     gen_v2_additional_flags_for_srvcc(),
     gen_v2_mdt_configuration(),
     gen_v2_additional_protocol_configuration_options(),
     gen_v2_absolute_time_of_mbms_data_transfer(),
     gen_v2_henb_information_reporting_(),
     gen_v2_ipv4_configuration_parameters(),
     gen_v2_change_to_report_flags_(),
     gen_v2_action_indication(),
     gen_v2_twan_identifier(),
     gen_v2_uli_timestamp(),
     gen_v2_mbms_flags(),
     gen_v2_ran_nas_cause(),
     gen_v2_cn_operator_selection_entity(),
     gen_v2_trusted_wlan_mode_indication(),
     gen_v2_node_number(),
     gen_v2_node_identifier(),
     gen_v2_presence_reporting_area_action(),
     gen_v2_presence_reporting_area_information(),
     gen_v2_twan_identifier_timestamp(),
     gen_v2_metric(),
     gen_v2_sequence_number(),
     gen_v2_apn_and_relative_capacity(),
     gen_v2_wlan_offloadability_indication(),
     gen_v2_private_extension()].

prime_simple_ie() ->
    oneof(
      [
       gen_cause(),
       gen_packet_transfer_command(),
       gen_charging_id(),
       gen_sequence_numbers_of_released_packets(),
       gen_sequence_numbers_of_cancelled_packets(),
       gen_charging_gateway_address(),
       gen_data_record_packet(),
       gen_requests_responded(),
       gen_address_of_recommended_node(),
       gen_private_extension()]).

v1_ie() ->
    ie_map(
      ?LET(I, integer(1,10), vector(I, v1_simple_ie()))).

v2_ie() ->
    ie_map(
      ?LET(I, integer(1,10), vector(I, oneof(v2_simple_ie() ++ v2_grouped_ie())))).

prime_ie() ->
    ie_map(
      ?LET(I, integer(1,10), vector(I, prime_simple_ie()))).

put_ie(IE, IEs) ->
    %% ct:pal("IE: ~p", [IE]),
    Key = {element(1, IE), element(2, IE)},
    IEs#{Key => IE}.

list2map(List) ->
    lists:foldl(fun put_ie/2, #{}, List).

ie_map(IEs) ->
    ?LET(L, IEs, list2map(L)).

v2_ie_group() ->
    ie_map(
      ?LET(I, integer(1,10), vector(I, oneof(v2_simple_ie())))).

%% v1 generator ==========================================================================

gen_routeing_area_identity() ->
    #routeing_area_identity{
       instance = instance(),
       mcc = mcc(),
       mnc = mnc(),
       lac = uint16(),
       rac = 0
      }.

gen_user_location_information() ->
    oneof(
      [#user_location_information{
	  instance = instance(),
	  type = 0,
	  mcc = mcc(),
	  mnc = mnc(),
	  lac = uint16(),
	  ci = uint16()
	 },
       #user_location_information{
	  instance = instance(),
	  type = 1,
	  mcc = mcc(),
	  mnc = mnc(),
	  lac = uint16(),
	  sac = uint16()
	 },
       #user_location_information{
	  instance = instance(),
	  type = 2,
	  mcc = mcc(),
	  mnc = mnc(),
	  lac = uint16(),
	  rac = uint8()
	 }]).

%% gen_v2_serving_network() ->
%%     #v2_serving_network{
%%        instance = instance(),
%%        mcc = mcc(),
%%        mnc = mnc()
%%       }.

gen_cause() ->
    #cause{
       instance = instance(),
       value =
	   oneof(
	     [request_imsi,
	      request_imei,
	      request_imsi_and_imei,
	      no_identity_needed,
	      ms_refuses,
	      ms_is_not_gprs_responding,
	      reactivation_requested,
	      pdp_address_inactivity_timer_expires,
	      network_failure,
	      qos_parameter_mismatch,
	      system_failure,
	      the_transmit_buffers_are_becoming_full,
	      the_receive_buffers_are_becoming_full,
	      another_node_is_about_to_go_down,
	      this_node_is_about_to_go_down,
	      request_accepted,
	      new_pdp_type_due_to_network_preference,
	      new_pdp_type_due_to_single_address_bearer_only,
	      cdr_decoding_error,
	      non_existent,
	      invalid_message_format,
	      imsi_imei_not_known,
	      ms_is_gprs_detached,
	      ms_is_not_gprs_responding,
	      ms_refuses,
	      version_not_supported,
	      no_resources_available,
	      service_not_supported,
	      mandatory_ie_incorrect,
	      mandatory_ie_missing,
	      optional_ie_incorrect,
	      system_failure,
	      roaming_restriction,
	      p_tmsi_signature_mismatch,
	      gprs_connection_suspended,
	      authentication_failure,
	      user_authentication_failed,
	      context_not_found,
	      all_dynamic_pdp_addresses_are_occupied,
	      no_memory_is_available,
	      relocation_failure,
	      unknown_mandatory_extension_header,
	      semantic_error_in_the_tft_operation,
	      syntactic_error_in_the_tft_operation,
	      semantic_errors_in_packet_filter,
	      syntactic_errors_in_packet_filter,
	      missing_or_unknown_apn,
	      unknown_pdp_address_or_pdp_type,
	      pdp_context_without_tft_already_activated,
	      apn_access_denied___no_subscription,
	      apn_restriction_type_incompatibility_with_currently_active_pdp_contexts,
	      ms_mbms_capabilities_insufficient,
	      invalid_correlation_id,
	      mbms_bearer_context_superseded,
	      bearer_control_mode_violation,
	      collision_with_network_initiated_request,
	      apn_congestion,
	      bearer_handling_not_supported,
	      target_access_restricted_for_the_subscriber,
	      request_related_to_possibly_duplicated_packets_already_fulfilled,
	      request_already_fulfilled,
	      sequence_numbers_of_released_cancelled_packets_ie_incorrect,
	      request_not_fulfilled])
      }.

gen_international_mobile_subscriber_identity() ->
    #international_mobile_subscriber_identity{
       instance = instance(),
       imsi = imsi()
      }.


gen_temporary_logical_link_identity() ->
    #temporary_logical_link_identity{
       instance = instance(),
       tlli = binary(4)
      }.

gen_packet_tmsi() ->
    #packet_tmsi{
       instance = instance(),
       p_tmsi = binary(4)
      }.

gen_reordering_required() ->
    #reordering_required{
       instance = instance(),
       required = oneof(['no', 'yes'])
      }.

gen_authentication_triplet() ->
    #authentication_triplet{
       instance = instance(),
       rand = binary(16),
       sres = binary(4),
       kc = binary(8)
      }.

gen_map_cause() ->
    #map_cause{
       instance = instance(),
       value = binary(1)
      }.

gen_p_tmsi_signature() ->
    #p_tmsi_signature{
       instance = instance(),
       value = binary(3)
      }.

gen_ms_validated() ->
    #ms_validated{
       instance = instance(),
       validated = oneof(['no', 'yes'])
      }.

gen_recovery() ->
    #recovery{
       instance = instance(),
       restart_counter = uint8()
      }.

gen_selection_mode() ->
    #selection_mode{
       instance = instance(),
       mode = int_range(0,3)
      }.

gen_tunnel_endpoint_identifier_data_i() ->
    #tunnel_endpoint_identifier_data_i{
       instance = instance(),
       tei = tei()
      }.

gen_tunnel_endpoint_identifier_control_plane() ->
    #tunnel_endpoint_identifier_control_plane{
       instance = instance(),
       tei = tei()
      }.

gen_tunnel_endpoint_identifier_data_ii() ->
    #tunnel_endpoint_identifier_data_ii{
       instance = instance(),
       nsapi = 0,
       tei = tei()
      }.

gen_teardown_ind() ->
    #teardown_ind{
       instance = instance(),
       value = oneof([0, 1])
      }.

gen_nsapi() ->
    #nsapi{
       instance = instance(),
       nsapi = uint4()
      }.

gen_ranap_cause() ->
    #ranap_cause{
       instance = instance(),
       value = uint8()
      }.

gen_rab_context() ->
    #rab_context{
       instance = instance(),
       nsapi = uint4(),
       dl_gtp_u_sequence_number = uint16(),
       ul_gtp_u_sequence_number = uint16(),
       dl_pdcp_sequence_number = uint16(),
       ul_pdcp_sequence_number = uint16()
      }.

gen_radio_priority_sms() ->
    #radio_priority_sms{
       instance = instance(),
       value = int_range(0,7)
      }.

gen_radio_priority() ->
    #radio_priority{
       instance = instance(),
       nsapi = uint4(),
       value = int_range(0,7)
      }.

gen_packet_flow_id() ->
    #packet_flow_id{
       instance = instance(),
       nsapi = uint4(),
       value = uint8()
      }.

gen_charging_characteristics() ->
    #charging_characteristics{
       instance = instance(),
       value = binary(2)
      }.

gen_trace_reference() ->
    #trace_reference{
       instance = instance(),
       value = uint16()
      }.

gen_trace_type() ->
    #trace_type{
       instance = instance(),
       value = uint16()
      }.

gen_ms_not_reachable_reason() ->
    #ms_not_reachable_reason{
       instance = instance(),
       value = uint8()
      }.

gen_packet_transfer_command() ->
    #packet_transfer_command{
       instance = instance(),
       command = oneof([send_data_record_packet,
			send_possibly_duplicated_data_record_packet,
			cancel_data_record_packet,
			release_data_record_packet])
      }.

gen_charging_id() ->
    #charging_id{
       instance = instance(),
       id = binary(4)
      }.

gen_end_user_address() ->
    #end_user_address{
       instance = instance(),
       pdp_type_organization = uint4(),
       pdp_type_number = uint8(),
       pdp_address = binary()
      }.

%% TODO: the vector lenght en/decoding in this element looks wrong...
gen_mm_context_gsm() ->
    #mm_context_gsm{
       instance = instance(),
       cksn = uint4(),
       no_of_vectors = int_range(0,7),
       used_cipher = int_range(0,7),
       kc = binary(8),
       tripple = [], %% TODO
       drx_parameter = binary(2),
       ms_network_capability_length = 0,
       ms_network_capability = [],
       container_length = 0,
       container = []
      }.

%% TODO: the vector lenght en/decoding in this element looks wrong...
gen_mm_context_umts() ->
    #mm_context_umts{
       instance = instance(),
       ksi = uint4(),
       no_of_vectors = 0,
       ck = binary(16),
       ik = binary(16),
       quintuplet_length = 0,
       quintuplet = [],
       drx_parameter = binary(2),
       ms_network_capability_length = 0,
       ms_network_capability = [],
       container_length = 0,
       container = []
      }.

%% TODO: the vector lenght en/decoding in this element looks wrong...
gen_mm_context_gsm_and_umts() ->
    #mm_context_gsm_and_umts{
       instance = instance(),
       cksn = uint4(),
       no_of_vectors = 0,
       used_cipher = int_range(0,7),
       kc = binary(8),
       quintuplet_length = 0,
       quintuplet = [],
       drx_parameter = <<0,0>>,
       ms_network_capability_length = 0,
       ms_network_capability = [],
       container_length = 0,
       container = []
      }.

%% TODO: the vector lenght en/decoding in this element looks wrong...
gen_mm_context_umts_and_used_cipher() ->
    #mm_context_umts_and_used_cipher{
       instance = instance(),
       ksi = uint4(),
       no_of_vectors = 0,
       used_cipher = int_range(0,7),
       ck = binary(16),
       ik = binary(16),
       quintuplet_length = 0,
       quintuplet = [],
       drx_parameter = <<0,0>>,
       ms_network_capability_length = 0,
       ms_network_capability = [],
       container_length = 0,
       container = []
      }.

gen_pdp_context() ->
    #pdp_context{
       instance = instance()
      }.

gen_access_point_name() ->
    #access_point_name{
       instance = instance(),
       apn = apn()
      }.

gen_random_list_of_pco() ->
    list(oneof(
	   [{ipcp,'CP-Configure-Request',0,
	     [{ms_dns1,<<0,0,0,0>>},{ms_dns2,<<0,0,0,0>>}]},
	    {13, <<>>},
	    {65280, <<19,1,132>>},
	    {12, <<>>},
	    {10, <<>>},
	    {16, <<>>},
	    {13, ip4_address()},
	    {ipcp,'CP-Configure-Nak',0,
	     [{ms_dns1, ip4_address()},{ms_dns2, ip4_address()}]},
	    {5,<<2>>}])).

gen_protocol_configuration_options() ->
    #protocol_configuration_options{
       instance = instance(),
       config = {0, gen_random_list_of_pco()}
      }.

gen_gsn_address() ->
    #gsn_address{
       instance = instance(),
       address = oneof([ip4_address(), ip6_address()])
      }.

gen_ms_international_pstn_isdn_number() ->
    #ms_international_pstn_isdn_number{
       instance = instance(),
       msisdn = msisdn()
      }.

gen_quality_of_service_profile() ->
    #quality_of_service_profile{
       instance = instance(),
       priority = uint8(),
       data = binary()
      }.

gen_authentication_quintuplet() ->
    #authentication_quintuplet{
       instance = instance()
      }.

gen_traffic_flow_template() ->
    #traffic_flow_template{
       instance = instance()
      }.

gen_target_identification() ->
    #target_identification{
       instance = instance()
      }.

gen_utran_transparent_container() ->
    #utran_transparent_container{
       instance = instance()
      }.

gen_rab_setup_information() ->
    #rab_setup_information{
       instance = instance()
      }.

gen_extension_header_type_list() ->
    #extension_header_type_list{
       instance = instance()
      }.

gen_trigger_id() ->
    #trigger_id{
       instance = instance()
      }.

gen_omc_identity() ->
    #omc_identity{
       instance = instance()
      }.

gen_ran_transparent_container() ->
    #ran_transparent_container{
       instance = instance()
      }.

gen_pdp_context_prioritization() ->
    #pdp_context_prioritization{
       instance = instance()
      }.

gen_additional_rab_setup_information() ->
    #additional_rab_setup_information{
       instance = instance()
      }.

gen_sgsn_number() ->
    #sgsn_number{
       instance = instance()
      }.

gen_common_flags() ->
    #common_flags{
       instance = instance(),
       flags =
	   flags(
	     ['Dual Address Bearer Flag',
	      'Upgrade QoS Supported',
	      'NRSN',
	      'No QoS negotiation',
	      'MBMS Counting Information',
	      'RAN Procedures Ready',
	      'MBMS Service Type',
	      'Prohibit Payload Compression'])
      }.

gen_apn_restriction() ->
    #apn_restriction{
       instance = instance()
      }.

gen_radio_priority_lcs() ->
    #radio_priority_lcs{
       instance = instance()
      }.

gen_rat_type() ->
    #rat_type{
       instance = instance(),
       rat_type = uint8()
      }.


gen_ms_time_zone() ->
    #ms_time_zone{
       instance = instance(),
       timezone = uint8(),
       dst = int_range(0,3)
      }.

gen_imei() ->
    #imei{
       instance = instance(),
       imei = imei()
      }.

gen_camel_charging_information_container() ->
    #camel_charging_information_container{
       instance = instance()
      }.

gen_mbms_ue_context() ->
    #mbms_ue_context{
       instance = instance()
      }.

gen_temporary_mobile_group_identity() ->
    #temporary_mobile_group_identity{
       instance = instance()
      }.

gen_rim_routing_address() ->
    #rim_routing_address{
       instance = instance()
      }.

gen_mbms_protocol_configuration_options() ->
    #mbms_protocol_configuration_options{
       instance = instance()
      }.

gen_mbms_service_area() ->
    #mbms_service_area{
       instance = instance()
      }.

gen_source_rnc_pdcp_context_info() ->
    #source_rnc_pdcp_context_info{
       instance = instance()
      }.

gen_additional_trace_info() ->
    #additional_trace_info{
       instance = instance()
      }.

gen_hop_counter() ->
    #hop_counter{
       instance = instance()
      }.

gen_selected_plmn_id() ->
    #selected_plmn_id{
       instance = instance()
      }.

gen_mbms_session_identifier() ->
    #mbms_session_identifier{
       instance = instance()
      }.

gen_mbms_2g_3g_indicator() ->
    #mbms_2g_3g_indicator{
       instance = instance()
      }.

gen_enhanced_nsapi() ->
    #enhanced_nsapi{
       instance = instance()
      }.

gen_mbms_session_duration() ->
    #mbms_session_duration{
       instance = instance()
      }.

gen_additional_mbms_trace_info() ->
    #additional_mbms_trace_info{
       instance = instance()
      }.

gen_mbms_session_repetition_number() ->
    #mbms_session_repetition_number{
       instance = instance()
      }.

gen_mbms_time_to_data_transfer() ->
    #mbms_time_to_data_transfer{
       instance = instance()
      }.

gen_bss_container() ->
    #bss_container{
       instance = instance()
      }.

gen_cell_identification() ->
    #cell_identification{
       instance = instance()
      }.

gen_pdu_numbers() ->
    #pdu_numbers{
       instance = instance()
      }.

gen_bssgp_cause() ->
    #bssgp_cause{
       instance = instance()
      }.

gen_required_mbms_bearer_capabilities() ->
    #required_mbms_bearer_capabilities{
       instance = instance()
      }.

gen_rim_routing_address_discriminator() ->
    #rim_routing_address_discriminator{
       instance = instance()
      }.

gen_list_of_set_up_pfcs() ->
    #list_of_set_up_pfcs{
       instance = instance()
      }.

gen_ps_handover_xid_parameters() ->
    #ps_handover_xid_parameters{
       instance = instance()
      }.

gen_ms_info_change_reporting_action() ->
    #ms_info_change_reporting_action{
       instance = instance()
      }.

gen_direct_tunnel_flags() ->
    #direct_tunnel_flags{
       instance = instance()
      }.

gen_correlation_id() ->
    #correlation_id{
       instance = instance()
      }.

gen_bearer_control_mode() ->
    #bearer_control_mode{
       instance = instance()
      }.

gen_mbms_flow_identifier() ->
    #mbms_flow_identifier{
       instance = instance()
      }.

gen_mbms_ip_multicast_distribution() ->
    #mbms_ip_multicast_distribution{
       instance = instance()
      }.

gen_mbms_distribution_acknowledgement() ->
    #mbms_distribution_acknowledgement{
       instance = instance()
      }.

gen_reliable_inter_rat_handover_info() ->
    #reliable_inter_rat_handover_info{
       instance = instance()
      }.

gen_rfsp_index() ->
    #rfsp_index{
       instance = instance()
      }.

gen_fully_qualified_domain_name() ->
    #fully_qualified_domain_name{
       instance = instance(),
       fqdn = dns_name()
      }.

gen_evolved_allocation_retention_priority_i() ->
    #evolved_allocation_retention_priority_i{
       instance = instance(),
       pci = oneof([0, 1]),
       pl = uint4(),
       pvi = oneof([0, 1])
      }.

gen_evolved_allocation_retention_priority_ii() ->
    #evolved_allocation_retention_priority_ii{
       instance = instance()
      }.

gen_extended_common_flags() ->
    #extended_common_flags{
       instance = instance(),
       flags =
	   flags(['CCRSI', 'CPSR', 'RetLoc', 'VB', 'PCRI', 'BDWI', 'UASI'])
      }.

gen_user_csg_information() ->
    #user_csg_information{
       instance = instance()
      }.

gen_csg_information_reporting_action() ->
    #csg_information_reporting_action{
       instance = instance()
      }.

gen_csg_id() ->
    #csg_id{
       instance = instance()
      }.

gen_csg_membership_indication() ->
    #csg_membership_indication{
       instance = instance()
      }.

gen_aggregate_maximum_bit_rate() ->
    #aggregate_maximum_bit_rate{
       instance = instance(),
       uplink = uint32(),
       downlink = uint32()
      }.

gen_ue_network_capability() ->
    #ue_network_capability{
       instance = instance()
      }.

gen_ue_ambr() ->
    #ue_ambr{
       instance = instance()
      }.

gen_apn_ambr_with_nsapi() ->
    #apn_ambr_with_nsapi{
       instance = instance()
      }.

gen_ggsn_back_off_time() ->
    #ggsn_back_off_time{
       instance = instance()
      }.

gen_signalling_priority_indication() ->
    #signalling_priority_indication{
       instance = instance()
      }.

gen_signalling_priority_indication_with_nsapi() ->
    #signalling_priority_indication_with_nsapi{
       instance = instance()
      }.

gen_higher_bitrates_than_16_mbps_flag() ->
    #higher_bitrates_than_16_mbps_flag{
       instance = instance()
      }.

gen_additional_mm_context_for_srvcc() ->
    #additional_mm_context_for_srvcc{
       instance = instance()
      }.

gen_additional_flags_for_srvcc() ->
    #additional_flags_for_srvcc{
       instance = instance()
      }.

gen_stn_sr() ->
    #stn_sr{
       instance = instance()
      }.

gen_c_msisdn() ->
    #c_msisdn{
       instance = instance()
      }.

gen_extended_ranap_cause() ->
    #extended_ranap_cause{
       instance = instance()
      }.

gen_enodeb_id() ->
    #enodeb_id{
       instance = instance()
      }.

gen_selection_mode_with_nsapi() ->
    #selection_mode_with_nsapi{
       instance = instance()
      }.

gen_uli_timestamp() ->
    #uli_timestamp{
       instance = instance()
      }.

gen_local_home_network_id_with_nsapi() ->
    #local_home_network_id_with_nsapi{
       instance = instance()
      }.

gen_cn_operator_selection_entity() ->
    #cn_operator_selection_entity{
       instance = instance()
      }.

gen_sequence_numbers_of_released_packets() ->
    #sequence_numbers_of_released_packets{
       instance = instance(),
       sequence_numbers = uint16_array()
      }.

gen_sequence_numbers_of_cancelled_packets() ->
    #sequence_numbers_of_cancelled_packets{
       instance = instance(),
       sequence_numbers = uint16_array()
      }.

gen_charging_gateway_address() ->
    #charging_gateway_address{
       instance = instance(),
       address = oneof([ip4_address(), ip6_address()])
      }.

gen_data_record_packet() ->
    #data_record_packet{
       instance = instance(),
       format = int_range(1, 3),
       application = int_range(1, 7),
       version = {int_range(0, 7), int_range(0,9)},
       records =
	   ?LET(I, int_range(1,10), vector(I, binary()))
      }.

gen_requests_responded() ->
    #requests_responded{
       instance = instance(),
       sequence_numbers = uint16_array()
      }.

gen_address_of_recommended_node() ->
    #address_of_recommended_node{
       instance = instance(),
       address = oneof([ip4_address(), ip6_address()])
      }.











gen_private_extension() ->
    #private_extension{
       instance = instance()
      }.

%% v2 generator ==========================================================================

gen_v2_international_mobile_subscriber_identity() ->
    #v2_international_mobile_subscriber_identity{
       instance = instance(),
       imsi = imsi()
      }.

gen_v2_cause() ->
    #v2_cause{
       instance = instance(),
       v2_cause =
	   oneof([reserved,
		  local_detach,
		  complete_detach,
		  rat_changed_from_3gpp_to_non_3gpp,
		  isr_deactivation,
		  error_indication_received_from_rnc_enodeb_s4_sgsn,
		  imsi_detach_only,
		  reactivation_requested,
		  pdn_reconnection_to_this_apn_disallowed,
		  access_changed_from_non_3gpp_to_3gpp,
		  pdn_connection_inactivity_timer_expires,
		  pgw_not_responding,
		  network_failure,
		  qos_parameter_mismatch,
		  request_accepted,
		  request_accepted_partially,
		  new_pdn_type_due_to_network_preference,
		  new_pdn_type_due_to_single_address_bearer_only,
		  context_not_found,
		  invalid_message_format,
		  version_not_supported_by_next_peer,
		  invalid_length,
		  service_not_supported,
		  mandatory_ie_incorrect,
		  mandatory_ie_missing,
		  system_failure,
		  no_resources_available,
		  semantic_error_in_the_tft_operation,
		  syntactic_error_in_the_tft_operation,
		  semantic_errors_in_packet_filter,
		  syntactic_errors_in_packet_filter,
		  missing_or_unknown_apn,
		  gre_key_not_found,
		  relocation_failure,
		  denied_in_rat,
		  preferred_pdn_type_not_supported,
		  all_dynamic_addresses_are_occupied,
		  ue_context_without_tft_already_activated,
		  protocol_type_not_supported,
		  ue_not_responding,
		  ue_refuses,
		  service_denied,
		  unable_to_page_ue,
		  no_memory_available,
		  user_authentication_failed,
		  apn_access_denied___no_subscription,
		  request_rejected,
		  p_tmsi_signature_mismatch,
		  imsi_imei_not_known,
		  semantic_error_in_the_tad_operation,
		  syntactic_error_in_the_tad_operation,
		  remote_peer_not_responding,
		  collision_with_network_initiated_request,
		  unable_to_page_ue_due_to_suspension,
		  conditional_ie_missing,
		  apn_restriction_type_incompatible_with_currently_active_pdn_connection,
		  invalid_overall_length_of_the_triggered_response_message_and_a_piggybacked_initial_message,
		  data_forwarding_not_supported,
		  invalid_reply_from_remote_peer,
		  fallback_to_gtpv1,
		  invalid_peer,
		  temporarily_rejected_due_to_handover_tau_rau_procedure_in_progress,
		  modifications_not_limited_to_s1_u_bearers,
		  request_rejected_for_a_pmipv6_reason,
		  apn_congestion,
		  bearer_handling_not_supported,
		  ue_already_re_attached,
		  multiple_pdn_connections_for_a_given_apn_not_allowed,
		  target_access_restricted_for_the_subscriber,
		  mme_sgsn_refuses_due_to_vplmn_policy,
		  gtp_c_entity_congestion,
		  late_overlapping_request,
		  timed_out_request,
		  ue_is_temporarily_not_reachable_due_to_power_saving,
		  relocation_failure_due_to_nas_message_redirection,
		  ue_not_authorised_by_ocs_or_external_aaa_server,
		  multiple_accesses_to_a_pdn_connection_not_allowed,
		  request_rejected_due_to_ue_capability,
		  s1_u_path_failure,
		  '5gc_not_allowed']),
       pce = oneof([0,1]),
       bce = oneof([0,1]),
       cs = oneof([0,1]),
       offending_ie = oneof([undefined, binary(4)])
      }.

gen_v2_recovery() ->
    #v2_recovery{
       instance = instance(),
       restart_counter = uint8()
      }.

gen_v2_stn_sr() ->
    #v2_stn_sr{
       instance = instance()
      }.

gen_v2_access_point_name() ->
    #v2_access_point_name{
       instance = instance(),
       apn = apn()
      }.

gen_v2_aggregate_maximum_bit_rate() ->
    #v2_aggregate_maximum_bit_rate{
       instance = instance(),
       uplink = uint32(),
       downlink = uint32()
      }.

gen_v2_eps_bearer_id() ->
    #v2_eps_bearer_id{
       instance = instance(),
       eps_bearer_id = uint4()
      }.

gen_v2_ip_address() ->
    #v2_ip_address{
       instance = instance(),
       ip = oneof([ip4_address(), ip6_address()])
      }.

gen_v2_mobile_equipment_identity() ->
    #v2_mobile_equipment_identity{
       instance = instance(),
       mei = imei()
      }.

gen_v2_msisdn() ->
    #v2_msisdn{
       instance = instance(),
       msisdn = binstr_number(7,20)
      }.

gen_v2_indication() ->
    #v2_indication{
       instance = instance(),
       flags =
	   flags(
	     ['DAF', 'DTF', 'HI', 'DFI', 'OI', 'ISRSI', 'ISRAI', 'SGWCI',
	      'SQCI', 'UIMSI', 'CFSI', 'CRSI', 'P', 'PT', 'SI', 'MSV',
	      'RetLoc', 'PBIC', 'SRNI', 'S6AF', 'S4AF', 'MBMDT', 'ISRAU', 'CCRSI',
	      'CPRAI', 'ARRL', 'PPOF', 'PPON/PPEI', 'PPSI', 'CSFBI', 'CLII', 'CPSR',
	      'NSI', 'UASI', 'DTCI', 'BDWI', 'PSCI', 'PCRI', 'AOSI', 'AOPI',
	      'ROAAI', 'EPCOSI', 'CPOPCI', 'PMTSMI', 'S11TF', 'PNSI', 'UNACCSI', 'WPMSI',
	      '5GSNN26', 'REPREFI', '5GSIWK', 'EEVRSI', 'LTEMUI', 'LTEMPI', 'ENBCRSI', 'TSPCMI'])
      }.

gen_v2_protocol_configuration_options() ->
    #v2_protocol_configuration_options{
       instance = instance(),
       config = {0, gen_random_list_of_pco()}
      }.

gen_v2_pdn_address_allocation() ->
    oneof(
      [#v2_pdn_address_allocation{
	  instance = instance(),
	  type = 'ipv4',
	  address = ip4_address()
	 },
       #v2_pdn_address_allocation{
	  instance = instance(),
	  type = 'ipv6',
	  address = ip6_address()
	 },
       #v2_pdn_address_allocation{
	  instance = instance(),
	  type = 'ipv4v6',
	  address = ip46_address()
	 }]).

gen_v2_bearer_level_quality_of_service() ->
    #v2_bearer_level_quality_of_service{
       instance = instance(),
       pci = oneof([0, 1]),
       pl = uint4(),
       pvi = oneof([0, 1]),
       label = uint8(),
       maximum_bit_rate_for_uplink = uint40(),
       maximum_bit_rate_for_downlink = uint40(),
       guaranteed_bit_rate_for_uplink = uint40(),
       guaranteed_bit_rate_for_downlink = uint40()
      }.

gen_v2_flow_quality_of_service() ->
    #v2_flow_quality_of_service{
       instance = instance(),
       label = uint8(),
       maximum_bit_rate_for_uplink = uint40(),
       maximum_bit_rate_for_downlink = uint40(),
       guaranteed_bit_rate_for_uplink = uint40(),
       guaranteed_bit_rate_for_downlink = uint40()
      }.

gen_v2_rat_type() ->
    #v2_rat_type{
       instance = instance(),
       rat_type = uint8()
      }.

gen_v2_eps_bearer_level_traffic_flow_template() ->
    #v2_eps_bearer_level_traffic_flow_template{
       instance = instance(),
       value = binary()
      }.

gen_v2_traffic_aggregation_description() ->
    #v2_traffic_aggregation_description{
       instance = instance(),
       value = binary()
      }.

gen_v2_user_location_information() ->
    #v2_user_location_information{
       instance = instance(),
       cgi = oneof([undefined, binary(7)]),
       sai = oneof([undefined, binary(7)]),
       rai = oneof([undefined, binary(7)]),
       tai = oneof([undefined, binary(5)]),
       ecgi = oneof([undefined, binary(7)]),
       lai = oneof([undefined, binary(5)]),
       macro_enb = oneof([undefined, binary(6)]),
       ext_macro_enb = oneof([undefined, binary(6)])
      }.

gen_v2_fully_qualified_tunnel_endpoint_identifier() ->
    oneof(
      [#v2_fully_qualified_tunnel_endpoint_identifier{
	  instance = instance(),
	  interface_type = int_range(0,16#3f),
	  key = uint32(),
	  ipv4 = ip4_address()
	 },
       #v2_fully_qualified_tunnel_endpoint_identifier{
	  instance = instance(),
	  interface_type = int_range(0,16#3f),
	  key = uint32(),
	  ipv6 = ip6_address()
	 },
       #v2_fully_qualified_tunnel_endpoint_identifier{
	  instance = instance(),
	  interface_type = int_range(0,16#3f),
	  key = uint32(),
	  ipv4 = ip4_address(),
	  ipv6 = ip6_address()
	 }]).

gen_v2_tmsi() ->
    #v2_tmsi{
       instance = instance(),
       value = uint32()
      }.

gen_v2_global_cn_id() ->
    #v2_global_cn_id{
       instance = instance(),
       value = binary(7)
      }.

gen_v2_s103_pdn_data_forwarding_info() ->
    #v2_s103_pdn_data_forwarding_info{
       instance = instance(),
       hsgw_address = oneof([ip4_address(),ip6_address()]),
       gre_key = uint32(),
       eps_bearer_id = list(uint8())
      }.

gen_v2_s1_u_data_forwarding_info() ->
    #v2_s1_u_data_forwarding_info{
       instance = instance()
      }.

gen_v2_delay_value() ->
    #v2_delay_value{
       instance = instance(),
       delay = uint8()
      }.

gen_v2_bearer_context() ->
    #v2_bearer_context{
       instance = instance(),
       group = v2_ie_group()
      }.

gen_v2_charging_id() ->
    #v2_charging_id{
       instance = instance(),
       id = binary(4)
      }.

gen_v2_charging_characteristics() ->
    #v2_charging_characteristics{
       instance = instance()
      }.

gen_v2_trace_information() ->
    #v2_trace_information{
       instance = instance(),
       mcc = mcc(),
       mnc = mnc(),
       trace_id = uint32(),
       triggering_events = binary(9),
       list_of_ne_types = uint16(),
       session_trace_depth = uint8(),
       list_of_interfaces = binary(12),
       ip_address_of_trace_collection_entity =
	   oneof([ip4_address(), ip6_address()])
      }.

gen_v2_bearer_flags() ->
    #v2_bearer_flags{
       instance = instance(),
       flags = flags(['ASI', 'Vind', 'VB', 'PCC'])
      }.

gen_v2_pdn_type() ->
    #v2_pdn_type{
       instance = instance(),
       pdn_type = oneof(['ipv4', 'ipv6', 'ipv4v6'])
      }.

gen_v2_procedure_transaction_id() ->
    #v2_procedure_transaction_id{
       instance = instance(),
       pti = uint8()
      }.

gen_v2_mm_context_1() ->
    #v2_mm_context_1{
       instance = instance()
      }.

gen_v2_mm_context_2() ->
    #v2_mm_context_2{
       instance = instance()
      }.

gen_v2_mm_context_3() ->
    #v2_mm_context_3{
       instance = instance()
      }.

gen_v2_mm_context_4() ->
    #v2_mm_context_4{
       instance = instance()
      }.

gen_v2_mm_context_5() ->
    #v2_mm_context_5{
       instance = instance()
      }.

gen_v2_mm_context_6() ->
    #v2_mm_context_6{
       instance = instance()
      }.

gen_v2_pdn_connection() ->
    #v2_pdn_connection{
       instance = instance(),
       group = v2_ie_group()
      }.

gen_v2_pdu_numbers() ->
    #v2_pdu_numbers{
       instance = instance(),
       nsapi = uint4(),
       dl_gtp_u_sequence_number = uint16(),
       ul_gtp_u_sequence_number = uint16(),
       send_n_pdu_number = uint16(),
       receive_n_pdu_number = uint16()
      }.

gen_v2_p_tmsi() ->
    #v2_p_tmsi{
       instance = instance(),
       value = binary()
      }.

gen_v2_p_tmsi_signature() ->
    #v2_p_tmsi_signature{
       instance = instance(),
       value = binary()
      }.

gen_v2_hop_counter() ->
    #v2_hop_counter{
       instance = instance(),
       hop_counter = uint8()
      }.

gen_v2_ue_time_zone() ->
    #v2_ue_time_zone{
       instance = instance(),
       timezone = uint8(),
       dst = int_range(0,3)
      }.

gen_v2_trace_reference() ->
    #v2_trace_reference{
       instance = instance(),
       mcc = mcc(),
       mnc = mnc(),
       id = uint24()
      }.

gen_v2_complete_request_message() ->
    #v2_complete_request_message{
       instance = instance(),
       type = uint8(),
       message = binary()
      }.

gen_v2_guti() ->
    #v2_guti{
       instance = instance(),
       mcc = mcc(),
       mnc = mnc(),
       group_id = uint16(),
       code = uint24(),
       m_tmsi = binary()
      }.

gen_v2_f_container() ->
    #v2_f_container{
       instance = instance(),
       type = uint4(),
       data = binary()
      }.

gen_v2_f_cause() ->
    #v2_f_cause{
       instance = instance(),
       type = uint4(),
       data = binary()
      }.

gen_v2_plmn_id() ->
    #v2_plmn_id{
       instance = instance(),
       id = binary(3)
      }.

gen_v2_target_identification() ->
    #v2_target_identification{
       instance = instance(),
       type = uint8(),
       data = binary()
      }.

gen_v2_packet_flow_id() ->
    #v2_packet_flow_id{
       instance = instance(),
       ebi = uint4(),
       flow_id = binary()
      }.

gen_v2_rab_context() ->
    #v2_rab_context{
       instance = instance(),
       ulpsi = flag(),
       dlpsi = flag(),
       ulgsi = flag(),
       dlgsi = flag(),
       nsapi = uint4(),
       dl_gtp_u_sequence_number = uint16(),
       ul_gtp_u_sequence_number = uint16(),
       dl_pdcp_number = uint16(),
       ul_pdcp_number = uint16()
      }.

gen_v2_source_rnc_pdcp_context_info() ->
    #v2_source_rnc_pdcp_context_info{
       instance = instance(),
       rrc_container = binary()
      }.

gen_v2_udp_source_port_number() ->
    #v2_udp_source_port_number{
       instance = instance(),
       port = uint16()
      }.

gen_v2_apn_restriction() ->
    #v2_apn_restriction{
       instance = instance(),
       restriction_type_value = uint8()
      }.

gen_v2_selection_mode() ->
    #v2_selection_mode{
       instance = instance(),
       mode = int_range(0, 3)
      }.

gen_v2_source_identification() ->
    #v2_source_identification{
       instance = instance(),
       target_cell_id = binary(8),
       source_type = uint8(),
       source_id = binary()
      }.

gen_v2_change_reporting_action() ->
    #v2_change_reporting_action{
       instance = instance(),
       action = oneof([stop_reporting,
		       start_reporting_cgi_sai,
		       start_reporting_rai,
		       start_reporting_tai,
		       start_reporting_ecgi,
		       start_reporting_cgi_sai_and_rai,
		       start_reporting_tai_and_ecgi,
		       start_reporting_macro_enodeb_id_and_extended_macro_enodeb_id,
		       start_reporting_tai__macro_enodeb_id_and_extended_macro_enodeb_id])
      }.

gen_v2_fully_qualified_pdn_connection_set_identifier() ->
    oneof([
	   #v2_fully_qualified_pdn_connection_set_identifier{
	      instance = instance(),
	      node_id_type = 0,
	      node_id = ip4_address(),
	      csids = ?LET(I, uint4(), vector(I, uint16()))
	     },
	   #v2_fully_qualified_pdn_connection_set_identifier{
	      instance = instance(),
	      node_id_type = 1,
	      node_id = ip6_address(),
	      csids = ?LET(I, uint4(), vector(I, uint16()))
	     },
	   #v2_fully_qualified_pdn_connection_set_identifier{
	      instance = instance(),
	      node_id_type = 2,
	      node_id = {int_range(0,999), int_range(0,999), int_range(0,16#0fff)},
	      csids = ?LET(I, uint4(), vector(I, uint16()))
	     }]).

gen_v2_channel_needed() ->
    #v2_channel_needed{
       instance = instance(),
       value = binary()
      }.

gen_v2_emlpp_priority() ->
    #v2_emlpp_priority{
       instance = instance(),
       value = binary()
      }.

gen_v2_node_type() ->
    #v2_node_type{
       instance = instance(),
       node_type = uint8()
      }.

gen_v2_fully_qualified_domain_name() ->
    #v2_fully_qualified_domain_name{
       instance = instance(),
       fqdn = dns_name()
      }.

gen_v2_transaction_identifier() ->
    #v2_transaction_identifier{
       instance = instance(),
       value = binary()
      }.

gen_v2_mbms_session_duration() ->
    #v2_mbms_session_duration{
       instance = instance()
      }.

gen_v2_mbms_service_area() ->
    #v2_mbms_service_area{
       instance = instance()
      }.

gen_v2_mbms_session_identifier() ->
    #v2_mbms_session_identifier{
       instance = instance()
      }.

gen_v2_mbms_flow_identifier() ->
    #v2_mbms_flow_identifier{
       instance = instance()
      }.

gen_v2_mbms_ip_multicast_distribution() ->
    #v2_mbms_ip_multicast_distribution{
       instance = instance()
      }.

gen_v2_mbms_distribution_acknowledge() ->
    #v2_mbms_distribution_acknowledge{
       instance = instance()
      }.

gen_v2_rfsp_index() ->
    #v2_rfsp_index{
       instance = instance(),
       value = uint16()
      }.

gen_v2_user_csg_information() ->
    #v2_user_csg_information{
       instance = instance(),
       mcc = mcc(),
       mnc = mnc(),
       csg_id = bitstring(27),
       access_mode = int_range(0,3),
       lcsg = boolean(),
       cmi = flag()
      }.

gen_v2_csg_information_reporting_action() ->
    #v2_csg_information_reporting_action{
       instance = instance(),
       actions = flags(['UCIUHC', 'UCISHC', 'UCICSG'])
      }.

gen_v2_csg_id() ->
    #v2_csg_id{
       instance = instance(),
       id = bitstring(27)
      }.

gen_v2_csg_membership_indication() ->
    #v2_csg_membership_indication{
       instance = instance(),
       cmi = flag()
      }.

gen_v2_service_indicator() ->
    #v2_service_indicator{
       instance = instance(),
       value = uint8()
      }.

gen_v2_detach_type() ->
    #v2_detach_type{
       instance = instance(),
       value = uint8()
      }.

gen_v2_local_distiguished_name() ->
    #v2_local_distiguished_name{
       instance = instance(),
       value = binary(1, 400)
      }.

gen_v2_node_features() ->
    #v2_node_features{
       instance = instance(),
       features =
	   flags(['ETH', 'S1UN', 'CIOT', 'NTSR', 'MABR', 'PRN'])
      }.

gen_v2_mbms_time_to_data_transfer() ->
    #v2_mbms_time_to_data_transfer{
       instance = instance()
      }.

gen_v2_throttling() ->
    #v2_throttling{
       instance = instance(),
       unit = int_range(0,7),
       value = int_range(0,31),
       factor = uint8()
      }.

gen_v2_allocation_retention_priority() ->
    #v2_allocation_retention_priority{
       instance = instance(),
       pci = boolean(),
       pl = uint4(),
       pvi = boolean()
      }.

gen_v2_epc_timer() ->
    #v2_epc_timer{
       instance = instance(),
       unit = int_range(0,7),
       value = int_range(0,31)
      }.

gen_v2_signalling_priority_indication() ->
    #v2_signalling_priority_indication{
       instance = instance(),
       indication = flags(['LAPI'])
      }.

gen_v2_temporary_mobile_group_identity() ->
    #v2_temporary_mobile_group_identity{
       instance = instance()
      }.

gen_v2_additional_mm_context_for_srvcc() ->
    #v2_additional_mm_context_for_srvcc{
       instance = instance(),
       classmark_2 = binary(),
       classmark_3 = binary(),
       codec_list = binary()

      }.

gen_v2_additional_flags_for_srvcc() ->
    #v2_additional_flags_for_srvcc{
       instance = instance(),
       flags = flags(['VF', 'ICS'])
      }.

gen_v2_mdt_configuration() ->
    #v2_mdt_configuration{
       instance = instance()
      }.

gen_v2_additional_protocol_configuration_options() ->
    #v2_additional_protocol_configuration_options{
       instance = instance(),
       config = {0, gen_random_list_of_pco()}
      }.

gen_v2_absolute_time_of_mbms_data_transfer() ->
    #v2_absolute_time_of_mbms_data_transfer{
       instance = instance()
      }.

gen_v2_henb_information_reporting_() ->
    #v2_henb_information_reporting_{
       instance = instance(),
       flags = flags(['FTI'])
      }.

gen_v2_ipv4_configuration_parameters() ->
    #v2_ipv4_configuration_parameters{
       instance = instance(),
       prefix_length = int_range(0,32),
       default_route = ip4_address()
      }.

gen_v2_change_to_report_flags_() ->
    #v2_change_to_report_flags_{
       instance = instance(),
       flags = flags(['TZCR', 'SNCR'])
     }.

gen_v2_action_indication() ->
    #v2_action_indication{
       instance = instance(),
       indication = int_range(0,7)
      }.

gen_v2_twan_identifier() ->
    oneof(
      [#v2_twan_identifier{
	  instance = instance(),
	  ssid = binary(),
	  bssid = oneof([undefined, binary(6)]),
	  civic_address = oneof([undefined, binary()]),
	  plmn_id = oneof([undefined, {mcc(), mnc()}]),
	  operator_name = oneof([undefined, binary()]),
	  relay_identity_type = undefined,
	  relay_identity = undefined,
	  circuit_id = undefined
	 },
       #v2_twan_identifier{
	  instance = instance(),
	  ssid = binary(),
	  bssid = oneof([undefined, binary(6)]),
	  civic_address = oneof([undefined, binary()]),
	  plmn_id = oneof([undefined, {mcc(), mnc()}]),
	  operator_name = oneof([undefined, binary()]),
	  relay_identity_type = uint8(),
	  relay_identity = binary(),
	  circuit_id = binary()
	 }]).

gen_v2_uli_timestamp() ->
    #v2_uli_timestamp{
       instance = instance(),
       timestamp = uint32()
      }.

gen_v2_mbms_flags() ->
    #v2_mbms_flags{
       instance = instance()
      }.

gen_v2_ran_nas_cause() ->
    #v2_ran_nas_cause{
       instance = instance(),
       protocol = uint4(),
       type = uint4(),
       cause = binary()
     }.

gen_v2_cn_operator_selection_entity() ->
    #v2_cn_operator_selection_entity{
       instance = instance(),
       entity = int_range(0,3)
      }.

gen_v2_trusted_wlan_mode_indication() ->
    #v2_trusted_wlan_mode_indication{
       instance = instance(),
       indication = flags(['MCM', 'SCM'])
      }.

gen_v2_node_number() ->
    #v2_node_number{
       instance = instance(),
       number = binary()
      }.

gen_v2_node_identifier() ->
    #v2_node_identifier{
       instance = instance(),
       name = binary(),
       realm = binary()
      }.

gen_v2_presence_reporting_area_action() ->
    #v2_presence_reporting_area_action{
       instance = instance()
      }.

gen_v2_presence_reporting_area_information() ->
    #v2_presence_reporting_area_information{
       instance = instance()
      }.

gen_v2_twan_identifier_timestamp() ->
    #v2_twan_identifier_timestamp{
       instance = instance(),
       timestamp = uint32()
      }.

gen_v2_overload_control_information() ->
    #v2_overload_control_information{
       instance = instance(),
       group = v2_ie_group()
      }.

gen_v2_load_control_information() ->
    #v2_load_control_information{
       instance = instance(),
       group = v2_ie_group()
      }.

gen_v2_metric() ->
    #v2_metric{
       instance = instance(),
       value = uint8()
      }.

gen_v2_sequence_number() ->
    #v2_sequence_number{
       instance = instance(),
       value = uint32()
      }.

gen_v2_apn_and_relative_capacity() ->
    #v2_apn_and_relative_capacity{
       instance = instance(),
       capacity = uint8(),
       apn = binary()
      }.

gen_v2_wlan_offloadability_indication() ->
    #v2_wlan_offloadability_indication{
       instance = instance(),
       indication = flags(['EUTRAN', 'UTRAN'])
      }.

gen_v2_private_extension() ->
    #v2_private_extension{
       instance = instance()
      }.
