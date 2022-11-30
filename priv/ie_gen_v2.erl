#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

-mode(compile).

-define(V1_TAG, <<"%% -include(\"gtp_packet_v1_gen.hrl\").">>).
-define(V2_TAG, <<"%% -include(\"gtp_packet_v2_gen.hrl\").">>).

raw_ies() ->
    [
     {1, "v2 International Mobile Subscriber Identity",
      [{"IMSI", 0, {type, tbcd}}]},
     {2, "v2 Cause", 5,
      [{"v2 Cause", 8, {enum, [{1, "Reserved"},
			       {2, "Local Detach"},
			       {3, "Complete Detach"},
			       {4, "RAT changed from 3GPP to Non-3GPP"},
			       {5, "ISR deactivation"},
			       {6, "Error Indication received from RNC/eNodeB/S4-SGSN"},
			       {7, "IMSI Detach Only"},
			       {8, "Reactivation Requested"},
			       {9, "PDN reconnection to this APN disallowed"},
			       {10, "Access changed from Non-3GPP to 3GPP"},
			       {11, "PDN connection inactivity timer expires"},
			       {12, "PGW not responding"},
			       {13, "Network Failure"},
			       {14, "QoS parameter mismatch"},
			       {16, "Request accepted"},
			       {17, "Request accepted partially"},
			       {18, "New PDN type due to network preference"},
			       {19, "New PDN type due to single address bearer only"},
			       {64, "Context Not Found"},
			       {65, "Invalid Message Format"},
			       {66, "Version not supported by next peer"},
			       {67, "Invalid length"},
			       {68, "Service not supported"},
			       {69, "Mandatory IE incorrect"},
			       {70, "Mandatory IE missing"},
			       {72, "System failure"},
			       {73, "No resources available"},
			       {74, "Semantic error in the TFT operation"},
			       {75, "Syntactic error in the TFT operation"},
			       {76, "Semantic errors in packet filter"},
			       {77, "Syntactic errors in packet filter"},
			       {78, "Missing or unknown APN"},
			       {80, "GRE key not found"},
			       {81, "Relocation failure"},
			       {82, "Denied in RAT"},
			       {83, "Preferred PDN type not supported"},
			       {84, "All dynamic addresses are occupied"},
			       {85, "UE context without TFT already activated"},
			       {86, "Protocol type not supported"},
			       {87, "UE not responding"},
			       {88, "UE refuses"},
			       {89, "Service denied"},
			       {90, "Unable to page UE"},
			       {91, "No memory available"},
			       {92, "User authentication failed"},
			       {93, "APN access denied - no subscription"},
			       {94, "Request rejected"},
			       {95, "P-TMSI Signature mismatch"},
			       {96, "IMSI/IMEI not known"},
			       {97, "Semantic error in the TAD operation"},
			       {98, "Syntactic error in the TAD operation"},
			       {100, "Remote peer not responding"},
			       {101, "Collision with network initiated request"},
			       {102, "Unable to page UE due to Suspension"},
			       {103, "Conditional IE missing"},
			       {104, "APN Restriction type Incompatible with currently active PDN connection"},
			       {105, "Invalid overall length of the triggered response message and a piggybacked initial message"},
			       {106, "Data forwarding not supported"},
			       {107, "Invalid reply from remote peer"},
			       {108, "Fallback to GTPv1"},
			       {109, "Invalid peer"},
			       {110, "Temporarily rejected due to handover/TAU/RAU procedure in progress"},
			       {111, "Modifications not limited to S1-U bearers"},
			       {112, "Request rejected for a PMIPv6 reason"},
			       {113, "APN Congestion"},
			       {114, "Bearer handling not supported"},
			       {115, "UE already re-attached"},
			       {116, "Multiple PDN connections for a given APN not allowed"},
			       {117, "Target access restricted for the subscriber"},
			       {119, "MME/SGSN refuses due to VPLMN Policy"},
			       {120, "GTP-C Entity Congestion"},
			       {121, "Late Overlapping Request"},
			       {122, "Timed out Request"},
			       {123, "UE is temporarily not reachable due to power saving"},
			       {124, "Relocation failure due to NAS message redirection"},
			       {125, "UE not authorised by OCS or external AAA Server"},
			       {126, "Multiple accesses to a PDN connection not allowed"},
			       {127, "Request rejected due to UE capability"},
			       {128, "S1-U Path Failure"},
			       {129, "5GC not allowed"}]}},
       {'_', 5},
       {"PCE", 1, integer},
       {"BCE", 1, integer},
       {"CS", 1, integer},
       {"offending IE", 4, bytes},
       {'_', 0}]},
     {3, "v2 Recovery",
      [{"Restart counter", 8, integer},
       {'_', 0}]},
     {51, "v2 STN-SR", []},
     {71, "v2 Access Point Name",
      [{"APN", 0, {type, fqdn}}]},
     {72, "v2 Aggregate Maximum Bit Rate",
      [{"Uplink", 32, integer},
       {"Downlink", 32, integer}]},
     {73, "v2 EPS Bearer ID",
      [{'_', 4},
       {"EPS Bearer ID", 4, integer},
       {'_', 0}]},
     {74, "v2 IP Address",
      [{"IP", 0, binary}]},
     {75, "v2 Mobile Equipment Identity",
      [{"MEI", 0, {type, tbcd}}]},
     {76, "v2 MSISDN",
      [{"MSISDN", 0, {type, tbcd}}]},
     {77, "v2 Indication",
      [{"Flags", 16,
	{flags,	['DAF', 'DTF', 'HI', 'DFI', 'OI', 'ISRSI', 'ISRAI', 'SGWCI',
		 'SQCI', 'UIMSI', 'CFSI', 'CRSI', 'P', 'PT', 'SI', 'MSV',
		 'RetLoc', 'PBIC', 'SRNI', 'S6AF', 'S4AF', 'MBMDT', 'ISRAU', 'CCRSI',
		 'CPRAI', 'ARRL', 'PPOF', 'PPON/PPEI', 'PPSI', 'CSFBI', 'CLII', 'CPSR',
		 'NSI', 'UASI', 'DTCI', 'BDWI', 'PSCI', 'PCRI', 'AOSI', 'AOPI',
		 'ROAAI', 'EPCOSI', 'CPOPCI', 'PMTSMI', 'S11TF', 'PNSI', 'UNACCSI', 'WPMSI',
		 '5GSNN26', 'REPREFI', '5GSIWK', 'EEVRSI', 'LTEMUI', 'LTEMPI', 'ENBCRSI', 'TSPCMI',
		 'CSRMFI', 'MTEDTN', 'MTEDTA', 'N5GNMI', '5GCNRS', '5GCNRI', '5SRHOI', 'ETHPDN',
		 '_', '_', '_', '_', 'SISSME', 'NSENBI', 'IPFUPF', 'EMCI']}}]},
     {78, "v2 Protocol Configuration Options",
      [{"Config", protocol_config_opts}]},
     {79, "v2 PDN Address Allocation",
      [{'_', 5},
       {"Type", 3, {enum, [{1, "IPv4"},
			   {2, "IPv6"},
			   {3, "IPv4v6"},
			   {4, "Non-IP"},
			   {5, "Ethernet"}]}},
       {"Address", 0, binary}]},
     {80, "v2 Bearer Level Quality of Service",
      [{'_', 1},
       {"PCI", 1, integer},
       {"PL", 4, integer},
       {'_', 1},
       {"PVI", 1, integer},
       {"Label", 8, integer},
       {"Maximum bit rate for uplink", 40, integer},
       {"Maximum bit rate for downlink", 40, integer},
       {"Guaranteed bit rate for uplink", 40, integer},
       {"Guaranteed bit rate for downlink", 40, integer},
       {'_', 0}]},
     {81, "v2 Flow Quality of Service",
      [{"Label", 8, integer},
       {"Maximum bit rate for uplink", 40, integer},
       {"Maximum bit rate for downlink", 40, integer},
       {"Guaranteed bit rate for uplink", 40, integer},
       {"Guaranteed bit rate for downlink", 40, integer},
       {'_', 0}]},
     {82, "v2 RAT Type",
      [{"RAT Type", 8, integer},
       {'_', 0}]},
     {83, "v2 Serving Network",
      [{"PLMN", mccmnc},
       {'_', 0}]},
     {84, "v2 EPS Bearer Level Traffic Flow Template",
      [{"Value", 0, binary}]},
     {85, "v2 Traffic Aggregation Description",
      [{"Value", 0, binary}]},
     {86, "v2 User Location Information", v2_user_location_information},
     {87, "v2 Fully Qualified Tunnel Endpoint Identifier", v2_fully_qualified_tunnel_endpoint_identifier},
     {88, "v2 TMSI",
      [{"Value", 32, integer}]},
     {89, "v2 Global CN-Id",
      [{"PLMN", mccmnc},
       {"Value", 0, binary}]},
     {90, "v2 S103 PDN Data Forwarding Info",
      [{"HSGW Address", 8, length_binary},
       {"GRE Key", 32, integer},
       {"EPS Bearer Id", 8, {array, {8, integer}}}]},
     {91, "v2 S1-U Data Forwarding Info",
      [{"Service GW Address", 8, length_binary},
       {"TEID", 32, integer}]},
     {92, "v2 Delay Value",
      [{"Delay", 8, integer},
       {'_', 0}]},
     {93, "v2 Bearer Context",
      [{"Group", 0, {type, v2_grouped}}]},
     {94, "v2 Charging ID",
      [{"Id", 4, bytes},
       {'_', 0}]},
     {95, "v2 Charging Characteristics",
      [{"Value", 2, bytes},
       {'_', 0}]},
     {96, "v2 Trace Information",
      [{"PLMN", mccmnc},
       {"Trace ID", 32, integer},
       {"Triggering Events", 9, bytes},
       {"List of NE Types", 16, integer},
       {"Session Trace Depth", 8, integer},
       {"List of Interfaces", 12, bytes},
       {"IP Address of Trace Collection Entity", 0, binary}]},
     {97, "v2 Bearer Flags",
      [{"Flags", 0, {flags, ['_', '_', '_', '_', 'ASI', 'Vind', 'VB', 'PCC']}}]},
     {99, "v2 PDN Type",
      [{'_', 4},
       {"PDN Type", 4, {enum, [{1, "IPv4"},
			       {2, "IPv6"},
			       {3, "IPv4v6"},
			       {4, "Non-IP"}]}},
       {'_', 0}]},
     {100, "v2 Procedure Transaction ID",
      [{"PTI", 8, integer},
       {'_', 0}]},
     {103, "v2 MM Context 1", []},
     {104, "v2 MM Context 2", []},
     {105, "v2 MM Context 3", []},
     {106, "v2 MM Context 4", []},
     {107, "v2 MM Context 5", []},
     {108, "v2 MM Context 6", []},
     {109, "v2 PDN Connection",
      [{"Group", 0, {type, v2_grouped}}]},
     {110, "v2 PDU Numbers",
      [{'_', 4},
       {"NSAPI", 4, integer},
       {"DL GTP-U Sequence Number", 16, integer},
       {"UL GTP-U Sequence Number", 16, integer},
       {"Send N-PDU Number", 16, integer},
       {"Receive N-PDU Number", 16, integer},
       {'_', 0}]},
     {111, "v2 P-TMSI",
      [{"Value", 0, binary}]},
     {112, "v2 P-TMSI Signature",
      [{"Value", 0, binary}]},
     {113, "v2 Hop Counter",
      [{"Hop Counter", 8, integer},
       {'_', 0}]},
     {114, "v2 UE Time Zone",
      [{"TimeZone", 8, integer},
       {'_', 6},
       {"DST", 2, integer},
       {'_', 0}]},
     {115, "v2 Trace Reference",
      [{"PLMN", mccmnc},
       {"Id", 24, integer}]},
     {116, "v2 Complete Request Message",
      [{"Type", 8, integer},
       {"Message", 0, binary}]},
     {117, "v2 GUTI",
      [{"PLMN", mccmnc},
       {"Group Id", 16, integer},
       {"Code", 24, integer},
       {"M-TMSI", 0, binary}]},
     {118, "v2 F-Container",
      [{'_', 4},
       {"Type", 4, integer},
       {"Data", 0, binary}]},
     {119, "v2 F-Cause",
      [{'_', 4},
       {"Type", 4, integer},
       {"Data", 0, binary}]},
     {120, "v2 PLMN ID",
      [{"Id", 3, bytes}]},
     {121, "v2 Target Identification",
      [{"Type", 8, integer},
       {"Data", 0, binary}]},
     {123, "v2 Packet Flow ID",
      [{'_', 4},
       {"EBI", 4, integer},
       {"Flow Id", 0, binary}]},
     {124, "v2 RAB Context",
      [{"ULPSI", 1, integer},
       {"DLPSI", 1, integer},
       {"ULGSI", 1, integer},
       {"DLGSI", 1, integer},
       {"NSAPI", 4, integer},
       {"DL GTP-U Sequence Number", 16, integer},
       {"UL GTP-U Sequence Number", 16, integer},
       {"DL PDCP Number", 16, integer},
       {"UL PDCP Number", 16, integer}]},
     {125, "v2 Source RNC PDCP Context Info",
      [{"RRC Container", 0, binary}]},
     {126, "v2 UDP Source Port Number",
      [{"Port", 16, integer},
       {'_', 0}]},
     {127, "v2 APN Restriction",
      [{"Restriction Type Value", 8, integer},
       {'_', 0}]},
     {128, "v2 Selection Mode",
      [{'_', 6},
       {"Mode", 2, integer},
       {'_', 0}]},
     {129, "v2 Source Identification",
      [{"Target Cell Id", 8, binary},
       {"Source Type", 8, integer},
       {"Source Id", 0, binary}]},
     {131, "v2 Change Reporting Action",
      [{"Action", 8, {enum, [{0, "Stop Reporting"},
			     {1, "Start Reporting CGI/SAI"},
			     {2, "Start Reporting RAI"},
			     {3, "Start Reporting TAI"},
			     {4, "Start Reporting ECGI"},
			     {5, "Start Reporting CGI/SAI and RAI"},
			     {6, "Start Reporting TAI and ECGI"},
			     {7, "Start Reporting Macro eNodeB ID and Extended Macro eNodeB ID"},
			     {8, "Start Reporting TAI, Macro eNodeB ID and Extended Macro eNodeB ID"}
			    ]}},
       {'_', 0}]},
     {132, "v2 Fully Qualified PDN Connection Set Identifier",
      v2_fully_qualified_pdn_connection_set_identifier},
     {133, "v2 Channel needed",
      [{"Value", 0, binary}]},
     {134, "v2 eMLPP Priority",
      [{"Value", 0, binary}]},
     {135, "v2 Node Type",
      [{"Node Type", 8, integer},
       {'_', 0}]},
     {136, "v2 Fully Qualified Domain Name",
      [{"FQDN", 0, {type, fqdn}}]},
     {137, "v2 Transaction Identifier",
      [{"Value", 0, binary}]},
     {138, "v2 MBMS Session Duration", []},
     {139, "v2 MBMS Service Area", []},
     {140, "v2 MBMS Session Identifier", []},
     {141, "v2 MBMS Flow Identifier", []},
     {142, "v2 MBMS IP Multicast Distribution", []},
     {143, "v2 MBMS Distribution Acknowledge", []},
     {144, "v2 RFSP Index",
      [{"Value", 16, integer}]},
     {145, "v2 User CSG Information",
      [{"PLMN", mccmnc},
       {'_', 5},
       {"CSG Id", 27, bits},
       {"Access mode", 2, integer},
       {'_', 4},
       {"LCSG", 1, boolean},
       {"CMI", 1, integer}]},
     {146, "v2 CSG Information Reporting Action",
      [{"Actions", 0, {flags, ['_', '_', '_', '_', '_', 'UCIUHC', 'UCISHC', 'UCICSG']}}]},
     {147, "v2 CSG ID",
      [{'_', 5},
       {"Id", 27, bits},
       {'_', 0}]},
     {148, "v2 CSG Membership Indication",
      [{'_', 7},
       {"CMI", 1, integer},
       {'_', 0}]},
     {149, "v2 Service indicator",
      [{"Value", 8, integer}]},
     {150, "v2 Detach Type",
      [{"Value", 8, integer}]},
     {151, "v2 Local Distiguished Name",
      [{"Value", 0, binary}]},
     {152, "v2 Node Features",
      [{"Features", 0, {flags, ['_', '_', 'ETH', 'S1UN', 'CIOT', 'NTSR', 'MABR', 'PRN']}}]},
     {153, "v2 MBMS Time to Data Transfer", []},
     {154, "v2 Throttling",
      [{"Unit", 3, integer},
       {"Value", 5, integer},
       {"Factor", 8, integer},
       {'_', 0}]},
     {155, "v2 Allocation/Retention Priority",
      [{'_', 1},
       {"PCI", 1, boolean},
       {"PL", 4, integer},
       {'_', 1},
       {"PVI", 1, boolean},
       {'_', 0}]},
     {156, "v2 EPC Timer",
      [{"Unit", 3, integer},
       {"Value", 5, integer},
       {'_', 0}]},
     {157, "v2 Signalling Priority Indication",
      [{"Indication", 0, {flags, ['_', '_', '_', '_', '_', '_', '_', 'LAPI']}}]},
     {158, "v2 Temporary Mobile Group Identity", []},
     {159, "v2 Additional MM context for SRVCC",
      [{"Classmark 2", 8, length_binary},
       {"Classmark 3", 8, length_binary},
       {"Codec List", 8, length_binary},
       {'_', 0}]},
     {160, "v2 Additional flags for SRVCC",
      [{"Flags", 0, {flags, ['_', '_', '_', '_', '_', '_', 'VF', 'ICS']}}]},
     {162, "v2 MDT Configuration", []},
     {163, "v2 Additional Protocol Configuration Options",
      [{"Config", protocol_config_opts}]},
     {164, "v2 Absolute Time of MBMS Data Transfer", []},
     {165, "v2 HeNB Information Reporting ",
      [{"Flags", 0, {flags, ['_', '_', '_', '_', '_', '_', '_', 'FTI']}}]},
     {166, "v2 IPv4 Configuration Parameters",
      [{"Prefix Length", 8, integer},
       {"Default Route", 4, bytes},
       {'_', 0}]},
     {167, "v2 Change to Report Flags ",
      [{"Flags", 0, {flags, ['_', '_', '_', '_', '_', '_', 'TZCR', 'SNCR']}}]},
     {168, "v2 Action Indication",
      [{'_', 5},
       {"Indication", 3, integer},
       {'_', 0}]},
     {169, "v2 TWAN Identifier", v2_twan_identifier},
     {170, "v2 ULI Timestamp",
      [{"Timestamp", 32, integer},
       {'_', 0}]},
     {171, "v2 MBMS Flags", []},
     {172, "v2 RAN/NAS Cause",
      [{"Protocol", 4, integer},
       {"Type", 4, integer},
       {"Cause", 0, binary}]},
     {173, "v2 CN Operator Selection Entity",
       [{'_', 6},
	{"Entity", 2, integer},
	{'_', 0}]},
     {174, "v2 Trusted WLAN Mode Indication",
      [{"Indication", 0, {flags, ['_', '_', '_', '_', '_', '_', 'MCM', 'SCM']}}]},
     {175, "v2 Node Number",
      [{"Number", 8, length_binary},                    %% TBD: ISDN string
       {'_', 0}]},
     {176, "v2 Node Identifier",
      [{"Name", 8, length_binary},
       {"Realm", 8, length_binary},
       {'_', 0}]},
     {177, "v2 Presence Reporting Area Action", []},
     {178, "v2 Presence Reporting Area Information", []},
     {179, "v2 TWAN Identifier Timestamp",
      [{"Timestamp", 32, integer},
       {'_', 0}]},
     {180, "v2 Overload Control Information",
      [{"Group", 0, {type, v2_grouped}}]},
     {181, "v2 Load Control Information",
      [{"Group", 0, {type, v2_grouped}}]},
     {182, "v2 Metric",
      [{"Value", 8, integer}]},
     {183, "v2 Sequence Number",
      [{"Value", 32, integer}]},
     {184, "v2 APN and Relative Capacity",
      [{"Capacity", 8, integer},
       {"APN", 8, length_binary},                    %% TBD: APN encoding
       {'_', 0}]},
     {185, "v2 WLAN Offloadability Indication",
      [{"Indication", 0, {flags, ['_', '_', '_', '_', '_', '_', 'EUTRAN', 'UTRAN']}}]},
     {186, "v2 Paging and Service Information", v2_paging_and_service_information},
     {187, "v2 Integer Number", v2_integer_number},
     {188, "v2 Millisecond Time Stamp",
      [{"Timestamp", 48, integer},
       {'_', 0}]},
     {189, "v2 Monitoring Event Information", []},  % WTF: flags encoded in the spare bits
						    %      of the Instance field
     {190, "v2 ECGI List",
      [{"ECGIs", 16, {array, {7, bytes}}},
       {'_', 0}]},
     {191, "v2 Remote UE Context",
      [{"Group", 0, {type, v2_grouped}}]},
     {192, "v2 Remote User ID", v2_remote_user_id},
     {193, "v2 Remote UE IP information",
      [{"IP", 0, binary}]},
     {194, "v2 CIoT Optimizations Support Indication",
      [{"Indication", 0, {flags, ['_', '_', '_', '_', 'IHCSI', 'AWOPDN', 'SCNIPDN', 'SGNIPDN']}}]},
     {195, "v2 SCEF PDN Connection",
      [{"Group", 0, {type, v2_grouped}}]},
     {196, "v2 Header Compression Configuration",
      [{"ROHC Profiles", 16, integer},
       {"MAX CID", 16, integer},
       {'_', 0}]},
     {197, "v2 Extended Protocol Configuration Options",
      [{"Config", protocol_config_opts}]},
     {198, "v2 Serving PLMN Rate Control",
      [{"Uplink", 16, integer},
       {"Downlink", 16, integer},
       {'_', 0}]},
     {199, "v2 Counter",
      [{"Timestamp", 32, integer},
       {"Counter", 8, integer},
       {'_', 0}]},
     {200, "v2 Mapped UE Usage Type",
      [{"Usage Type", 16, integer},
       {'_', 0}]},
     {201, "v2 Secondary RAT Usage Data Report",
      [{'_', 6},
       {"IRSGW", 1, boolean},
       {"IRPGW", 1, boolean},
       {"RAT Type", 8, integer},
       {'_', 4},
       {"EBI", 4, integer},
       {"Start Time", 32, integer},
       {"End Time", 32, integer},
       {"DL", 64, integer},
       {"UL", 64, integer},
       {'_', 0}]},
     {202, "v2 UP Function Selection Indication Flags",
      [{"Indication", 0, {flags, ['_', '_', '_', '_', '_', '_', '_', 'DCNR']}}]},
     {203, "v2 Maximum Packet Loss Rate", v2_maximum_packet_loss_rate},
     {204, "v2 APN Rate Control Status",
      [{"Number of Uplink packets allowed", 32, integer},
       {"Number of additional exception reports", 32, integer},
       {"Number of Downlink packets allowed", 32, integer},
       {"APN Rate Control Status validity Time", 64, integer},
       {'_', 0}]},
     {205, "v2 Extended Trace Information",
      [{"PLMN", mccmnc},
       {"Trace ID", 32, integer},
       {"Triggering Events", 8, length_binary},
       {"List of NE Types", 8, length_binary},
       {"Session Trace Depth", 8, integer},
       {"List of Interfaces", 8, length_binary},
       {"IP Address of Trace Collection Entity", 8, length_binary},
       {'_', 0}]},
     {206, "v2 Monitoring Event Extension Information", v2_monitoring_event_extension_information},
     {207, "v2 Additional RRM Policy Index",
      [{"Value", 32, integer}]},
     {255, "v2 Private Extension", v2_private_extension}].

msgs() ->
    [{1, "Echo Request"},
     {2, "Echo Response"},
     {3, "Version Not Supported"},
     {32, "Create Session Request"},
     {33, "Create Session Response"},
     {36, "Delete Session Request"},
     {37, "Delete Session Response"},
     {34, "Modify Bearer Request"},
     {35, "Modify Bearer Response"},
     {38, "Change Notification Request"},
     {39, "Change Notification Response"},
     {64, "Modify Bearer Command"},
     {65, "Modify Bearer Failure Indication"},
     {66, "Delete Bearer Command"},
     {67, "Delete Bearer Failure Indication"},
     {68, "Bearer Resource Command"},
     {69, "Bearer Resource Failure Indication"},
     {70, "Downlink Data Notification Failure Indication"},
     {71, "Trace Session Activation"},
     {72, "Trace Session Deactivation"},
     {73, "Stop Paging Indication"},
     {95, "Create Bearer Request"},
     {96, "Create Bearer Response"},
     {97, "Update Bearer Request"},
     {98, "Update Bearer Response"},
     {99, "Delete Bearer Request"},
     {100, "Delete Bearer Response"},
     {101, "Delete PDN Connection Set Request"},
     {102, "Delete PDN Connection Set Response"},
     {103, "PGW Downlink Triggering Notification"},
     {104, "PGW Downlink Triggering Acknowledge"},
     {128, "Identification Request"},
     {129, "Identification Response"},
     {130, "Context Request"},
     {131, "Context Response"},
     {132, "Context Acknowledge"},
     {133, "Forward Relocation Request"},
     {134, "Forward Relocation Response"},
     {135, "Forward Relocation Complete Notification"},
     {136, "Forward Relocation Complete Acknowledge"},
     {137, "Forward Access Context Notification"},
     {138, "Forward Access Context Acknowledge"},
     {139, "Relocation Cancel Request"},
     {140, "Relocation Cancel Response"},
     {141, "Configuration Transfer Tunnel"},
     {149, "Detach Notification"},
     {150, "Detach Acknowledge"},
     {151, "CS Paging Indication"},
     {152, "RAN Information Relay"},
     {153, "Alert MME Notification"},
     {154, "Alert MME Acknowledge"},
     {155, "UE Activity Notification"},
     {156, "UE Activity Acknowledge"},
     {157, "ISR Status Indication"},
     {160, "Create Forwarding Tunnel Request"},
     {161, "Create Forwarding Tunnel Response"},
     {162, "Suspend Notification"},
     {163, "Suspend Acknowledge"},
     {164, "Resume Notification"},
     {165, "Resume Acknowledge"},
     {166, "Create Indirect Data Forwarding Tunnel Request"},
     {167, "Create Indirect Data Forwarding Tunnel Response"},
     {168, "Delete Indirect Data Forwarding Tunnel Request"},
     {169, "Delete Indirect Data Forwarding Tunnel Response"},
     {170, "Release Access Bearers Request"},
     {171, "Release Access Bearers Response"},
     {176, "Downlink Data Notification"},
     {177, "Downlink Data Notification Acknowledge"},
     {179, "PGW Restart Notification"},
     {180, "PGW Restart Notification Acknowledge"},
     {200, "Update PDN Connection Set Request"},
     {201, "Update PDN Connection Set Response"},
     {232, "MBMS Session Start Response"},
     {233, "MBMS Session Update Request"},
     {234, "MBMS Session Update Response"},
     {235, "MBMS Session Stop Request"},
     {236, "MBMS Session Stop Response"}
    ].

-type flag() :: any().
-type enum() :: any().
-type array_def() :: any().
-type field_type() ::
    {flags, [flag()]} |
    {enum, [enum()]} |
    boolean |
    integer |
    bits |
    bytes |
    binary |
    length_binary |
    {array, array_def()} |
    tuple().

-record(ie, {id, name, type, min_field_count, fields}).
-record(field, {name, len, optional, type, spec}).

-define('Instance', #field{name = 'instance', type = integer}).
-define('WildCard', #field{type = '_', len = 0}).
-define('DecoderFunName', "decode_v2_element").
-define('EncoderFunName', "encode_v2_element").

ies() ->
    TypeFF = fun(Type,          F) when is_atom(Type) -> F#field{type = Type};
		({type,  Type}, F) when is_atom(Type) -> F#field{type = helper, spec = Type};
		({array, Size}, F) when is_integer(Size) -> F#field{type = array, spec = {1, byte}};
		({Type,  Spec}, F) when is_atom(Type) -> F#field{type = Type, spec = Spec}
	     end,
    FieldF = fun({Name, Len, Type}, Optional, F) when is_integer(Len) ->
		     [TypeFF(Type, #field{name = s2a(Name), len = Len,
					  optional = Optional}) | F];
		({Name, Type}, Optional, F) when is_list(Name), is_atom(Type) ->
		     [#field{name = s2a(Name), len = 0, optional = Optional,
			     type = helper, spec = Type} | F];
		({'_', Len}, Optional, F) when is_integer(Len) ->
		     [#field{len = Len, optional = Optional, type = '_'} | F]
	     end,
    SpecF = fun(Fields, #ie{min_field_count = MinLen} = IE) when is_list(Fields) ->
		    {FieldDef, _} =
			lists:foldl(
			  fun(Field, {F, Cnt}) ->
				  {FieldF(Field, Cnt >= MinLen, F), Cnt + 1} end,
			  {[], 0}, Fields),
		    IE#ie{fields = lists:reverse(FieldDef)};
	      (Helper, IE) when is_atom(Helper) ->
		    IE#ie{type = Helper}
	   end,
    lists:map(
      fun ({Id, Name, Spec}) ->
	      SpecF(Spec, #ie{id = Id, name = s2a(Name)});
	  ({Id, Name, MinLen, Spec}) ->
	      SpecF(Spec, #ie{id = Id, name = s2a(Name), min_field_count = MinLen})
      end, raw_ies()).

%% gen_record_def({Value, _}) when is_integer(Value); is_atom(Value) ->

%% gen_record_def(#field{len = undefined}) ->
%%     [];
gen_record_def(#field{type = '_'}) ->
    [];
gen_record_def(#field{spec = mccmnc}) ->
    ["plmn_id = {<<\"001\">>, <<\"001\">>}"];
gen_record_def(#field{name = Name, optional = true}) ->
    [to_string(Name)];
gen_record_def(#field{name = Name, type = flags}) ->
    [io_lib:format("~s = #{}", [Name])];
gen_record_def(#field{name = Name, type = enum, spec = [{_,H}|_]}) ->
    [io_lib:format("~s = ~s", [Name, s2a(H)])];
gen_record_def(#field{name = Name, type = enum, spec = [H|_]}) ->
    [io_lib:format("~s = ~s", [Name, s2a(H)])];
gen_record_def(#field{name = Name, type = boolean}) ->
    [io_lib:format("~s = false", [Name])];
gen_record_def(#field{name = Name, type = integer}) ->
    [io_lib:format("~s = 0", [Name])];
gen_record_def(#field{name = Name, len = Size, type = bits}) ->
    [io_lib:format("~s = ~w", [Name, <<0:Size>>])];
gen_record_def(#field{name = Name, len = Size, type = bytes}) ->
    [io_lib:format("~s = ~w", [Name, <<0:(Size * 8)>>])];
gen_record_def(#field{name = Name, type = binary}) ->
    [io_lib:format("~s = <<>>", [Name])];
gen_record_def(#field{name = Name, type = length_binary}) ->
    [io_lib:format("~s = <<>>", [Name])];
gen_record_def(#field{name = Name, type = array}) ->
    [io_lib:format("~s = []", [Name])];
gen_record_def(#field{name = Name}) ->
    [to_string(Name)].

gen_decoder_header_match(#field{type = '_', len = 0}) ->
    ["_/binary"];
gen_decoder_header_match(#field{type = '_', len = Size}) ->
    [io_lib:format("_:~w", [Size])];
%% gen_decoder_header_match(#field{Value, Size}) when is_integer(Value); is_atom(Value) ->
%%     [io_lib:format("~w:~w", [Value, Size])];
gen_decoder_header_match(#field{name = Name, spec = mccmnc}) ->
    [io_lib:format("M_~s:3/bytes", [Name])];
gen_decoder_header_match(#field{name = Name, type = flags}) ->
    [io_lib:format("M_~s/binary", [Name])];
gen_decoder_header_match(#field{name = Name, len = Size, type = enum}) ->
    [io_lib:format("M_~s:~w/integer", [Name, Size])];
gen_decoder_header_match(#field{name = Name, type = array, spec = Multi})
  when is_list(Multi) ->
    {stop, [io_lib:format("M_~s_Rest/binary", [Name])]};
gen_decoder_header_match(#field{name = Name, len = Len, type = array}) ->
    {stop, [io_lib:format("M_~s_len:~w/integer, M_~s_Rest/binary", [Name, Len, Name])]};
gen_decoder_header_match(#field{name = Name, len = Len, type = length_binary}) ->
    [io_lib:format("M_~s_len:~w/integer, M_~s:M_~s_len/bytes", [Name, Len, Name, Name])];
gen_decoder_header_match(#field{name = Name, len = 0, type = helper}) ->
    [io_lib:format("M_~s/binary", [Name])];
gen_decoder_header_match(#field{name = Name, len = Size, type = helper}) ->
    [io_lib:format("M_~s:~w/bits", [Name, Size])];
gen_decoder_header_match(#field{name = Name, len = Size, type = boolean}) ->
    [io_lib:format("M_~s:~w/integer", [Name, Size])];
gen_decoder_header_match(#field{name = Name, len = 0, type = Type}) ->
    [io_lib:format("M_~s/~w", [Name, Type])];
gen_decoder_header_match(#field{name = Name, len = Size, type = Type}) ->
    [io_lib:format("M_~s:~w/~s", [Name, Size, Type])].

%% gen_decoder_record_assign(#field{Value, _}) when is_integer(Value); is_atom(Value) ->
%%     [];
gen_decoder_record_assign(#field{type = '_'}) ->
    [];
gen_decoder_record_assign(#field{name = Name, spec = mccmnc}) ->
    [io_lib:format("plmn_id = {decode_mcc(M_~s), decode_mnc(M_~s)}", [Name, Name])];
gen_decoder_record_assign(#field{name = Name, type = flags, spec = Flags}) ->
    [io_lib:format("~s = decode_flags(M_~s, ~p)",
		   [Name, Name, Flags])];

gen_decoder_record_assign(#field{name = Name, type = enum}) ->
    [io_lib:format("~s = enum_v2_~s(M_~s)", [Name, Name, Name])];
gen_decoder_record_assign(#field{name = Name, len = Size, type = array, spec = Multi})
  when is_list(Multi) ->
    [io_lib:format("~s = [X || <<X:~w/bytes>> <= M_~s]", [Name, Size, Name])];
gen_decoder_record_assign(#field{name = Name, type = array, spec = {Size, Type}}) ->
    [io_lib:format("~s = [X || <<X:~w/~s>> <= M_~s]", [Name, Size, Type, Name])];
gen_decoder_record_assign(#field{name = Name, type = helper, spec = TypeName}) ->
    [io_lib:format("~s = decode_~s(M_~s)", [Name, TypeName, Name])];
gen_decoder_record_assign(#field{name = Name, type = boolean}) ->
    [io_lib:format("~s = int2bool(M_~s)", [Name, Name])];
gen_decoder_record_assign(#field{name = Name}) ->
    [io_lib:format("~s = M_~s", [Name, Name])].

%% gen_encoder_record_assign({Value, _}) when is_integer(Value); is_atom(Value) ->
%%     [];
gen_encoder_record_assign(#field{type = '_'}) ->
    [];
gen_encoder_record_assign(#field{spec = mccmnc}) ->
    ["plmn_id = {M_mcc, M_mnc}"];
gen_encoder_record_assign(#field{name = Name, type = undefined}) ->
    [io_lib:format("~s = undefined", [Name])];
gen_encoder_record_assign(#field{name = Name}) ->
    [io_lib:format("~s = M_~s", [Name, Name])].

gen_encoder_bin(#field{type = '_', len = 0}) ->
    [];
gen_encoder_bin(#field{type = '_', len = Size}) ->
    [io_lib:format("0:~w", [Size])];

%% gen_encoder_bin(#field{Value, Size}) when is_integer(Value); is_atom(Value) ->
%%     [io_lib:format("~w:~w", [Value, Size])];
gen_encoder_bin(#field{type = undefined}) ->
    [];
gen_encoder_bin(#field{spec = mccmnc}) ->
    ["(encode_mccmnc(M_mcc, M_mnc))/binary"];
gen_encoder_bin(#field{name = Name, len = MinSize, type = flags, spec = Flags}) ->
    [io_lib:format("(encode_min_int(~p, encode_flags(M_~s, ~p), little))/binary",
		   [MinSize, Name, reorder_flags(Flags)])];
gen_encoder_bin(#field{name = Name, len = Size, type = enum}) ->
    [io_lib:format("(enum_v2_~s(M_~s)):~w/integer", [Name, Name, Size])];
gen_encoder_bin(#field{name = Name, len = Len, type = array, spec = {Size, Type}}) ->
    [io_lib:format("(length(M_~s)):~w/integer, (<< <<X:~w/~w>> || X <- M_~s>>)/binary",
		   [Name, Len, Size, Type, Name])];
gen_encoder_bin(#field{name = Name, len = Len, type = array}) ->
    [io_lib:format("(length(M_~s)):~w/integer, (<< <<X/binary>> || X <- M_~s>>)/binary", [Name, Len, Name])];
gen_encoder_bin(#field{name = Name, len = 0, type = helper, spec = TypeName}) ->
    [io_lib:format("(encode_~s(M_~s))/binary", [TypeName, Name])];
gen_encoder_bin(#field{name = Name, len = Size, type = helper, spec = TypeName}) ->
    [io_lib:format("(encode_~s(M_~s)):~w/bits", [TypeName, Name, Size])];
gen_encoder_bin(#field{name = Name, len = Len, type = length_binary}) ->
    [io_lib:format("(byte_size(M_~s)):~w/integer, M_~s/binary", [Name, Len, Name])];
gen_encoder_bin(#field{name = Name, len = 0, type = Type}) ->
    [io_lib:format("M_~s/~w", [Name, Type])];
gen_encoder_bin(#field{name = Name, len = Size, type = boolean}) ->
    [io_lib:format("(bool2int(M_~s)):~w/integer", [Name, Size])];
gen_encoder_bin(#field{name = Name, len = Size, type = Type}) ->
    [io_lib:format("M_~s:~w/~s", [Name, Size, Type])].
%% gen_encoder_bin(#field{name = Name, len = Size}) ->
%%     [io_lib:format("M_~s:~w", [Name, Size])].

indent(Atom, Extra) when is_atom(Atom) ->
    indent(atom_to_list(Atom), Extra);
indent(List, Extra) ->
    Indent = length(lists:flatten(List)) + Extra,
    Spaces = Indent rem 8,
    Tabs = Indent div 8,
    [lists:duplicate(Tabs, "\t"), lists:duplicate(Spaces, " ")].

s2a(Name) when is_atom(Name) ->
    Name;
s2a(Name) ->
    S = lists:map(fun(32) -> $_;
		     ($/) -> $_;
		     ($-) -> $_;
		     ($.) -> $_;
		     ($,) -> $_;
		     (C)  -> C
		  end,
		  string:to_lower(Name)),
    list_to_atom(S).

to_string(S) when is_list(S)   -> S;
to_string(A) when is_atom(A)   -> atom_to_list(A);
to_string(B) when is_binary(B) -> binary_to_list(B).

append([], Acc) ->
    Acc;
append([H|T], Acc) ->
    append(T, [H|Acc]).

collect(_Fun, [], Acc) ->
    lists:reverse(Acc);
collect(Fun, [F|Fields], Acc) ->
    case Fun(F) of
	[] ->
	    collect(Fun, Fields, Acc);
	{stop, L} ->
	    lists:reverse(append(L, Acc));
	L ->
	    collect(Fun, Fields, append(L, Acc))
    end.

collect(Fun, Fields) ->
    collect(Fun, Fields, []).

gen_enum(Name, Value, Cnt, Next, {FwdFuns, RevFuns}) ->
    Fwd = io_lib:format("enum_v2_~s(~p) -> ~w", [Name, s2a(Value), Cnt]),
    Rev = io_lib:format("enum_v2_~s(~w) -> ~p", [Name, Cnt, s2a(Value)]),
    gen_enum(Name, Next, Cnt + 1, {[Fwd|FwdFuns], [Rev|RevFuns]}).

gen_enum(_, [], _, {FwdFuns, RevFuns}) ->
    {lists:reverse(FwdFuns), lists:reverse(RevFuns)};
gen_enum(Name, [{Cnt, Value}|Rest], _, Acc) ->
    gen_enum(Name, Value, Cnt, Rest, Acc);
gen_enum(Name, [Value|Rest], Cnt, Acc) ->
    gen_enum(Name, Value, Cnt, Rest, Acc).

gen_message_type(Value, Name, Next, {FwdFuns, RevFuns}) ->
    Fwd = io_lib:format("message_type_v2(~s) -> ~w", [s2a(Name), Value]),
    Rev = io_lib:format("message_type_v2(~w) -> ~s", [Value, s2a(Name)]),
    gen_message_type(Next, {[Fwd|FwdFuns], [Rev|RevFuns]}).

gen_message_type([], {FwdFuns, RevFuns}) ->
    {lists:reverse(FwdFuns), lists:reverse(RevFuns)};
gen_message_type([{Value, Name}|Rest], Acc) ->
    gen_message_type(Value, Name, Rest, Acc).

reorder_flags([]) -> [];
reorder_flags(Flags) ->
    {Head, Tail} = lists:split(8, Flags),
    lists:reverse(Head) ++ reorder_flags(Tail).

build_late_assign([]) ->
    [];
build_late_assign([H = #field{type = array} | T]) ->
    build_late_assign(H, T);
build_late_assign([_ | T]) ->
    build_late_assign(T).

build_late_assign(#field{name = Name, len = Len, type = array, spec = Multi}, T)
  when is_list(Multi) ->
    Init = io_lib:format("M_~s_size = M_~s * ~w", [Name, s2a(Multi), Len]),
    build_late_assign(Name, Init, T);
build_late_assign(#field{name = Name, type = array, spec = {Size, Type}}, T)
  when Type =:= integer; Type =:= bits ->
    Init = io_lib:format("M_~s_size = M_~s_len * ~w", [Name, Name, Size]),
    build_late_assign(Name, Init, T);
build_late_assign(#field{name = Name, type = array, spec = {Size, _}}, T) ->
    Init = io_lib:format("M_~s_size = M_~s_len * ~w * 8", [Name, Name, Size]),
    build_late_assign(Name, Init, T).

build_late_assign(Name, Init, Fields) ->
    Match = io_lib:format("M_~s:M_~s_size/bits", [Name, Name]),
    {Body, Next} = collect_late_assign(Fields, [Match]),
    M = io_lib:format("    <<~s>> = M_~s_Rest,", [string:join(Body, ",\n      "), Name]),
    ["    ", Init, ",\n", M, "\n"] ++ build_late_assign(Next).

collect_late_assign([], Acc) ->
    {lists:reverse(Acc), []};
collect_late_assign(Fields = [H | T], Acc) ->
    case gen_decoder_header_match(H) of
	{stop, Match} ->
	    {lists:reverse([Match|Acc]), Fields};
	Match ->
	    collect_late_assign(T, [Match|Acc])
    end.


collect_enum(#field{name = Name, type = enum, spec = Enum}, Acc) ->
    {FwdFuns, RevFuns} = gen_enum(Name, Enum, 0, {[], []}),
    Wildcard = io_lib:format("enum_v2_~s(X) when is_integer(X) -> X", [Name]),
    S = string:join(FwdFuns ++ RevFuns ++ [Wildcard], ";\n") ++ ".\n",
    lists:keystore(Name, 1, Acc, {Name, S});
collect_enum(_, Acc) ->
    Acc.

collect_enums(#ie{type = undefined, fields = Fields}, AccIn) ->
    lists:foldr(fun(X, Acc) -> collect_enum(X, Acc) end, AccIn, Fields);
collect_enums(_, AccIn) ->
    AccIn.

write_enums(IEs) ->
    E = lists:foldr(fun(X, Acc) -> collect_enums(X, Acc) end, [], IEs),
    {_, Str} = lists:unzip(E),
    string:join(Str, "\n").

write_record(#ie{name = Name, type = undefined, fields = Fields}) ->
    Indent = "\t  ",
    RecordDef = string:join(collect(fun gen_record_def/1, [?'Instance' | Fields], []), [",\n", Indent]),
    io_lib:format("-record(~s, {~n~s~s~n}).~n", [Name, Indent, RecordDef]);
write_record(_) ->
    [].

write_decoder(#ie{min_field_count = Min, fields = Fields} = IE, Fns)
  when is_integer(Min), length(Fields) > Min ->
    SubIE = IE#ie{min_field_count = undefined},
    lists:foldl(
      fun (Len, FnsSub) ->
	      {H,T} = lists:split(Len, Fields),
	      case T of
		  [] -> FnsSub;
		  _ ->
		      write_decoder(SubIE#ie{fields = H ++ [?WildCard]}, FnsSub)
	      end
      end, Fns, lists:seq(Min, length(Fields)));

write_decoder(#ie{id = Id, type = undefined, name = Name, fields = Fields}, Fns) ->
    MatchIdent = indent(?DecoderFunName, 3),
    Match = string:join(collect(fun gen_decoder_header_match/1, Fields), [",\n", MatchIdent]),
    Body = build_late_assign(Fields),
    RecIdent = indent(Name, 6),
    RecAssign = string:join(["instance = Instance" |
			     collect(fun gen_decoder_record_assign/1, Fields)], [",\n", RecIdent]),
    F = io_lib:format("~s(<<~s>>, ~w, Instance) ->~n~s    #~s{~s}",
		      [?DecoderFunName, Match, Id, Body, Name, RecAssign]),
    [F | Fns];

write_decoder(#ie{id = Id, type = Helper}, Fns) ->
    F = io_lib:format("~s(<<Data/binary>>, ~w, Instance) ->~n    decode_~s(Data, Instance)",
		      [?DecoderFunName, Id, Helper]),
    [F | Fns].

write_encoder(#ie{min_field_count = Min, fields = Fields} = IE, Fns)
  when is_integer(Min), length(Fields) > Min ->
    SubIE = IE#ie{min_field_count = undefined},
    lists:foldl(
      fun (Len, FnsSub) ->
	      {H,T} = lists:split(Len, Fields),
	      case T of
		  [] ->
		      write_encoder(SubIE#ie{fields = H}, FnsSub);
		  [#field{type = '_'}|_] -> FnsSub;
		  [M|_] ->
		      write_encoder(SubIE#ie{fields = H ++ [M#field{type = undefined}]}, FnsSub)
	      end
      end, Fns, lists:seq(length(Fields), Min, -1));

write_encoder(#ie{id = Id, name = Name, type = undefined, fields = Fields}, Fns) ->
    RecIdent = indent("encode_v2_element(#", 2),
    RecAssign = string:join(["instance = Instance" |
			     collect(fun gen_encoder_record_assign/1, Fields)], [",\n", RecIdent]),
    FunHead = io_lib:format("encode_v2_element(#~s{~n~s~s}) ->~n", [Name, RecIdent, RecAssign]),
    DecHead = io_lib:format("    ~s(~w, Instance, ", [?EncoderFunName, Id]),
    BinIndent = indent(DecHead, 2),
    BinAssign = string:join(collect(fun gen_encoder_bin/1, Fields), [",\n", BinIndent]),
    F = io_lib:format("~s~s<<~s>>)", [FunHead, DecHead, BinAssign]),
    [F | Fns];
write_encoder(#ie{id = Id, name = Name, type = Helper}, Fns) ->
    F = io_lib:format("encode_v2_element(#~s{instance = Instance} = IE) ->~n    ~s(~w, Instance, encode_~s(IE))",
		      [Name, ?EncoderFunName, Id, Helper]),
    [F | Fns].

main(_) ->
    IEs = ies(),

    MsgDescription = string:join([io_lib:format("msg_description_v2(~s) -> <<\"~s\">>", [s2a(X), X]) || {_, X} <- msgs()]
				 ++ ["msg_description_v2(X) -> io_lib:format(\"~p\", [X])"], ";\n") ++ ".\n",

    {FwdFuns, RevFuns} = gen_message_type(msgs(), {[], []}),
    ErrorFun = ["message_type_v2(Type) -> error(badarg, [Type])"],
    MTypes = string:join(FwdFuns ++ RevFuns ++ ErrorFun, ";\n") ++ ".\n",

    Records = string:join([write_record(X) || X <- IEs], "\n"),
    ExpRecs = io_lib:format("-define(GTP_V2_RECORDS, ~p).~n",
			    [[ExpRecName || #ie{name = ExpRecName} <- IEs]]),
    HrlRecs = io_lib:format("~n~n~s~n~s", [ExpRecs, Records]),
    Enums = write_enums(IEs),

    CatchAnyDecoder = ?DecoderFunName ++ "(Value, Tag, Instance) ->\n    {Tag, Instance, Value}",

    DecoderFns = lists:foldr(fun write_decoder/2, [CatchAnyDecoder], IEs),
    Funs = string:join(DecoderFns, ";\n\n"),

    CatchAnyEncoder = ?EncoderFunName ++ "({Tag, Instance, Value}) when is_integer(Tag), is_integer(Instance), is_binary(Value) ->\n    encode_v2_element(Tag, Instance, Value)",
    EncoderFns = lists:foldr(fun write_encoder/2, [CatchAnyEncoder], IEs),
    EncFuns = string:join(EncoderFns, ";\n\n"),

    ErlDecls = io_lib:format("~n~n~s~n~s~n~s~n~s.~n~n~s.~n",
			     [MsgDescription, MTypes, Enums, Funs,
			      EncFuns]),

    {ok, HrlF0} = file:read_file("include/gtp_packet.hrl"),
    [HrlHead, HrlV1, _] = binary:split(HrlF0, [?V1_TAG, ?V2_TAG], [global]),
    file:write_file("include/gtp_packet.hrl", [HrlHead, ?V1_TAG, HrlV1, ?V2_TAG, HrlRecs]),

    {ok, ErlF0} = file:read_file("src/gtp_packet.erl"),
    [ErlHead, ErlV1, _] = binary:split(ErlF0, [?V1_TAG, ?V2_TAG], [global]),
    file:write_file("src/gtp_packet.erl", [ErlHead, ?V1_TAG, ErlV1, ?V2_TAG, ErlDecls]).
