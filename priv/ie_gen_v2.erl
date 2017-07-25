#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

-mode(compile).

ies() ->
    [
     {1, "v2 International Mobile Subscriber Identity",
      [{"IMSI", 0, {type, tbcd}}]},
     {2, "v2 Cause",
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
			       {120, "GTP-C Entity Congestion"}]}},
       {'_', 5},
       {"PCE", 1, integer},
       {"BCE", 1, integer},
       {"CS", 1, integer},
       {'_', 0}]},
     {3, "v2 Recovery",
      [{"Restart counter", 8, integer},
       {'_', 0}]},
     {51, "v2 STN-SR", []},
     {71, "v2 Access Point Name",
      [{"APN", 0, {type, apn}}]},
     {72, "v2 Aggregate Maximum Bit Rate",
      [{"Uplink", 32, integer},
       {"Downlink", 32, integer},
       {'_', 0}]},
     {73, "v2 EPS Bearer ID",
      [{'_', 4},
       {"EPS Bearer ID", 4, integer},
       {'_', 0}]},
     {74, "v2 IP Address",
      [{"IP", 0, binary}]},
     {75, "v2 Mobile Equipment Identity",
      [{"MEI", 0, binary}]},
     {76, "v2 MSISDN",
      [{"MSISDN", 0, {type, tbcd}}]},
     {77, "v2 Indication",
      [{"Flags", 0, {type, v2_indication_flags}}]},
     {78, "v2 Protocol Configuration Options",
      [{"Config", protocol_config_opts}]},
     {79, "v2 PDN Address Allocation",
      [{'_', 5},
       {"Type", 3, {enum, [{1, "IPv4"},
			   {2, "IPv6"},
			   {3, "IPv4v6"},
			   {4, "Non-IP"}]}},
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
     {81, "v2 Flow Quality of Service", []},
     {82, "v2 RAT Type",
      [{"RAT Type", 8, integer},
       {'_', 0}]},
     {83, "v2 Serving Network", v2_mccmnc},
     {84, "v2 EPS Bearer Level Traffic Flow Template", []},
     {85, "v2 Traffic Aggregation Description", []},
     {86, "v2 User Location Information", v2_user_location_information},
     {87, "v2 Fully Qualified Tunnel Endpoint Identifier", v2_fully_qualified_tunnel_endpoint_identifier},
     {88, "v2 TMSI", []},
     {89, "v2 Global CN-Id", []},
     {90, "v2 S103 PDN Data Forwarding Info", []},
     {91, "v2 S1-U Data Forwarding Info", []},
     {92, "v2 Delay Value", []},
     {93, "v2 Bearer Context",
      [{"Group", 0, {type, v2_grouped}}]},
     {94, "v2 Charging ID",
      [{id, 4, bytes}]},
     {95, "v2 Charging Characteristics", []},
     {96, "v2 Trace Information", []},
     {97, "v2 Bearer Flags", []},
     {99, "v2 PDN Type",
      [{'_', 4},
       {"PDN Type", 4, {enum, [{1, "IPv4"},
			       {2, "IPv6"},
			       {3, "IPv4v6"},
			       {4, "Non-IP"}]}},
       {'_', 0}]},
     {100, "v2 Procedure Transaction ID", []},
     {103, "v2 MM Context 1", []},
     {104, "v2 MM Context 2", []},
     {105, "v2 MM Context 3", []},
     {106, "v2 MM Context 4", []},
     {107, "v2 MM Context 5", []},
     {108, "v2 MM Context 6", []},
     {109, "v2 PDN Connection", []},
     {110, "v2 PDU Numbers", []},
     {111, "v2 P-TMSI", []},
     {112, "v2 P-TMSI Signature", []},
     {113, "v2 Hop Counter", []},
     {114, "v2 UE Time Zone",
      [{"TimeZone", 8, integer},
       {'_', 6},
       {"DST", 2, integer},
       {'_', 0}]},
     {115, "v2 Trace Reference", []},
     {116, "v2 Complete Request Message", []},
     {117, "v2 GUTI", []},
     {118, "v2 F-Container", []},
     {119, "v2 F-Cause", []},
     {120, "v2 PLMN ID", []},
     {121, "v2 Target Identification", []},
     {123, "v2 Packet Flow ID ", []},
     {124, "v2 RAB Context ", []},
     {125, "v2 Source RNC PDCP Context Info", []},
     {126, "v2 UDP Source Port Number", []},
     {127, "v2 APN Restriction",
      [{"Restriction Type Value", 8, integer},
       {'_', 0}]},
     {128, "v2 Selection Mode",
      [{'_', 6},
       {"Mode", 2, integer},
       {'_', 0}]},
     {129, "v2 Source Identification", []},
     {131, "v2 Change Reporting Action", []},
     {132, "v2 Fully Qualified PDN Connection Set Identifier", []},
     {133, "v2 Channel needed", []},
     {134, "v2 eMLPP Priority", []},
     {135, "v2 Node Type", []},
     {136, "v2 Fully Qualified Domain Name", []},
     {137, "v2 Transaction Identifier", []},
     {138, "v2 MBMS Session Duration", []},
     {139, "v2 MBMS Service Area", []},
     {140, "v2 MBMS Session Identifier", []},
     {141, "v2 MBMS Flow Identifier", []},
     {142, "v2 MBMS IP Multicast Distribution", []},
     {143, "v2 MBMS Distribution Acknowledge", []},
     {144, "v2 RFSP Index", []},
     {145, "v2 User CSG Information", []},
     {146, "v2 CSG Information Reporting Action", []},
     {147, "v2 CSG ID", []},
     {148, "v2 CSG Membership Indication", []},
     {149, "v2 Service indicator", []},
     {150, "v2 Detach Type", []},
     {151, "v2 Local Distiguished Name", []},
     {152, "v2 Node Features", []},
     {153, "v2 MBMS Time to Data Transfer", []},
     {154, "v2 Throttling", []},
     {155, "v2 Allocation/Retention Priority", []},
     {156, "v2 EPC Timer", []},
     {157, "v2 Signalling Priority Indication", []},
     {158, "v2 Temporary Mobile Group Identity", []},
     {159, "v2 Additional MM context for SRVCC", []},
     {160, "v2 Additional flags for SRVCC", []},
     {162, "v2 MDT Configuration", []},
     {163, "v2 Additional Protocol Configuration Options", []},
     {164, "v2 Absolute Time of MBMS Data Transfer", []},
     {165, "v2 HeNB Information Reporting ", []},
     {166, "v2 IPv4 Configuration Parameters", []},
     {167, "v2 Change to Report Flags ", []},
     {168, "v2 Action Indication", []},
     {169, "v2 TWAN Identifier", []},
     {170, "v2 ULI Timestamp", []},
     {171, "v2 MBMS Flags", []},
     {172, "v2 RAN/NAS Cause", []},
     {173, "v2 CN Operator Selection Entity", []},
     {174, "v2 Trusted WLAN Mode Indication", []},
     {175, "v2 Node Number", []},
     {176, "v2 Node Identifier", []},
     {177, "v2 Presence Reporting Area Action", []},
     {178, "v2 Presence Reporting Area Information", []},
     {179, "v2 TWAN Identifier Timestamp", []},
     {180, "v2 Overload Control Information", []},
     {181, "v2 Load Control Information", []},
     {182, "v2 Metric", []},
     {183, "v2 Sequence Number", []},
     {184, "v2 APN and Relative Capacity", []},
     {185, "v2 WLAN Offloadability Indication", []},
     {255, "v2 Private Extension", []}].

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

gen_record_def({Value, _}) when is_integer(Value); is_atom(Value) ->
    [];
gen_record_def({Name, {flags, _}}) ->
    [io_lib:format("~s = []", [s2a(Name)])];
gen_record_def({Name, _, {enum, [{_,H}|_]}}) ->
    [io_lib:format("~s = ~w", [s2a(Name), H])];
gen_record_def({Name, _, {enum, [H|_]}}) ->
    [io_lib:format("~s = ~w", [s2a(Name), H])];
gen_record_def({Name, _, integer}) ->
    [io_lib:format("~s = 0", [s2a(Name)])];
gen_record_def({Name, Size, bits}) ->
    [io_lib:format("~s = ~w", [s2a(Name), <<0:Size>>])];
gen_record_def({Name, Size, bytes}) ->
    [io_lib:format("~s = ~w", [s2a(Name), <<0:(Size * 8)>>])];
gen_record_def({Name, _, binary}) ->
    [io_lib:format("~s = <<>>", [s2a(Name)])];
gen_record_def({Name, _, length_binary}) ->
    [io_lib:format("~s = <<>>", [s2a(Name)])];
gen_record_def({Name, _, {array, _}}) ->
    [io_lib:format("~s = []", [s2a(Name)])];
gen_record_def(Tuple) ->
    Name = element(1, Tuple),
    [s2a(Name)].

gen_decoder_header_match({'_', 0}) ->
    ["_/binary"];
gen_decoder_header_match({'_', Size}) ->
    [io_lib:format("_:~w", [Size])];
gen_decoder_header_match({Value, Size}) when is_integer(Value); is_atom(Value) ->
    [io_lib:format("~w:~w", [Value, Size])];
gen_decoder_header_match({Name, {flags, Flags}}) ->
    [io_lib:format("M_~s_~s:1", [s2a(Name), s2a(Flag)]) || Flag <- Flags];
gen_decoder_header_match({Name, Size, {enum, _Enum}}) ->
    [io_lib:format("M_~s:~w/integer", [s2a(Name), Size])];
gen_decoder_header_match({Name, _Fun}) ->
    [io_lib:format("M_~s/binary", [s2a(Name)])];
gen_decoder_header_match({Name, _Len, {array, Multi}}) when is_list(Multi) ->
    {stop, [io_lib:format("M_~s_Rest/binary", [s2a(Name)])]};
gen_decoder_header_match({Name, Len, {array, _Multi}}) ->
    {stop, [io_lib:format("M_~s_len:~w/integer, M_~s_Rest/binary", [s2a(Name), Len, s2a(Name)])]};
gen_decoder_header_match({Name, Len, length_binary}) ->
    [io_lib:format("M_~s_len:~w/integer, M_~s:M_~s_len/bytes", [s2a(Name), Len, s2a(Name), s2a(Name)])];
gen_decoder_header_match({Name, 0, {type, _TypeName}}) ->
    [io_lib:format("M_~s/binary", [s2a(Name)])];
gen_decoder_header_match({Name, 0, Type}) ->
    [io_lib:format("M_~s/~w", [s2a(Name), Type])];
gen_decoder_header_match({Name, Size, {type, _TypeName}}) ->
    [io_lib:format("M_~s:~w/bits", [s2a(Name), Size])];
gen_decoder_header_match({Name, Size, Type}) ->
    [io_lib:format("M_~s:~w/~s", [s2a(Name), Size, Type])].

gen_decoder_record_assign({Value, _}) when is_integer(Value); is_atom(Value) ->
    [];
gen_decoder_record_assign({Name, {flags, Flags}}) ->
    F = [io_lib:format("[ '~s' || M_~s_~s =/= 0 ]", [X, s2a(Name), s2a(X)]) || X <- Flags],
    [io_lib:format("~s = ~s", [s2a(Name), string:join(F, " ++ ")])];
gen_decoder_record_assign({Name, _Size, {enum, _Enum}}) ->
    [io_lib:format("~s = enum_v2_~s(M_~s)", [s2a(Name), s2a(Name), s2a(Name)])];
gen_decoder_record_assign({Name, Fun}) ->
    [io_lib:format("~s = decode_~s(M_~s)", [s2a(Name), Fun, s2a(Name)])];
gen_decoder_record_assign({Name, Size, {array, Multi}}) when is_list(Multi) ->
    [io_lib:format("~s = [X || <<X:~w/bytes>> <= M_~s]", [s2a(Name), Size, s2a(Name)])];
gen_decoder_record_assign({Name, _Size, {array, Multi}}) ->
    [io_lib:format("~s = [X || <<X:~w/bytes>> <= M_~s]", [s2a(Name), Multi, s2a(Name)])];
gen_decoder_record_assign({Name, _Size, {type, TypeName}}) ->
    [io_lib:format("~s = decode_~s(M_~s)", [s2a(Name), TypeName, s2a(Name)])];
gen_decoder_record_assign({Name, _Size, _Type}) ->
    [io_lib:format("~s = M_~s", [s2a(Name), s2a(Name)])].

gen_encoder_record_assign({Value, _}) when is_integer(Value); is_atom(Value) ->
    [];
gen_encoder_record_assign(Tuple) ->
    Name = element(1, Tuple),
    [io_lib:format("~s = M_~s", [s2a(Name), s2a(Name)])].

gen_encoder_bin({'_', 0}) ->
    [];
gen_encoder_bin({'_', Size}) ->
    [io_lib:format("0:~w", [Size])];
gen_encoder_bin({Value, Size}) when is_integer(Value); is_atom(Value) ->
    [io_lib:format("~w:~w", [Value, Size])];
gen_encoder_bin({Name, {flags, Flags}}) ->
    [io_lib:format("(encode_v2_flag('~s', M_~s)):1", [Flag, s2a(Name)]) || Flag <- Flags];
gen_encoder_bin({Name, Size, {enum, _Enum}}) ->
    [io_lib:format("(enum_v2_~s(M_~s)):~w/integer", [s2a(Name), s2a(Name), Size])];
gen_encoder_bin({Name, Fun}) ->
    [io_lib:format("(encode_~s(M_~s))/binary", [Fun, s2a(Name)])];
gen_encoder_bin({Name, Len, {array, _Multi}}) ->
    [io_lib:format("(length(M_~s)):~w/integer, (<< <<X/binary>> || X <- M_~s>>)/binary", [s2a(Name), Len, s2a(Name)])];
gen_encoder_bin({Name, 0, {type, TypeName}}) ->
    [io_lib:format("(encode_~s(M_~s))/binary", [TypeName, s2a(Name)])];
gen_encoder_bin({Name, Size, {type, TypeName}}) ->
    [io_lib:format("(encode_~s(M_~s)):~w/bits", [TypeName, s2a(Name), Size])];
gen_encoder_bin({Name, Len, length_binary}) ->
    [io_lib:format("(byte_size(M_~s)):~w/integer, M_~s/binary", [s2a(Name), Len, s2a(Name)])];
gen_encoder_bin({Name, 0, Type}) ->
    [io_lib:format("M_~s/~w", [s2a(Name), Type])];
gen_encoder_bin({Name, Size, bytes}) ->
    [io_lib:format("M_~s:~w/bytes", [s2a(Name), Size])];
gen_encoder_bin({Name, Size, bits}) ->
    [io_lib:format("M_~s:~w/bits", [s2a(Name), Size])];
gen_encoder_bin({Name, Size, _Type}) ->
    [io_lib:format("M_~s:~w", [s2a(Name), Size])].

indent(Atom, Extra) when is_atom(Atom) ->
    indent(atom_to_list(Atom), Extra);
indent(List, Extra) ->
    Indent = length(lists:flatten(List)) + Extra,
    lists:duplicate(Indent, " ").

s2a(Name) when is_atom(Name) ->
    Name;
s2a(Name) ->
    lists:map(fun(32) -> $_;
		 ($/) -> $_;
		 ($-) -> $_;
		 ($.) -> $_;
		 ($,) -> $_;
		 (C)  -> C
	      end,
	      string:to_lower(Name)).

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
    Fwd = io_lib:format("enum_v2_~s(~s) -> ~w", [s2a(Name), s2a(Value), Cnt]),
    Rev = io_lib:format("enum_v2_~s(~w) -> ~s", [s2a(Name), Cnt, s2a(Value)]),
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

build_late_assign([]) ->
    [];
build_late_assign([H = {_Name, _Len, {array, _Multi}} | T]) ->
    build_late_assign(H, T);
build_late_assign([_ | T]) ->
    build_late_assign(T).

build_late_assign({Name, Len, {array, Multi}}, T)
  when is_list(Multi) ->
    Init = io_lib:format("M_~s_size = M_~s * ~w", [s2a(Name), s2a(Multi), Len]),
    build_late_assign(Name, Init, T);
build_late_assign({Name, _Len, {array, Multi}}, T) ->
    Init = io_lib:format("M_~s_size = M_~s_len * ~w", [s2a(Name), s2a(Name), Multi]),
    build_late_assign(Name, Init, T).

build_late_assign(Name, Init, Fields) ->
    Match = io_lib:format("M_~s:M_~s_size/bytes", [s2a(Name), s2a(Name)]),
    {Body, Next} = collect_late_assign(Fields, [Match]),
    M = io_lib:format("    <<~s>> = M_~s_Rest,", [string:join(Body, ",\n      "), s2a(Name)]),
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


collect_enum({Name, _, {enum, Enum}}, Acc) ->
    {FwdFuns, RevFuns} = gen_enum(Name, Enum, 0, {[], []}),
    Wildcard = io_lib:format("enum_v2_~s(X) when is_integer(X) -> X", [s2a(Name)]),
    S = string:join(FwdFuns ++ RevFuns ++ [Wildcard], ";\n") ++ ".\n",
    lists:keystore(Name, 1, Acc, {Name, S});
collect_enum(_, Acc) ->
    Acc.

collect_enums({_, _, Fields}, AccIn)
  when is_list(Fields) ->
    lists:foldr(fun(X, Acc) -> collect_enum(X, Acc) end, AccIn, Fields);
collect_enums(_, AccIn) ->
    AccIn.

write_enums(IEs) ->
    E = lists:foldr(fun(X, Acc) -> collect_enums(X, Acc) end, [], IEs),
    {_, Str} = lists:unzip(E),
    string:join(Str, "\n").

write_record({_Id, Name, Fields})
  when is_list(Fields) ->
    Indent = "        ",
    RecordDef = string:join(collect(fun gen_record_def/1, [{"Instance", 0, integer} | Fields], []), [",\n", Indent]),
    io_lib:format("-record(~s, {~n~s~s~n}).~n", [s2a(Name), Indent, RecordDef]);
write_record(_) ->
    [].

write_decoder(FunName, {Id, Name, Fields})
  when is_list(Fields) ->
    FunHead = io_lib:format("~s(~w, Instance, ", [FunName, Id]),
    MatchIdent = indent(FunHead, 2),
    Match = string:join(collect(fun gen_decoder_header_match/1, Fields), [",\n", MatchIdent]),
    Body = build_late_assign(Fields),
    RecIdent = indent(Name, 6),
    RecAssign = string:join(["instance = Instance" |
			     collect(fun gen_decoder_record_assign/1, Fields)], [",\n", RecIdent]),
    io_lib:format("~s<<~s>>) ->~n~s    #~s{~s}", [FunHead, Match, Body, s2a(Name), RecAssign]);
write_decoder(FunName, {Id, _Name, Helper})
  when is_atom(Helper) ->
    io_lib:format("~s(~w, Instance, Data) ->~n    decode_~s(Instance, Data)",
		  [FunName, Id, Helper]).

write_encoder(FunName, {Id, Name, Fields})
  when is_list(Fields) ->
    RecIdent = indent("encode_v2_element(#", 4),
    RecAssign = string:join(["instance = Instance" |
			     collect(fun gen_encoder_record_assign/1, Fields)], [",\n", RecIdent]),
    FunHead = io_lib:format("encode_v2_element(#~s{~n~s~s}) ->~n", [s2a(Name), RecIdent, RecAssign]),
    DecHead = io_lib:format("    ~s(~w, Instance, ", [FunName, Id]),
    BinIndent = indent(DecHead, 2),
    BinAssign = string:join(collect(fun gen_encoder_bin/1, Fields), [",\n", BinIndent]),
    io_lib:format("~s~s<<~s>>)", [FunHead, DecHead, BinAssign]);
write_encoder(FunName, {Id, Name, Helper})
  when is_atom(Helper) ->
    io_lib:format("encode_v2_element(#~s{instance = Instance} = IE) ->~n    ~s(~w, Instance, encode_~s(IE))",
		  [s2a(Name), FunName, Id, Helper]).

main(_) ->
    MsgDescription = string:join([io_lib:format("msg_description_v2(~s) -> <<\"~s\">>", [s2a(X), X]) || {_, X} <- msgs()]
				 ++ ["msg_description_v2(X) -> io_lib:format(\"~p\", [X])"], ";\n") ++ ".\n",

    {FwdFuns, RevFuns} = gen_message_type(msgs(), {[], []}),
    ErrorFun = ["message_type_v2(Type) -> error(badarg, [Type])"],
    MTypes = string:join(FwdFuns ++ RevFuns ++ ErrorFun, ";\n") ++ ".\n",

    Records = string:join([write_record(X) || X <- ies()], "\n"),
    HrlRecs = io_lib:format("%% This file is auto-generated. DO NOT EDIT~n~n~s~n", [Records]),
    Enums = write_enums(ies()),

    CatchAnyDecoder = "decode_v2_element(Tag, Instance, Value) ->\n        {Tag, Instance, Value}",

    Funs = string:join([write_decoder("decode_v2_element", X) || X <- ies()] ++ [CatchAnyDecoder], ";\n\n"),


    CatchAnyEncoder = "encode_v2_element({Tag, Instance, Value}) when is_integer(Tag), is_integer(Instance), is_binary(Value) ->\n    encode_v2_element(Tag, Instance, Value)",
    EncFuns = string:join([write_encoder("encode_v2_element", X) || X <- ies()]
			  ++ [CatchAnyEncoder] , ";\n\n"),

    ErlDecls = io_lib:format("%% This file is auto-generated. DO NOT EDIT~n~n~s~n~s~n~s~n~s.~n~n~s.~n",
			     [MsgDescription, MTypes, Enums, Funs, EncFuns]),
    file:write_file("include/gtp_packet_v2_gen.hrl", HrlRecs),
    file:write_file("src/gtp_packet_v2_gen.hrl", ErlDecls).
