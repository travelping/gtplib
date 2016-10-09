#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

-mode(compile).

ies() ->
    [{1, "Cause", 1,
      [{"Value", 8, {enum, [{0, "Request IMSI"},
			    {1, "Request IMEI"},
			    {2, "Request IMSI and IMEI"},
			    {3, "No identity needed"},
			    {4, "MS Refuses"},
			    {5, "MS is not GPRS Responding"},
			    {6, "Reactivation Requested"},
			    {7, "PDP address inactivity timer expires"},
			    {8, "Network Failure"},
			    {9, "QoS parameter mismatch"},
			    {128, "Request accepted"},
			    {129, "New PDP type due to network preference"},
			    {130, "New PDP type due to single address bearer only"},
			    {192, "Non-existent"},
			    {193, "Invalid message format"},
			    {194, "IMSI/IMEI not known"},
			    {195, "MS is GPRS Detached"},
			    {196, "MS is not GPRS Responding"},
			    {197, "MS Refuses"},
			    {198, "Version not supported"},
			    {199, "No resources available"},
			    {200, "Service not supported"},
			    {201, "Mandatory IE incorrect"},
			    {202, "Mandatory IE missing"},
			    {203, "Optional IE incorrect"},
			    {204, "System failure"},
			    {205, "Roaming restriction"},
			    {206, "P-TMSI Signature mismatch"},
			    {207, "GPRS connection suspended"},
			    {208, "Authentication failure"},
			    {209, "User authentication failed"},
			    {210, "Context not found"},
			    {211, "All dynamic PDP addresses are occupied"},
			    {212, "No memory is available"},
			    {213, "Relocation failure"},
			    {214, "Unknown mandatory extension header"},
			    {215, "Semantic error in the TFT operation"},
			    {216, "Syntactic error in the TFT operation"},
			    {217, "Semantic errors in packet filter"},
			    {218, "Syntactic errors in packet filter"},
			    {219, "Missing or unknown APN"},
			    {220, "Unknown PDP address or PDP type"},
			    {221, "PDP context without TFT already activated"},
			    {222, "APN access denied - no subscription"},
			    {223, "APN Restriction type incompatibility with currently active PDP Contexts"},
			    {224, "MS MBMS Capabilities Insufficient"},
			    {225, "Invalid Correlation-ID"},
			    {226, "MBMS Bearer Context Superseded"},
			    {227, "Bearer Control Mode violation"},
			    {228, "Collision with network initiated request"},
			    {229, "APN Congestion"},
			    {230, "Bearer handling not supported"},
			    {231, "Target access restricted for the subscriber"}]}}
      ]},
     {2, "International Mobile Subscriber Identity", 8,
      [{"IMSI", 64, {type, tbcd}}]},
     {3, "Routeing Area Identity", 6, v1_rai},
     {4, "Temporary Logical Link Identity", 4,
      [{"TLLI", 4, bytes}]},
     {5, "Packet TMSI", 4,
      [{"P-TMSI", 4, bytes}]},
     {8, "Reordering Required", 1,
      [{'1', 7},
       {"required", 1, {enum, [no, yes]}}]},
     {9, "Authentication Triplet", 28,
      [{"RAND", 16, bytes},
       {"SRES", 4, bytes},
       {"Kc", 8, bytes}]},
     {11, "MAP Cause", 1,
      []},
     {12, "P-TMSI Signature", 3,
      []},
     {13, "MS Validated", 1,
      [{'1', 7},
       {"validated", 1, {enum, [no, yes]}}]},
     {14, "Recovery", 1,
      [{"Restart counter", 8, integer}]},
     {15, "Selection Mode", 1,
      [{'1', 6},
       {"Mode", 2, integer}]},
     {16, "Tunnel Endpoint Identifier Data I", 4,
      [{"TEI", 32, integer}]},
     {17, "Tunnel Endpoint Identifier Control Plane", 4,
      [{"TEI", 32, integer}]},
     {18, "Tunnel Endpoint Identifier Data II", 5,
      [{'_', 4},
       {"NSAPI", 4, integer},
       {"TEI", 32, integer}]},
     {19, "Teardown Ind", 1,
      [{'1', 7},
       {"Value", 1, integer}]},
     {20, "NSAPI", 1,
      [{'_', 4},
       {"NSAPI", 4, integer}]},
     {21, "RANAP Cause", 1,
      []},
     {22, "RAB Context", 9,
      []},
     {23, "Radio Priority SMS", 1,
      []},
     {24, "Radio Priority", 1,
      []},
     {25, "Packet Flow Id", 2,
      []},
     {26, "Charging Characteristics", 2,
      [{"Value", 2, bytes}]},                              %% TODO,The Charging Characteristics is defined
                                                            %% in 3GPP TS 32.251 [18] and 3GPP TS 32.298 [34]
     {27, "Trace Reference", 2,
      []},
     {28, "Trace Type", 2,
      []},
     {29, "MS Not Reachable Reason", 1,
      []},
     {127, "Charging ID", 4,
      [{id, 4, bytes}]},
     {128, "End User Address", '_',
      [{'1', 4},
       {"PDP Type Organization", 4, integer},
       {"PDP Type Number", 8, integer},
       {"PDP Address", 0, binary}]},
     {129, "MM Context GSM", '_',
      [{'1', 4},
       {"CKSN", 4, integer},
       {1, 2},
       {"No of Vectors", 3, integer},
       {"Used Cipher", 3, integer},
       {"Kc", 8, bytes},
       {"Tripple", 8, {array, "No of Vectors"}},
       {"DRX parameter", 2, bytes},
       {"MS Network Capability Length", 8, integer},
       {"MS Network Capability", 1, {array, "MS Network Capability Length"}},
       {"Container Length", 16, integer},
       {"Container", 1, {array, "Container Length"}},
       {'_', 0}
      ]},
     {129, "MM Context UMTS", '_',
      [{'1', 4},
       {"KSI", 4, integer},
       {2, 2},
       {"No of Vectors", 3, integer},
       {'1', 3},
       {"CK", 16, bytes},
       {"IK", 16, bytes},
       {"Quintuplet Length", 16, integer},
       {"Quintuplet", 1, {array, "Quintuplet Length"}},
       {"DRX parameter", 2, bytes},
       {"MS Network Capability Length", 8, integer},
       {"MS Network Capability", 1, {array, "MS Network Capability Length"}},
       {"Container Length", 16, integer},
       {"Container", 1, {array, "Container Length"}},
       {'_', 0}
      ]},
     {129, "MM Context GSM and UMTS", '_',
      [{'1', 4},
       {"CKSN", 4, integer},
       {3, 2},
       {"No of Vectors", 3, integer},
       {"Used Cipher", 3, integer},
       {"Kc", 8, bytes},
       {"Quintuplet Length", 16, integer},
       {"Quintuplet", 1, {array, "Quintuplet Length"}},
       {"DRX parameter", 2, bytes},
       {"MS Network Capability Length", 8, integer},
       {"MS Network Capability", 1, {array, "MS Network Capability Length"}},
       {"Container Length", 16, integer},
       {"Container", 1, {array, "Container Length"}},
       {'_', 0}
      ]},
     {129, "MM Context UMTS and Used Cipher", '_',
      [{'1', 4},
       {"KSI", 4, integer},
       {0, 2},
       {"No of Vectors", 3, integer},
       {"Used Cipher", 3, integer},
       {"CK", 16, bytes},
       {"IK", 16, bytes},
       {"Quintuplet Length", 16, integer},
       {"Quintuplet", 1, {array, "Quintuplet Length"}},
       {"DRX parameter", 2, bytes},
       {"MS Network Capability Length", 8, integer},
       {"MS Network Capability", 1, {array, "MS Network Capability Length"}},
       {"Container Length", 16, integer},
       {"Container", 1, {array, "Container Length"}},
       {'_', 0}
      ]},
     {130, "PDP Context", '_',
      []},
     {131, "Access Point Name", '_',
      [{"APN", 0, {type, apn}}]},
     {132, "Protocol Configuration Options", '_',
      %% TODO,  The content and the coding of the Protocol Configuration are
      %% defined in octet 3-z of the Protocol Configuration Options in
      %% subclause 10.5.6.3 of 3GPP TS 24.008 [5]. Please refer to subclause 10.5.6.3 of
      %% 3GPP TS 24.008 [5] for the maximum length of Protocol Configuration Options.
      [{"Config", protocol_config_opts}]},
     {133, "GSN Address", '_',
      [{"Address", 0, binary}]},
     {134, "MS International PSTN/ISDN Number", '_',
      [{"MSISDN", isdn_address_string}]},
     {135, "Quality of Service Profile", '_',
      [{"Priority", 8, integer},
       {"Data", 0, binary}]},
     {136, "Authentication Quintuplet", '_',
      []},
     {137, "Traffic Flow Template", '_',
      []},
     {138, "Target Identification", '_',
      []},
     {139, "UTRAN Transparent Container", '_',
      []},
     {140, "RAB Setup Information", '_',
      []},
     {141, "Extension Header Type List", '_',
      []},
     {142, "Trigger Id", '_',
      []},
     {143, "OMC Identity", '_',
      []},
     {144, "RAN Transparent Container", '_',
      []},
     {145, "PDP Context Prioritization", '_',
      []},
     {146, "Additional RAB Setup Information", '_',
      []},
     {147, "SGSN Number", '_',
      []},
     {148, "Common Flags", '_',
      [{"Dual Address Bearer Flag", 1, integer},
       {"Upgrade QoS Supported", 1, integer},
       {"NRSN", 1, integer},
       {"No QoS negotiation", 1, integer},
       {"MBMS Counting Information", 1, integer},
       {"RAN Procedures Ready", 1, integer},
       {"MBMS Service Type", 1, integer},
       {"Prohibit Payload Compression", 1, integer},
       {'_', 0}]},
     {149, "APN Restriction", '_',
      []},
     {150, "Radio Priority LCS", '_',
      []},
     {151, "RAT Type", '_',
      [{"RAT Type", 8, integer},
       {'_', 0}]},
     {152, "User Location Information", v1_uli},
     {153, "MS Time Zone", '_',
      [{"TimeZone", 8, integer},
       {'_', 6},
       {"DST", 2, integer},
       {'_', 0}]},
     {154, "IMEI", '_',
      [{"IMEI", 64, {type, tbcd}},
       {'_', 0}]},
     {155, "CAMEL Charging Information Container", '_',
      []},
     {156, "MBMS UE Context", '_',
      []},
     {157, "Temporary Mobile Group Identity", '_',
      []},
     {158, "RIM Routing Address", '_',
      []},
     {159, "MBMS Protocol Configuration Options", '_',
      []},
     {160, "MBMS Service Area", '_',
      []},
     {161, "Source RNC PDCP context info", '_',
      []},
     {162, "Additional Trace Info", '_',
      []},
     {163, "Hop Counter", '_',
      []},
     {164, "Selected PLMN ID", '_',
      []},
     {165, "MBMS Session Identifier", '_',
      []},
     {166, "MBMS 2G/3G Indicator", '_',
      []},
     {167, "Enhanced NSAPI", '_',
      []},
     {168, "MBMS Session Duration", '_',
      []},
     {169, "Additional MBMS Trace Info", '_',
      []},
     {170, "MBMS Session Repetition Number", '_',
      []},
     {171, "MBMS Time To Data Transfer", '_',
      []},
     {173, "BSS Container", '_',
      []},
     {174, "Cell Identification", '_',
      []},
     {175, "PDU Numbers", '_',
      []},
     {176, "BSSGP Cause", '_',
      []},
     {177, "Required MBMS bearer capabilities", '_',
      []},
     {178, "RIM Routing Address Discriminator", '_',
      []},
     {179, "List of set-up PFCs", '_',
      []},
     {180, "PS Handover XID Parameters", '_',
      []},
     {181, "MS Info Change Reporting Action", '_',
      []},
     {182, "Direct Tunnel Flags", '_',
      []},
     {183, "Correlation-ID", '_',
      []},
     {184, "Bearer Control Mode", '_',
      []},
     {185, "MBMS Flow Identifier", '_',
      []},
     {186, "MBMS IP Multicast Distribution", '_',
      []},
     {187, "MBMS Distribution Acknowledgement", '_',
      []},
     {188, "Reliable INTER RAT HANDOVER INFO", '_',
      []},
     {189, "RFSP Index", '_',
      []},
     {190, "Fully Qualified Domain Name", '_',
      []},
     {191, "Evolved Allocation/Retention Priority I", '_',
      []},
     {192, "Evolved Allocation/Retention Priority II", '_',
      []},
     {193, "Extended Common Flags", '_',
      []},
     {194, "User CSG Information", '_',
      []},
     {195, "CSG Information Reporting Action", '_',
      []},
     {196, "CSG ID", '_',
      []},
     {197, "CSG Membership Indication", '_',
      []},
     {198, "Aggregate Maximum Bit Rate", '_',
      []},
     {199, "UE Network Capability", '_',
      []},
     {200, "UE-AMBR", '_',
      []},
     {201, "APN-AMBR with NSAPI", '_',
      []},
     {202, "GGSN Back-Off Time", '_',
      []},
     {203, "Signalling Priority Indication", '_',
      []},
     {204, "Signalling Priority Indication with NSAPI", '_',
      []},
     {205, "Higher bitrates than 16 Mbps flag", '_',
      []},
     {207, "Additional MM context for SRVCC", '_',
      []},
     {208, "Additional flags for SRVCC", '_',
      []},
     {209, "STN-SR", '_',
      []},
     {210, "C-MSISDN", '_',
      []},
     {211, "Extended RANAP Cause", '_',
      []},
     {212, "eNodeB ID", '_',
      []},
     {213, "Selection Mode with NSAPI", '_',
      []},
     {214, "ULI Timestamp", '_',
      []},
     {215, "Local Home Network ID with NSAPI", '_',
      []},
     {216, "CN Operator Selection Entity", '_',
      []},
     {251, "Charging Gateway Address", '_',
      []},
     {255, "Private Extension", '_',
      []}
    ].

msgs() ->
    [{1, "Echo Request"},
     {2, "Echo Response"},
     {3, "Version Not Supported"},
     {4, "Node Alive Request"},
     {5, "Node Alive Response"},
     {6, "Redirection Request"},
     {7, "Redirection Response"},
     {16, "Create PDP Context Request"},
     {17, "Create PDP Context Response"},
     {18, "Update PDP Context Request"},
     {19, "Update PDP Context Response"},
     {20, "Delete PDP Context Request"},
     {21, "Delete PDP Context Response"},
     {22, "Initiate PDP Context Activation Request"},
     {23, "Initiate PDP Context Activation Response"},
     {26, "Error Indication"},
     {27, "PDU Notification Request"},
     {28, "PDU Notification Response"},
     {29, "PDU Notification Reject Request"},
     {30, "PDU Notification Reject Response"},
     {31, "Supported Extension Headers Notification"},
     {32, "Send Routeing Information for GPRS Request"},
     {33, "Send Routeing Information for GPRS Response"},
     {34, "Failure Report Request"},
     {35, "Failure Report Response"},
     {36, "Note MS GPRS Present Request"},
     {37, "Note MS GPRS Present Response"},
     {48, "Identification Request"},
     {49, "Identification Response"},
     {50, "SGSN Context Request"},
     {51, "SGSN Context Response"},
     {52, "SGSN Context Acknowledge"},
     {53, "Forward Relocation Request"},
     {54, "Forward Relocation Response"},
     {55, "Forward Relocation Complete"},
     {56, "Relocation Cancel Request"},
     {57, "Relocation Cancel Response"},
     {58, "Forward SRNS Context"},
     {59, "Forward Relocation Complete Acknowledge"},
     {60, "Forward SRNS Context Acknowledge"},
     {70, "RAN Information Relay"},
     {96, "MBMS Notification Request"},
     {97, "MBMS Notification Response"},
     {98, "MBMS Notification Reject Request"},
     {99, "MBMS Notification Reject Response"},
     {100, "Create MBMS Context Request"},
     {101, "Create MBMS Context Response"},
     {102, "Update MBMS Context Request"},
     {103, "Update MBMS Context Response"},
     {104, "Delete MBMS Context Request"},
     {105, "Delete MBMS Context Response"},
     {112, "MBMS Registration Request"},
     {113, "MBMS Registration Response"},
     {114, "MBMS De-Registration Request"},
     {115, "MBMS De-Registration Response"},
     {116, "MBMS Session Start Request"},
     {117, "MBMS Session Start Response"},
     {118, "MBMS Session Stop Request"},
     {119, "MBMS Session Stop Response"},
     {120, "MBMS Session Update Request"},
     {121, "MBMS Session Update Response"},
     {128, "MS Info Change Notification Request"},
     {129, "MS Info Change Notification Response"},
     {240, "Data Record Transfer Request"},
     {241, "Data Record Transfer Response"},
     {254, "End Marker"},
     {255, "G-PDU"}
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
gen_decoder_header_match({'1', Size}) ->
    [io_lib:format("~w:~w", [(1 bsl Size) - 1, Size])];
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
    [io_lib:format("~s = enum_~s(M_~s)", [s2a(Name), s2a(Name), s2a(Name)])];
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
gen_encoder_bin({'1', Size}) ->
    [io_lib:format("~w:~w", [(1 bsl Size) - 1, Size])];
gen_encoder_bin({Value, Size}) when is_integer(Value); is_atom(Value) ->
    [io_lib:format("~w:~w", [Value, Size])];
gen_encoder_bin({Name, {flags, Flags}}) ->
    [io_lib:format("(encode_v1_flag('~s', M_~s)):1", [Flag, s2a(Name)]) || Flag <- Flags];
gen_encoder_bin({Name, Size, {enum, _Enum}}) ->
    [io_lib:format("(enum_~s(M_~s)):~w/integer", [s2a(Name), s2a(Name), Size])];
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
    Fwd = io_lib:format("enum_~s(~s) -> ~w", [s2a(Name), s2a(Value), Cnt]),
    Rev = io_lib:format("enum_~s(~w) -> ~s", [s2a(Name), Cnt, s2a(Value)]),
    gen_enum(Name, Next, Cnt + 1, {[Fwd|FwdFuns], [Rev|RevFuns]}).

gen_enum(_, [], _, {FwdFuns, RevFuns}) ->
    {lists:reverse(FwdFuns), lists:reverse(RevFuns)};
gen_enum(Name, [{Cnt, Value}|Rest], _, Acc) ->
    gen_enum(Name, Value, Cnt, Rest, Acc);
gen_enum(Name, [Value|Rest], Cnt, Acc) ->
    gen_enum(Name, Value, Cnt, Rest, Acc).

gen_message_type(Value, Name, Next, {FwdFuns, RevFuns}) ->
    Fwd = io_lib:format("message_type_v1(~s) -> ~w", [s2a(Name), Value]),
    Rev = io_lib:format("message_type_v1(~w) -> ~s", [Value, s2a(Name)]),
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
    S = string:join(FwdFuns ++ RevFuns, ";\n") ++ ".\n",
    lists:keystore(Name, 1, Acc, {Name, S});
collect_enum(_, Acc) ->
    Acc.

collect_enums({_, _, _, Fields}, AccIn)
  when is_list(Fields) ->
    lists:foldr(fun(X, Acc) -> collect_enum(X, Acc) end, AccIn, Fields);
collect_enums(_, AccIn) ->
    AccIn.

write_enums(IEs) ->
    E = lists:foldr(fun(X, Acc) -> collect_enums(X, Acc) end, [], IEs),
    {_, Str} = lists:unzip(E),
    string:join(Str, "\n").

write_record({_Id, Name, _Length, Fields})
  when is_list(Fields) ->
    Indent = "        ",
    RecordDef = string:join(collect(fun gen_record_def/1, [{"Instance", 0, integer} | Fields], []), [",\n", Indent]),
    io_lib:format("-record(~s, {~n~s~s~n}).~n", [s2a(Name), Indent, RecordDef]);
write_record(_) ->
    [].

write_decoder(FunName, {Id, Name, _Length, Fields})
  when is_list(Fields) ->
    FunHead = io_lib:format("~s(~w, Instance, ", [FunName, Id]),
    MatchIdent = indent(FunHead, 2),
    Match = string:join(collect(fun(X) -> gen_decoder_header_match(X) end, Fields), [",\n", MatchIdent]),
    Body = build_late_assign(Fields),
    RecIdent = indent(Name, 6),
    RecAssign = string:join(["instance = Instance" |
			     collect(fun gen_decoder_record_assign/1, Fields)], [",\n", RecIdent]),
    io_lib:format("~s<<~s>>) ->~n~s    #~s{~s}", [FunHead, Match, Body, s2a(Name), RecAssign]);
write_decoder(FunName, {Id, _Name, Helper})
  when is_atom(Helper) ->
    io_lib:format("~s(~w, Instance, Data) ->~n    decode_~s(Instance, Data)",
		  [FunName, Id, Helper]);
write_decoder(FunName, {Id, _Name, Length, Helper})
  when is_integer(Length), is_atom(Helper) ->
    io_lib:format("~s(~w, Instance, Data) ->~n    decode_~s(Instance, Data)",
		  [FunName, Id, Helper]).

write_encoder(FunName, {Id, Name, _Length, Fields})
  when is_list(Fields) ->
    RecIdent = indent("encode_v1_element(#", 4),
    RecAssign = string:join(["instance = Instance" |
			     collect(fun gen_encoder_record_assign/1, Fields)], [",\n", RecIdent]),
    FunHead = io_lib:format("encode_v1_element(#~s{~n~s~s}) ->~n", [s2a(Name), RecIdent, RecAssign]),
    DecHead = io_lib:format("    ~s(~w, Instance, ", [FunName, Id]),
    BinIndent = indent(DecHead, 2),
    BinAssign = string:join(collect(fun(X) -> gen_encoder_bin(X) end, Fields), [",\n", BinIndent]),
    io_lib:format("~s~s<<~s>>)", [FunHead, DecHead, BinAssign]);
write_encoder(FunName, {Id, Name, Helper})
  when is_atom(Helper) ->
    io_lib:format("encode_v1_element(#~s{instance = Instance} = IE) ->~n    ~s(~w, Instance, encode_~s(IE))",
		  [s2a(Name), FunName, Id, Helper]);
write_encoder(FunName, {Id, Name, Length, Helper})
  when is_integer(Length), is_atom(Helper) ->
    io_lib:format("encode_v1_element(#~s{instance = Instance} = IE) ->~n    ~s(~w, Instance, encode_~s(IE))",
		  [s2a(Name), FunName, Id, Helper]).

main(_) ->
    MsgDescription = string:join([io_lib:format("msg_description(~s) -> <<\"~s\">>", [s2a(X), X]) || {_, X} <- msgs()]
				 ++ ["msg_description(X) -> io_lib:format(\"~p\", [X])"], ";\n") ++ ".\n",

    {FwdFuns, RevFuns} = gen_message_type(msgs(), {[], []}),
    WildFun = ["message_type_v1({Vendor, Type}) when is_integer(Vendor), is_integer(Type) -> {Vendor, Type}"],
    MTypes = string:join(FwdFuns ++ RevFuns ++ WildFun, ";\n") ++ ".\n",

    Records = string:join([write_record(X) || X <- ies()], "\n"),
    HrlRecs = io_lib:format("%% This file is auto-generated. DO NOT EDIT~n~n~s~n", [Records]),
    Enums = write_enums(ies()),

    CatchAnyDecoder = "decode_v1_element(Tag, Instance, Value) ->\n        {Tag, Instance, Value}",

    Funs = string:join([write_decoder("decode_v1_element", X) || X <- ies()] ++ [CatchAnyDecoder], ";\n\n"),

    MainDecodeSwitch = ["decode_v1(<<>>, _PrevId, _PrevInst, IEs) ->\n    lists:reverse(IEs);\n",
	[io_lib:format("decode_v1(<<~w, Data:~w/bytes, Next/binary>>, PrevInst, PrevId, IEs) ->~n"
		       "    Instance = v1_instance(~w, PrevId, PrevInst),~n"
		       "    IE = decode_v1_element(~w, Instance, Data),~n"
		       "    decode_v1(Next, ~w, Instance, [IE|IEs]);~n", [Id, Length, Id, Id, Id]) || {Id, _, Length, _} <- ies(), Id < 128],
	"decode_v1(<<Id, Length:16/integer, Rest/binary>>, PrevId, PrevInst, IEs) when Id > 127 ->\n"
	"    <<Data:Length/binary, Next/binary>> = Rest,\n"
	"    Instance = v1_instance(Id, PrevId, PrevInst),\n"
	"    IE = decode_v1_element(Id, Instance, Data),\n"
	"    decode_v1(Next, Id, Instance, [IE|IEs]);\n"
	"decode_v1(<<Id, Rest/binary>>, PrevId, PrevInst, IEs) ->\n"
	"    Instance = v1_instance(Id, PrevId, PrevInst),\n"
	"    IE = {Id, Instance, Rest},\n"
	"    decode_v1(<<>>, Id, Instance, [IE|IEs]).\n"],

    CatchAnyEncoder = "encode_v1_element({Tag, Instance, Value}) when is_integer(Tag), is_integer(Instance), is_binary(Value) ->\n    encode_v1_element(Tag, Instance, Value)",
    EncFuns = string:join([write_encoder("encode_v1_element", X) || X <- ies()]
			  ++ [CatchAnyEncoder] , ";\n\n"),

    ErlDecls = io_lib:format("%% This file is auto-generated. DO NOT EDIT~n~n~s~n~s~n~s~n~s.~n~n~s~n~n~s.~n",
			     [MsgDescription, MTypes, Enums, Funs, MainDecodeSwitch, EncFuns]),
    file:write_file("include/gtp_packet_v1_gen.hrl", HrlRecs),
    file:write_file("src/gtp_packet_v1_gen.hrl", ErlDecls).
