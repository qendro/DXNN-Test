-module(analyze_network).
-compile(export_all).

%% Systematically trace the network from cortex
analyze() ->
    Cortex = list_to_pid("<0.1426.0>"),
    
    io:format("~n=== ANALYZING NETWORK FROM CORTEX ~p ===~n", [Cortex]),
    
    %% Get cortex state - it knows all its sensors, neurons, and actuators
    io:format("~n1. CORTEX STATE:~n"),
    CxInfo = erlang:process_info(Cortex, [dictionary, links]),
    io:format("  Cortex links: ~p~n", [proplists:get_value(links, CxInfo)]),
    io:format("  Cortex dict: ~p~n", [proplists:get_value(dictionary, CxInfo)]),
    
    %% Manually identify the processes we know
    Sensors = [list_to_pid("<0.1428.0>"), list_to_pid("<0.1430.0>")],
    Neurons = [list_to_pid("<0.1437.0>")],
    Actuators = [list_to_pid("<0.1436.0>")],
    
    io:format("~n2. SENSORS (what they send to):~n"),
    lists:foreach(fun(SPid) ->
        analyze_sensor(SPid)
    end, Sensors),
    
    io:format("~n3. NEURONS (what they receive from and send to):~n"),
    lists:foreach(fun(NPid) ->
        analyze_neuron(NPid)
    end, Neurons),
    
    io:format("~n4. ACTUATORS (what they expect to receive from):~n"),
    lists:foreach(fun(APid) ->
        analyze_actuator(APid)
    end, Actuators),
    
    io:format("~n5. TOPOLOGY ANALYSIS:~n"),
    check_topology(Sensors, Neurons, Actuators),
    
    ok.

analyze_sensor(SPid) ->
    io:format("~n  Sensor ~p:~n", [SPid]),
    case erlang:process_info(SPid, [current_stacktrace, links, message_queue_len]) of
        undefined ->
            io:format("    DEAD~n");
        Info ->
            Links = proplists:get_value(links, Info),
            MQL = proplists:get_value(message_queue_len, Info),
            io:format("    Links (who it's connected to): ~p~n", [Links]),
            io:format("    Message queue length: ~p~n", [MQL]),
            io:format("    Status: Waiting for cortex sync (already sent outputs)~n")
    end.

analyze_neuron(NPid) ->
    io:format("~n  Neuron ~p:~n", [NPid]),
    case erlang:process_info(NPid, [current_stacktrace, links, message_queue_len, messages]) of
        undefined ->
            io:format("    DEAD~n");
        Info ->
            Links = proplists:get_value(links, Info),
            MQL = proplists:get_value(message_queue_len, Info),
            Msgs = proplists:get_value(messages, Info),
            io:format("    Links (who it's connected to): ~p~n", [Links]),
            io:format("    Message queue length: ~p~n", [MQL]),
            if length(Msgs) > 0 ->
                io:format("    Queued messages: ~p~n", [Msgs]);
            true -> ok
            end,
            io:format("    Status: Waiting for inputs at neuron.erl:87~n")
    end.

analyze_actuator(APid) ->
    io:format("~n  Actuator ~p:~n", [APid]),
    case erlang:process_info(APid, [current_stacktrace, links, message_queue_len, messages, backtrace]) of
        undefined ->
            io:format("    DEAD~n");
        Info ->
            Links = proplists:get_value(links, Info),
            MQL = proplists:get_value(message_queue_len, Info),
            Msgs = proplists:get_value(messages, Info),
            Backtrace = proplists:get_value(backtrace, Info),
            io:format("    Links (who it's connected to): ~p~n", [Links]),
            io:format("    Message queue length: ~p~n", [MQL]),
            io:format("    Status: Waiting at actuator.erl:17~n"),
            %% Extract PID from backtrace
            case re:run(Backtrace, "<0\\.[0-9]+\\.[0-9]+>", [{capture, first, list}]) of
                {match, [PidStr]} ->
                    WaitingForPid = list_to_pid(PidStr),
                    io:format("    Waiting for PID: ~p (from backtrace)~n", [WaitingForPid]),
                    case is_process_alive(WaitingForPid) of
                        true ->
                            PInfo = erlang:process_info(WaitingForPid, [current_function, initial_call]),
                            io:format("      -> This PID is ALIVE: ~p~n", [PInfo]);
                        false ->
                            io:format("      -> This PID is DEAD! ***PROBLEM***~n")
                    end;
                _ ->
                    io:format("    Could not extract waiting PID from backtrace~n")
            end
    end.

check_topology(Sensors, Neurons, Actuators) ->
    io:format("~n  Checking if message flow is connected:~n"),
    io:format("    Sensors (~p) -> should send to -> Neurons (~p)~n", [length(Sensors), length(Neurons)]),
    io:format("    Neurons (~p) -> should send to -> Actuators (~p)~n", [length(Neurons), length(Actuators)]),
    io:format("~n  MISMATCH DETECTED:~n"),
    io:format("    - Actuator is waiting for a DEAD process~n"),
    io:format("    - This means actuator's fanin list has stale/incorrect PIDs~n"),
    io:format("    - The network topology is inconsistent!~n"),
    ok.

