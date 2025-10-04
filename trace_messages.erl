%% Trace messages for debugging

-module(trace_messages).
-compile(export_all).

%% Check if sensors sent to the neuron
check_flow() ->
    Sensor1 = list_to_pid("<0.1428.0>"),
    Sensor2 = list_to_pid("<0.1430.0>"),
    Neuron = list_to_pid("<0.1437.0>"),
    Actuator = list_to_pid("<0.1436.0>"),
    Cortex = list_to_pid("<0.1426.0>"),
    
    io:format("~n=== Checking Message Flow ===~n"),
    
    %% Check sensor 1
    io:format("~nSensor1 ~p:~n", [Sensor1]),
    case erlang:process_info(Sensor1, [messages, current_stacktrace]) of
        undefined -> io:format("  DEAD~n");
        Info1 ->
            Msgs1 = proplists:get_value(messages, Info1),
            Stack1 = proplists:get_value(current_stacktrace, Info1),
            io:format("  Messages: ~p~n", [Msgs1]),
            io:format("  Stack: ~p~n", [lists:sublist(Stack1, 1)])
    end,
    
    %% Check sensor 2
    io:format("~nSensor2 ~p:~n", [Sensor2]),
    case erlang:process_info(Sensor2, [messages, current_stacktrace]) of
        undefined -> io:format("  DEAD~n");
        Info2 ->
            Msgs2 = proplists:get_value(messages, Info2),
            Stack2 = proplists:get_value(current_stacktrace, Info2),
            io:format("  Messages: ~p~n", [Msgs2]),
            io:format("  Stack: ~p~n", [lists:sublist(Stack2, 1)])
    end,
    
    %% Check neuron
    io:format("~nNeuron ~p:~n", [Neuron]),
    case erlang:process_info(Neuron, [messages, current_stacktrace]) of
        undefined -> io:format("  DEAD~n");
        InfoN ->
            MsgsN = proplists:get_value(messages, InfoN),
            StackN = proplists:get_value(current_stacktrace, InfoN),
            io:format("  Messages in queue: ~p~n", [length(MsgsN)]),
            if length(MsgsN) > 0 ->
                io:format("  First message: ~p~n", [hd(MsgsN)]);
            true -> ok
            end,
            io:format("  Stack: ~p~n", [lists:sublist(StackN, 1)])
    end,
    
    %% Check actuator
    io:format("~nActuator ~p:~n", [Actuator]),
    case erlang:process_info(Actuator, [messages, current_stacktrace]) of
        undefined -> io:format("  DEAD~n");
        InfoA ->
            MsgsA = proplists:get_value(messages, InfoA),
            StackA = proplists:get_value(current_stacktrace, InfoA),
            io:format("  Messages: ~p~n", [MsgsA]),
            io:format("  Stack: ~p~n", [lists:sublist(StackA, 1)])
    end,
    
    %% Check cortex
    io:format("~nCortex ~p:~n", [Cortex]),
    case erlang:process_info(Cortex, [messages]) of
        undefined -> io:format("  DEAD~n");
        InfoC ->
            MsgsC = proplists:get_value(messages, InfoC),
            io:format("  Messages: ~p~n", [MsgsC])
    end,
    
    ok.

