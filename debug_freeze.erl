-module(debug_freeze).
-compile(export_all).

%% Call this when the system appears frozen
%% It will show you what all processes are doing
check_freeze() ->
    io:format("~n=== CHECKING FOR FROZEN PROCESSES ===~n"),
    io:format("Total processes: ~p~n", [length(erlang:processes())]),
    
    %% Find processes by registered name
    PM_Pid = whereis(population_monitor),
    Benchmarker_Pid = whereis(benchmarker),
    
    io:format("~nPopulation Monitor: ~p~n", [PM_Pid]),
    if PM_Pid =/= undefined -> show_process_info(PM_Pid, "Population Monitor");
       true -> ok
    end,
    
    io:format("~nBenchmarker: ~p~n", [Benchmarker_Pid]),
    if Benchmarker_Pid =/= undefined -> show_process_info(Benchmarker_Pid, "Benchmarker");
       true -> ok
    end,
    
    %% Look for agent/cortex/sensor processes by scanning all processes
    io:format("~n=== Scanning for stuck sensors ===~n"),
    find_stuck_sensors(),
    
    io:format("~n=== Scanning for cortex processes ===~n"),
    find_cortex_processes(),
    
    io:format("~n=== Scanning for actuator processes ===~n"),
    find_actuator_processes(),
    
    io:format("~n=== Scanning for scape processes ===~n"),
    find_scape_processes(),
    
    io:format("~n=== Scanning for neuron processes ===~n"),
    find_neuron_processes(),
    
    io:format("~n=== Done ===~n"),
    ok.

show_process_info(Pid, Name) ->
    case erlang:process_info(Pid, [status, message_queue_len, current_function, dictionary]) of
        undefined ->
            io:format("~s: Process is dead~n", [Name]);
        Info ->
            Status = proplists:get_value(status, Info),
            MsgQLen = proplists:get_value(message_queue_len, Info),
            CurrentFun = proplists:get_value(current_function, Info),
            Dict = proplists:get_value(dictionary, Info),
            
            io:format("~s (~p):~n", [Name, Pid]),
            io:format("  Status: ~p~n", [Status]),
            io:format("  Message Queue: ~p messages~n", [MsgQLen]),
            io:format("  Current Function: ~p~n", [CurrentFun]),
            
            %% Check if it's waiting in receive
            if Status =:= waiting ->
                io:format("  *** WAITING (likely in receive block) ***~n");
               true -> ok
            end,
            
            %% Show opmode if available
            case proplists:get_value(opmode, Dict) of
                undefined -> ok;
                OpMode -> io:format("  OpMode: ~p~n", [OpMode])
            end
    end.

find_stuck_sensors() ->
    Processes = erlang:processes(),
    %% Look for processes that might be sensors (have opmode in dictionary)
    StuckSensors = lists:filter(fun(Pid) ->
        case erlang:process_info(Pid, [status, current_function, dictionary]) of
            undefined -> false;
            Info ->
                Status = proplists:get_value(status, Info),
                Dict = proplists:get_value(dictionary, Info),
                OpMode = proplists:get_value(opmode, Dict),
                CurrentFun = proplists:get_value(current_function, Info),
                
                %% Check if it's a sensor waiting
                Status =:= waiting andalso OpMode =/= undefined andalso
                (CurrentFun =:= {sensor, loop, 8} orelse 
                 CurrentFun =:= {gen, receive_after, 3})
        end
    end, Processes),
    
    io:format("Found ~p potentially stuck sensors~n", [length(StuckSensors)]),
    lists:foreach(fun(Pid) ->
        io:format("~n  Sensor ~p:~n", [Pid]),
        case erlang:process_info(Pid, [current_function, message_queue_len, current_stacktrace, initial_call]) of
            undefined -> io:format("    dead~n");
            Info ->
                CF = proplists:get_value(current_function, Info),
                MQL = proplists:get_value(message_queue_len, Info),
                Stack = proplists:get_value(current_stacktrace, Info),
                InitCall = proplists:get_value(initial_call, Info),
                io:format("    Current function: ~p~n", [CF]),
                io:format("    Message queue: ~p msgs~n", [MQL]),
                io:format("    Initial call: ~p~n", [InitCall]),
                io:format("    Stack trace:~n"),
                lists:foreach(fun(Frame) -> io:format("      ~p~n", [Frame]) end, Stack)
        end
    end, StuckSensors),
    ok.

%% Inspect a specific sensor PID to see what it's doing
inspect_sensor(Pid) when is_pid(Pid) ->
    io:format("~n=== Inspecting Sensor ~p ===~n", [Pid]),
    case erlang:process_info(Pid) of
        undefined ->
            io:format("Process is dead~n");
        Info ->
            lists:foreach(fun({Key, Val}) ->
                io:format("~p: ~p~n", [Key, Val])
            end, Info)
    end,
    ok;
inspect_sensor(Other) ->
    io:format("Not a PID: ~p~n", [Other]),
    ok.

find_cortex_processes() ->
    Processes = erlang:processes(),
    CortexProcs = lists:filter(fun(Pid) ->
        case erlang:process_info(Pid, [current_function, initial_call]) of
            undefined -> false;
            Info ->
                CurrentFun = proplists:get_value(current_function, Info),
                InitCall = proplists:get_value(initial_call, Info),
                (CurrentFun =:= {cortex, loop, 9}) orelse (InitCall =:= {cortex, prep, 1})
        end
    end, Processes),
    
    io:format("Found ~p cortex processes~n", [length(CortexProcs)]),
    lists:foreach(fun(Pid) ->
        io:format("~n  Cortex ~p:~n", [Pid]),
        case erlang:process_info(Pid, [status, current_function, message_queue_len, current_stacktrace]) of
            undefined -> io:format("    dead~n");
            Info ->
                Status = proplists:get_value(status, Info),
                CF = proplists:get_value(current_function, Info),
                MQL = proplists:get_value(message_queue_len, Info),
                Stack = proplists:get_value(current_stacktrace, Info),
                io:format("    Status: ~p~n", [Status]),
                io:format("    Current function: ~p~n", [CF]),
                io:format("    Message queue: ~p msgs~n", [MQL]),
                io:format("    Stack trace:~n"),
                lists:foreach(fun(Frame) -> io:format("      ~p~n", [Frame]) end, Stack)
        end
    end, CortexProcs),
    ok.

find_actuator_processes() ->
    Processes = erlang:processes(),
    ActuatorProcs = lists:filter(fun(Pid) ->
        case erlang:process_info(Pid, [current_function, initial_call, dictionary]) of
            undefined -> false;
            Info ->
                CurrentFun = proplists:get_value(current_function, Info),
                InitCall = proplists:get_value(initial_call, Info),
                Dict = proplists:get_value(dictionary, Info),
                OpMode = proplists:get_value(opmode, Dict),
                (CurrentFun =:= {actuator, loop, 8}) orelse 
                (InitCall =:= {actuator, prep, 1} andalso OpMode =/= undefined)
        end
    end, Processes),
    
    io:format("Found ~p actuator processes~n", [length(ActuatorProcs)]),
    lists:foreach(fun(Pid) ->
        io:format("~n  Actuator ~p:~n", [Pid]),
        case erlang:process_info(Pid, [status, current_function, message_queue_len, current_stacktrace]) of
            undefined -> io:format("    dead~n");
            Info ->
                Status = proplists:get_value(status, Info),
                CF = proplists:get_value(current_function, Info),
                MQL = proplists:get_value(message_queue_len, Info),
                Stack = proplists:get_value(current_stacktrace, Info),
                io:format("    Status: ~p~n", [Status]),
                io:format("    Current function: ~p~n", [CF]),
                io:format("    Message queue: ~p msgs~n", [MQL]),
                io:format("    Stack trace:~n"),
                lists:foreach(fun(Frame) -> io:format("      ~p~n", [Frame]) end, Stack)
        end
    end, ActuatorProcs),
    ok.

find_scape_processes() ->
    Processes = erlang:processes(),
    ScapeProcs = lists:filter(fun(Pid) ->
        case erlang:process_info(Pid, [current_function, initial_call]) of
            undefined -> false;
            Info ->
                CurrentFun = proplists:get_value(current_function, Info),
                InitCall = proplists:get_value(initial_call, Info),
                (CurrentFun =:= {fx, sim, 3}) orelse 
                (InitCall =:= {scape, prep, 1})
        end
    end, Processes),
    
    io:format("Found ~p scape (fx:sim) processes~n", [length(ScapeProcs)]),
    lists:foreach(fun(Pid) ->
        io:format("~n  Scape ~p:~n", [Pid]),
        case erlang:process_info(Pid, [status, current_function, message_queue_len, current_stacktrace]) of
            undefined -> io:format("    dead~n");
            Info ->
                Status = proplists:get_value(status, Info),
                CF = proplists:get_value(current_function, Info),
                MQL = proplists:get_value(message_queue_len, Info),
                Stack = proplists:get_value(current_stacktrace, Info),
                io:format("    Status: ~p~n", [Status]),
                io:format("    Current function: ~p~n", [CF]),
                io:format("    Message queue: ~p msgs~n", [MQL]),
                io:format("    Stack trace:~n"),
                lists:foreach(fun(Frame) -> io:format("      ~p~n", [Frame]) end, Stack)
        end
    end, ScapeProcs),
    ok.

find_neuron_processes() ->
    Processes = erlang:processes(),
    NeuronProcs = lists:filter(fun(Pid) ->
        case erlang:process_info(Pid, [current_function, initial_call]) of
            undefined -> false;
            Info ->
                CurrentFun = proplists:get_value(current_function, Info),
                InitCall = proplists:get_value(initial_call, Info),
                (CurrentFun =:= {neuron, loop, 6}) orelse 
                (InitCall =:= {neuron, prep, 1})
        end
    end, Processes),
    
    io:format("Found ~p neuron processes~n", [length(NeuronProcs)]),
    if length(NeuronProcs) > 10 ->
        io:format("(Showing first 10 neurons)~n"),
        lists:foreach(fun(Pid) ->
            show_neuron_detail(Pid)
        end, lists:sublist(NeuronProcs, 10));
    true ->
        lists:foreach(fun(Pid) ->
            show_neuron_detail(Pid)
        end, NeuronProcs)
    end,
    ok.

show_neuron_detail(Pid) ->
    io:format("~n  Neuron ~p:~n", [Pid]),
    case erlang:process_info(Pid, [status, current_function, message_queue_len, current_stacktrace]) of
        undefined -> io:format("    dead~n");
        Info ->
            Status = proplists:get_value(status, Info),
            CF = proplists:get_value(current_function, Info),
            MQL = proplists:get_value(message_queue_len, Info),
            Stack = proplists:get_value(current_stacktrace, Info),
            io:format("    Status: ~p~n", [Status]),
            io:format("    Current function: ~p~n", [CF]),
            io:format("    Message queue: ~p msgs~n", [MQL]),
            io:format("    Stack trace (first 2 frames):~n"),
            lists:foreach(fun(Frame) -> io:format("      ~p~n", [Frame]) end, lists:sublist(Stack, 2))
    end.

%% Quick command to kill everything and start fresh
reset() ->
    io:format("Killing all processes...~n"),
    case whereis(benchmarker) of
        undefined -> ok;
        Pid -> exit(Pid, kill)
    end,
    case whereis(population_monitor) of
        undefined -> ok;
        Pid2 -> exit(Pid2, kill)
    end,
    timer:sleep(100),
    io:format("Done. You can now restart.~n"),
    ok.

