-module(inspect_agent).
-compile(export_all).

%% Check what the stuck neuron is waiting for
check_neuron(NeuronPid) ->
    io:format("~n=== Inspecting Neuron ~p ===~n", [NeuronPid]),
    case erlang:process_info(NeuronPid, [current_stacktrace, dictionary]) of
        undefined ->
            io:format("Neuron is dead~n");
        Info ->
            Stack = proplists:get_value(current_stacktrace, Info),
            Dict = proplists:get_value(dictionary, Info),
            
            io:format("Stack trace:~n"),
            lists:foreach(fun(Frame) -> 
                io:format("  ~p~n", [Frame]) 
            end, Stack),
            
            io:format("~nProcess dictionary:~n"),
            lists:foreach(fun({Key, Val}) -> 
                io:format("  ~p: ~p~n", [Key, Val]) 
            end, Dict)
    end.

%% Check the actuator's fanin list
check_actuator(ActuatorPid) ->
    io:format("~n=== Inspecting Actuator ~p ===~n", [ActuatorPid]),
    case erlang:process_info(ActuatorPid, [current_stacktrace]) of
        undefined ->
            io:format("Actuator is dead~n");
        Info ->
            Stack = proplists:get_value(current_stacktrace, Info),
            io:format("Full stack trace:~n"),
            lists:foreach(fun(Frame) -> 
                io:format("  ~p~n", [Frame]) 
            end, Stack)
    end.

%% Check if PIDs are alive
check_pids(Pids) ->
    io:format("~nChecking if PIDs are alive:~n"),
    lists:foreach(fun(Pid) ->
        case is_process_alive(Pid) of
            true -> 
                Info = erlang:process_info(Pid, [current_function, message_queue_len]),
                io:format("  ~p: ALIVE - ~p~n", [Pid, Info]);
            false -> 
                io:format("  ~p: DEAD~n", [Pid])
        end
    end, Pids).

%% Find which agent owns these stuck processes
find_agent() ->
    io:format("~n=== Finding stuck agent ===~n"),
    
    %% Look for exoself processes by function name
    AllProcs = erlang:processes(),
    ExoselfProcs = lists:filter(fun(Pid) ->
        case erlang:process_info(Pid, [current_function, initial_call]) of
            undefined -> false;
            Info ->
                CF = proplists:get_value(current_function, Info),
                IC = proplists:get_value(initial_call, Info),
                %% Check if it's an exoself process
                case CF of
                    {exoself, _, _} -> true;
                    _ -> case IC of
                        {exoself, _, _} -> true;
                        _ -> false
                    end
                end
        end
    end, AllProcs),
    
    io:format("Found ~p exoself processes:~n", [length(ExoselfProcs)]),
    lists:foreach(fun(Pid) ->
        Info = erlang:process_info(Pid, [current_function, message_queue_len]),
        io:format("  ~p: ~p~n", [Pid, Info])
    end, ExoselfProcs),
    
    io:format("~n=== Checking all neurons ===~n"),
    AllNeurons = lists:filter(fun(Pid) ->
        case erlang:process_info(Pid, [current_function, initial_call]) of
            undefined -> false;
            Info ->
                IC = proplists:get_value(initial_call, Info),
                IC =:= {neuron, prep, 1}
        end
    end, AllProcs),
    
    io:format("Total neurons: ~p (should match expected count)~n", [length(AllNeurons)]),
    io:format("Alive neurons: ~p~n", [length([P || P <- AllNeurons, is_process_alive(P)])]),
    
    ok.

