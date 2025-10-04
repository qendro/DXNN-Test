-module(topology_validator).
-compile(export_all).
-include("records.hrl").

%% Validates the spawned topology to ensure all connections are valid
%% This catches bugs immediately when they happen, not later
validate_spawned_topology(IdsNPIds, Agent_Id, SIds, AIds, NIds) ->
    io:format("Validating topology for agent ~p...~n", [Agent_Id]),
    
    %% Validate all neurons were spawned and are alive
    validate_neurons(IdsNPIds, NIds),
    
    %% Validate all sensors were spawned and are alive
    validate_sensors(IdsNPIds, SIds),
    
    %% Validate all actuators were spawned and are alive
    validate_actuators(IdsNPIds, AIds),
    
    %% Validate sensor fanout references exist
    validate_sensor_fanouts(IdsNPIds, SIds),
    
    %% Validate actuator fanin references exist (CRITICAL - prevents the freeze bug!)
    validate_actuator_fanins(IdsNPIds, AIds),
    
    %% Validate neuron input/output references exist
    validate_neuron_connections(IdsNPIds, NIds),
    
    io:format("Topology validation PASSED for agent ~p~n", [Agent_Id]),
    ok.

%% Validate neurons were spawned and are alive
validate_neurons(IdsNPIds, NIds) ->
    lists:foreach(fun(NId) ->
        case ets:lookup(IdsNPIds, NId) of
            [] -> 
                io:format("FATAL ERROR: Neuron ~p not found in IdsNPIds table!~n", [NId]),
                exit({neuron_not_spawned, NId});
            [{NId, NPid}] ->
                case is_process_alive(NPid) of
                    false -> 
                        io:format("FATAL ERROR: Neuron ~p has dead PID ~p!~n", [NId, NPid]),
                        exit({neuron_pid_dead, NId, NPid});
                    true -> ok
                end
        end
    end, NIds).

%% Validate sensors were spawned and are alive
validate_sensors(IdsNPIds, SIds) ->
    lists:foreach(fun(SId) ->
        case ets:lookup(IdsNPIds, SId) of
            [] -> 
                io:format("FATAL ERROR: Sensor ~p not found in IdsNPIds table!~n", [SId]),
                exit({sensor_not_spawned, SId});
            [{SId, SPid}] ->
                case is_process_alive(SPid) of
                    false -> 
                        io:format("FATAL ERROR: Sensor ~p has dead PID ~p!~n", [SId, SPid]),
                        exit({sensor_pid_dead, SId, SPid});
                    true -> ok
                end
        end
    end, SIds).

%% Validate actuators were spawned and are alive
validate_actuators(IdsNPIds, AIds) ->
    lists:foreach(fun(AId) ->
        case ets:lookup(IdsNPIds, AId) of
            [] -> 
                io:format("FATAL ERROR: Actuator ~p not found in IdsNPIds table!~n", [AId]),
                exit({actuator_not_spawned, AId});
            [{AId, APid}] ->
                case is_process_alive(APid) of
                    false -> 
                        io:format("FATAL ERROR: Actuator ~p has dead PID ~p!~n", [AId, APid]),
                        exit({actuator_pid_dead, AId, APid});
                    true -> ok
                end
        end
    end, AIds).

%% Validate sensor fanout references exist
validate_sensor_fanouts(IdsNPIds, SIds) ->
    lists:foreach(fun(SId) ->
        S = genotype:dirty_read({sensor, SId}),
        Fanout_Ids = S#sensor.fanout_ids,
        lists:foreach(fun(FanoutId) ->
            case ets:lookup(IdsNPIds, FanoutId) of
                [] -> 
                    io:format("FATAL ERROR: Sensor ~p fanout ~p not found in IdsNPIds!~n", 
                             [SId, FanoutId]),
                    exit({sensor_fanout_not_found, SId, FanoutId});
                [{FanoutId, FanoutPid}] ->
                    case is_process_alive(FanoutPid) of
                        false -> 
                            io:format("FATAL ERROR: Sensor ~p fanout ~p has DEAD PID ~p!~n", 
                                     [SId, FanoutId, FanoutPid]),
                            io:format("  Substrate crashed during initialization!~n"),
                            exit({sensor_fanout_pid_dead, SId, FanoutId, FanoutPid});
                        true -> ok
                    end
            end
        end, Fanout_Ids)
    end, SIds).

%% Validate actuator fanin references exist (THIS PREVENTS THE FREEZE BUG!)
validate_actuator_fanins(IdsNPIds, AIds) ->
    lists:foreach(fun(AId) ->
        A = genotype:dirty_read({actuator, AId}),
        Fanin_Ids = A#actuator.fanin_ids,
        lists:foreach(fun(FaninId) ->
            case ets:lookup(IdsNPIds, FaninId) of
                [] -> 
                    io:format("FATAL ERROR: Actuator ~p fanin ~p not found in IdsNPIds!~n", 
                             [AId, FaninId]),
                    io:format("  This would cause a freeze - actuator waiting for non-existent process!~n"),
                    exit({actuator_fanin_not_found, AId, FaninId});
                [{FaninId, FaninPid}] ->
                    case is_process_alive(FaninPid) of
                        false -> 
                            io:format("FATAL ERROR: Actuator ~p fanin ~p has dead PID ~p!~n", 
                                     [AId, FaninId, FaninPid]),
                            io:format("  This would cause a freeze - actuator waiting for dead process!~n"),
                            exit({actuator_fanin_pid_dead, AId, FaninId, FaninPid});
                        true -> ok
                    end
            end
        end, Fanin_Ids)
    end, AIds).

%% Validate neuron input/output references exist
validate_neuron_connections(IdsNPIds, NIds) ->
    lists:foreach(fun(NId) ->
        N = genotype:dirty_read({neuron, NId}),
        
        %% Validate input connections
        Input_IdPs = N#neuron.input_idps,
        lists:foreach(fun({InputId, _Weights}) ->
            case ets:lookup(IdsNPIds, InputId) of
                [] -> 
                    io:format("FATAL ERROR: Neuron ~p input ~p not found in IdsNPIds!~n", 
                             [NId, InputId]),
                    exit({neuron_input_not_found, NId, InputId});
                [{InputId, InputPid}] ->
                    case is_process_alive(InputPid) of
                        false -> 
                            io:format("FATAL ERROR: Neuron ~p input ~p has dead PID ~p!~n", 
                                     [NId, InputId, InputPid]),
                            exit({neuron_input_pid_dead, NId, InputId, InputPid});
                        true -> ok
                    end
            end
        end, Input_IdPs),
        
        %% Validate output connections
        Output_Ids = N#neuron.output_ids,
        lists:foreach(fun(OutputId) ->
            case ets:lookup(IdsNPIds, OutputId) of
                [] -> 
                    io:format("FATAL ERROR: Neuron ~p output ~p not found in IdsNPIds!~n", 
                             [NId, OutputId]),
                    exit({neuron_output_not_found, NId, OutputId});
                [{OutputId, OutputPid}] ->
                    case is_process_alive(OutputPid) of
                        false -> 
                            io:format("FATAL ERROR: Neuron ~p output ~p has dead PID ~p!~n", 
                                     [NId, OutputId, OutputPid]),
                            exit({neuron_output_pid_dead, NId, OutputId, OutputPid});
                        true -> ok
                    end
            end
        end, Output_Ids)
    end, NIds).

