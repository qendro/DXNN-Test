-module(agent_logger).
-compile(export_all).
-include("records.hrl").

%% Selective event logging - only logs topology events and mutations
%% Minimal performance impact (<1%)

%% Initialize logging directory
init() ->
    case filelib:is_dir("logs/agents") of
        false -> file:make_dir("logs/agents");
        true -> ok
    end.

%% Log agent spawn with all its parameters
log_agent_spawn(Agent_Id) ->
    A = genotype:dirty_read({agent, Agent_Id}),
    Cx = genotype:dirty_read({cortex, A#agent.cx_id}),
    
    LogData = io_lib:format(
        "~n========================================~n"
        "AGENT SPAWN: ~p~n"
        "Time: ~p~n"
        "----------------------------------------~n"
        "Generation: ~p~n"
        "Population ID: ~p~n"
        "Specie ID: ~p~n"
        "Cortex ID: ~p~n"
        "Encoding Type: ~p~n"
        "Heredity Type: ~p~n"
        "Fingerprint: ~p~n"
        "Pattern: ~p~n"
        "Fitness: ~p~n"
        "Constraint: ~p~n"
        "Evo History Length: ~p~n"
        "Substrate ID: ~p~n"
        "----------------------------------------~n"
        "Topology:~n"
        "  Sensors: ~p (~p)~n"
        "  Neurons: ~p (~p)~n"
        "  Actuators: ~p (~p)~n"
        "========================================~n~n",
        [Agent_Id, timestamp(),
         A#agent.generation,
         A#agent.population_id,
         A#agent.specie_id,
         A#agent.cx_id,
         A#agent.encoding_type,
         A#agent.heredity_type,
         A#agent.fingerprint,
         A#agent.pattern,
         A#agent.fitness,
         A#agent.constraint,
         length(A#agent.evo_hist),
         A#agent.substrate_id,
         length(Cx#cortex.sensor_ids), Cx#cortex.sensor_ids,
         length(Cx#cortex.neuron_ids), Cx#cortex.neuron_ids,
         length(Cx#cortex.actuator_ids), Cx#cortex.actuator_ids]
    ),
    
    write_log(Agent_Id, LogData).

%% Log topology after spawning (with PIDs)
log_topology_spawned(Agent_Id, IdsNPIds, SIds, NIds, AIds) ->
    %% Extract PID mappings
    SPids = [begin [{SId, SPid}] = ets:lookup(IdsNPIds, SId), {SId, SPid} end || SId <- SIds],
    NPids = [begin [{NId, NPid}] = ets:lookup(IdsNPIds, NId), {NId, NPid} end || NId <- NIds],
    APids = [begin [{AId, APid}] = ets:lookup(IdsNPIds, AId), {AId, APid} end || AId <- AIds],
    
    LogData = io_lib:format(
        "TOPOLOGY SPAWNED: ~p~n"
        "Time: ~p~n"
        "Sensors (ID -> PID):~n~s~n"
        "Neurons (ID -> PID):~n~s~n"
        "Actuators (ID -> PID):~n~s~n"
        "~n",
        [Agent_Id, timestamp(),
         format_id_pid_list(SPids),
         format_id_pid_list(NPids),
         format_id_pid_list(APids)]
    ),
    
    write_log(Agent_Id, LogData).

%% Log topology linking details
log_topology_linked(Agent_Id, Type, Details) ->
    LogData = io_lib:format(
        "TOPOLOGY LINKED: ~p - ~p~n"
        "Time: ~p~n"
        "Details: ~p~n~n",
        [Agent_Id, Type, timestamp(), Details]
    ),
    
    write_log(Agent_Id, LogData).

%% Log mutations with full details
log_mutation(Agent_Id, MutationOp, Details) ->
    LogData = io_lib:format(
        "MUTATION: ~p~n"
        "Time: ~p~n"
        "Operation: ~p~n"
        "Details: ~p~n~n",
        [Agent_Id, timestamp(), MutationOp, Details]
    ),
    
    write_log(Agent_Id, LogData).

%% Log agent termination
log_agent_terminate(Agent_Id, Reason, Fitness, Cycles) ->
    LogData = io_lib:format(
        "AGENT TERMINATE: ~p~n"
        "Time: ~p~n"
        "Reason: ~p~n"
        "Final Fitness: ~p~n"
        "Total Cycles: ~p~n"
        "========================================~n~n",
        [Agent_Id, timestamp(), Reason, Fitness, Cycles]
    ),
    
    write_log(Agent_Id, LogData).

%% Log validation errors (when topology validation fails)
log_validation_error(Agent_Id, Error) ->
    LogData = io_lib:format(
        "*** VALIDATION ERROR ***~n"
        "Agent: ~p~n"
        "Time: ~p~n"
        "Error: ~p~n"
        "*** This agent will be terminated ***~n~n",
        [Agent_Id, timestamp(), Error]
    ),
    
    write_log(Agent_Id, LogData).

%% Log individual messages between processes (for debugging)
log_message(ExoSelf_PId, ProcessType, ProcessId, Action, MessageInfo) ->
    %% Get agent_id from exoself PID - stored in process dictionary
    Agent_Id = case get({exoself_to_agent, ExoSelf_PId}) of
        undefined ->
            %% Try to extract from genotype if not cached
            case catch genotype:dirty_read({exoself, ExoSelf_PId}) of
                {'EXIT', _} -> ExoSelf_PId;  % Fallback to PID
                Agent_Id_Val -> Agent_Id_Val
            end;
        Cached_Agent_Id -> Cached_Agent_Id
    end,
    
    LogData = io_lib:format(
        "[~s] ~p(~p) ~p: ~p~n",
        [timestamp(), ProcessType, ProcessId, Action, MessageInfo]
    ),
    
    write_log(Agent_Id, LogData).

%% Helper: Write to log file
write_log(Agent_Id, Data) ->
    Filename = log_filename(Agent_Id),
    file:write_file(Filename, Data, [append]).

%% Helper: Generate log filename
log_filename(Agent_Id) ->
    lists:flatten(io_lib:format("logs/agents/agent_~p.log", [Agent_Id])).

%% Helper: Format timestamp
timestamp() ->
    {{Y,M,D},{H,Min,S}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", 
                                [Y,M,D,H,Min,S])).

%% Helper: Format ID->PID mappings
format_id_pid_list(IdPidList) ->
    lists:flatten([io_lib:format("  ~p -> ~p~n", [Id, Pid]) || {Id, Pid} <- IdPidList]).

