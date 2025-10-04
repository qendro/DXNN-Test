-module(test_substrate_spawn).
-compile(export_all).
-include("records.hrl").

test() ->
    io:format("~n=== TESTING SUBSTRATE SPAWN ISSUE ===~n"),
    
    %% Get a real agent that has 2 sensors (from add_sensor mutation)
    io:format("Looking for agents with 2 sensors...~n"),
    
    F = fun() ->
        AllAgents = mnesia:dirty_all_keys(agent),
        find_two_sensor_agent(AllAgents)
    end,
    
    case mnesia:transaction(F) of
        {atomic, {ok, Agent_Id}} ->
            io:format("Found agent with 2 sensors: ~p~n", [Agent_Id]),
            test_agent_spawn(Agent_Id);
        {atomic, not_found} ->
            io:format("No agent with 2 sensors found. Creating one...~n"),
            create_and_test()
    end.

find_two_sensor_agent([Agent_Id | Rest]) ->
    A = mnesia:dirty_read({agent, Agent_Id}),
    case A of
        [Agent] when Agent#agent.encoding_type =:= substrate ->
            Cx = mnesia:dirty_read({cortex, Agent#agent.cx_id}),
            case Cx of
                [C] when length(C#cortex.sensor_ids) =:= 2 ->
                    {ok, Agent_Id};
                _ ->
                    find_two_sensor_agent(Rest)
            end;
        _ ->
            find_two_sensor_agent(Rest)
    end;
find_two_sensor_agent([]) ->
    not_found.

test_agent_spawn(Agent_Id) ->
    io:format("~n--- Testing agent spawn sequence ---~n"),
    
    A = genotype:dirty_read({agent, Agent_Id}),
    Cx = genotype:dirty_read({cortex, A#agent.cx_id}),
    
    io:format("Agent: ~p~n", [Agent_Id]),
    io:format("Encoding: ~p~n", [A#agent.encoding_type]),
    io:format("Sensor IDs: ~p~n", [Cx#cortex.sensor_ids]),
    io:format("Generation: ~p~n", [A#agent.generation]),
    
    %% Check sensors
    io:format("~nChecking sensors:~n"),
    lists:foreach(fun(SId) ->
        S = genotype:dirty_read({sensor, SId}),
        io:format("  Sensor ~p:~n", [SId]),
        io:format("    Name: ~p~n", [S#sensor.name]),
        io:format("    Generation: ~p~n", [S#sensor.generation]),
        io:format("    Fanout_ids: ~p~n", [S#sensor.fanout_ids])
    end, Cx#cortex.sensor_ids),
    
    %% Check substrate
    io:format("~nSubstrate ID: ~p~n", [A#agent.substrate_id]),
    Substrate = genotype:dirty_read({substrate, A#agent.substrate_id}),
    io:format("Substrate: ~p~n", [Substrate]),
    
    io:format("~n--- Now attempting spawn ---~n"),
    
    %% Try spawning just the substrate and sensors to see what happens
    IdsNPIds = ets:new(test_ids, [set, private]),
    
    %% Spawn substrate
    io:format("1. Spawning substrate...~n"),
    Substrate_PId = substrate:gen(self(), node()),
    io:format("   Substrate PID: ~p, Alive: ~p~n", [Substrate_PId, is_process_alive(Substrate_PId)]),
    ets:insert(IdsNPIds, {A#agent.substrate_id, Substrate_PId}),
    
    timer:sleep(100),
    io:format("   After 100ms, Alive: ~p~n", [is_process_alive(Substrate_PId)]),
    
    %% Spawn sensors
    io:format("~n2. Spawning sensors...~n"),
    SIds = Cx#cortex.sensor_ids,
    SPIds = [sensor:gen(self(), node()) || _ <- SIds],
    lists:foreach(fun({SId, SPId}) ->
        io:format("   Sensor ~p -> PID ~p, Alive: ~p~n", [SId, SPId, is_process_alive(SPId)]),
        ets:insert(IdsNPIds, {SId, SPId})
    end, lists:zip(SIds, SPIds)),
    
    timer:sleep(100),
    io:format("   After 100ms, Substrate still alive: ~p~n", [is_process_alive(Substrate_PId)]),
    
    %% Send init to substrate
    io:format("~n3. Sending init to substrate...~n"),
    Sensors = [genotype:dirty_read({sensor, SId}) || SId <- SIds],
    Actuators = [genotype:dirty_read({actuator, AId}) || AId <- Cx#cortex.actuator_ids],
    
    Substrate_PId ! {self(), init, {Sensors, Actuators, SPIds, [], [], [], 
                                    Substrate#substrate.densities, 
                                    Substrate#substrate.plasticity, 
                                    Substrate#substrate.linkform}},
    
    timer:sleep(100),
    io:format("   After sending init, Substrate alive: ~p~n", [is_process_alive(Substrate_PId)]),
    
    timer:sleep(1000),
    io:format("   After 1 second, Substrate alive: ~p~n", [is_process_alive(Substrate_PId)]),
    
    io:format("~n=== TEST COMPLETE ===~n"),
    ok.

create_and_test() ->
    io:format("Creating test not implemented yet~n"),
    ok.

