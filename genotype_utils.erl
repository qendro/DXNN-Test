%% Utility functions for working with genotypes in DXNN
-module(genotype_utils).
-compile(export_all).
-include("records.hrl").

%% Find and print the best genotype from the Mnesia database
print_best_genotype() ->
    print_best_genotype(test).

print_best_genotype(Population_Id) ->
    F = fun() ->
        % Get all agent keys from the agent table
        Agent_Keys = mnesia:dirty_all_keys(agent),
        io:format("Found ~p agents in the database~n", [length(Agent_Keys)]),
        
        % Filter agents by population if specified
        Filtered_Agents = case Population_Id of
            all ->
                Agent_Keys;
            _ ->
                [Agent_Id || Agent_Id <- Agent_Keys, 
                 case mnesia:dirty_read({agent, Agent_Id}) of
                     [] -> false;
                     [Agent] -> Agent#agent.population_id == Population_Id
                 end]
        end,
        
        io:format("Found ~p agents in population ~p~n", [length(Filtered_Agents), Population_Id]),
        
        % Find the best agent (highest fitness)
        case Filtered_Agents of
            [] ->
                io:format("No agents found in population ~p~n", [Population_Id]);
            _ ->
                Best_Agent = lists:foldl(fun(Agent_Id, Best_Acc) ->
                    case mnesia:dirty_read({agent, Agent_Id}) of
                        [] -> Best_Acc;
                        [Agent] ->
                            case Best_Acc of
                                undefined -> Agent;
                                Best when Agent#agent.fitness > Best#agent.fitness -> Agent;
                                _ -> Best_Acc
                            end
                    end
                end, undefined, Filtered_Agents),
                
                case Best_Agent of
                    undefined ->
                        io:format("Could not find best agent~n");
                    Agent ->
                        io:format("Best agent found: ~p~n", [Agent#agent.id]),
                        io:format("Fitness: ~p~n", [Agent#agent.fitness]),
                        io:format("Generation: ~p~n", [Agent#agent.generation]),
                        io:format("Specie ID: ~p~n", [Agent#agent.specie_id]),
                        io:format("~n--- Complete Genotype ---~n"),
                        genotype:print(Agent#agent.id)
                end
        end
    end,
    mnesia:transaction(F).

%% List all agents with their fitness values
list_all_agents() ->
    list_all_agents(test).

list_all_agents(Population_Id) ->
    F = fun() ->
        Agent_Keys = mnesia:dirty_all_keys(agent),
        io:format("=== All Agents in Population ~p ===~n", [Population_Id]),
        
        Agents = lists:foldl(fun(Agent_Id, Acc) ->
            case mnesia:dirty_read({agent, Agent_Id}) of
                [] -> Acc;
                [Agent] when Population_Id == all ->
                    [{Agent#agent.id, Agent#agent.fitness, Agent#agent.generation, Agent#agent.specie_id} | Acc];
                [Agent] when Agent#agent.population_id == Population_Id ->
                    [{Agent#agent.id, Agent#agent.fitness, Agent#agent.generation, Agent#agent.specie_id} | Acc];
                _ -> Acc
            end
        end, [], Agent_Keys),
        
        % Sort by fitness (descending)
        Sorted_Agents = lists:sort(fun({_, F1, _, _}, {_, F2, _, _}) -> F1 >= F2 end, Agents),
        
        io:format("Found ~p agents:~n", [length(Sorted_Agents)]),
        lists:foreach(fun({Agent_Id, Fitness, Generation, Specie_Id}) ->
            io:format("Agent: ~p, Fitness: ~p, Generation: ~p, Specie: ~p~n", 
                     [Agent_Id, Fitness, Generation, Specie_Id])
        end, Sorted_Agents),
        
        Sorted_Agents
    end,
    mnesia:transaction(F).

%% Print the top N agents
print_top_agents(N) ->
    print_top_agents(N, test).

print_top_agents(N, Population_Id) ->
    {atomic, Agents} = list_all_agents(Population_Id),
    Top_Agents = lists:sublist(Agents, N),
    
    io:format("~n=== Top ~p Agents ===~n", [N]),
    lists:foreach(fun({Agent_Id, Fitness, Generation, Specie_Id}) ->
        io:format("~nAgent: ~p (Fitness: ~p, Generation: ~p, Specie: ~p)~n", 
                 [Agent_Id, Fitness, Generation, Specie_Id]),
        io:format("--- Genotype ---~n"),
        genotype:print(Agent_Id)
    end, Top_Agents).

%% Get agent statistics
get_agent_stats() ->
    get_agent_stats(test).

get_agent_stats(Population_Id) ->
    F = fun() ->
        Agent_Keys = mnesia:dirty_all_keys(agent),
        
        Agents = lists:foldl(fun(Agent_Id, Acc) ->
            case mnesia:dirty_read({agent, Agent_Id}) of
                [] -> Acc;
                [Agent] when Population_Id == all ->
                    [Agent | Acc];
                [Agent] when Agent#agent.population_id == Population_Id ->
                    [Agent | Acc];
                _ -> Acc
            end
        end, [], Agent_Keys),
        
        case Agents of
            [] ->
                io:format("No agents found in population ~p~n", [Population_Id]);
            _ ->
                Fitnesses = [Agent#agent.fitness || Agent <- Agents],
                Generations = [Agent#agent.generation || Agent <- Agents],
                
                Max_Fitness = lists:max(Fitnesses),
                Min_Fitness = lists:min(Fitnesses),
                Avg_Fitness = lists:sum(Fitnesses) / length(Fitnesses),
                
                Max_Generation = lists:max(Generations),
                Min_Generation = lists:min(Generations),
                Avg_Generation = lists:sum(Generations) / length(Generations),
                
                io:format("=== Agent Statistics for Population ~p ===~n", [Population_Id]),
                io:format("Total Agents: ~p~n", [length(Agents)]),
                io:format("Fitness - Max: ~p, Min: ~p, Avg: ~.2f~n", [Max_Fitness, Min_Fitness, Avg_Fitness]),
                io:format("Generation - Max: ~p, Min: ~p, Avg: ~.2f~n", [Max_Generation, Min_Generation, Avg_Generation]),
                
                {length(Agents), Max_Fitness, Min_Fitness, Avg_Fitness, Max_Generation, Min_Generation, Avg_Generation}
        end
    end,
    mnesia:transaction(F).
%% Return the best agent id (by fitness)
find_best_agent() -> find_best_agent(test).

find_best_agent(Population_Id) ->
    F = fun() ->
        Agent_Keys = mnesia:dirty_all_keys(agent),
        Filtered = case Population_Id of
            all -> Agent_Keys;
            _ -> [Agent_Id || Agent_Id <- Agent_Keys,
                   case mnesia:dirty_read({agent, Agent_Id}) of
                       [] -> false;
                       [Agent] -> Agent#agent.population_id == Population_Id
                   end]
        end,
        case Filtered of
            [] -> undefined;
            _ ->
                Best = lists:foldl(
                    fun(Agent_Id, Acc) ->
                        case mnesia:dirty_read({agent, Agent_Id}) of
                            [] -> Acc;
                            [Agent] ->
                                case Acc of
                                    undefined -> Agent;
                                    BestSoFar when Agent#agent.fitness > BestSoFar#agent.fitness -> Agent;
                                    _ -> Acc
                                end
                        end
                    end, undefined, Filtered),
                case Best of
                    undefined -> undefined;
                    Agent -> Agent#agent.id
                end
        end
    end,
    mnesia:transaction(F).
