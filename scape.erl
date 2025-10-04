
-module(scape).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId,Node)->
    %fx:log(io_lib:format("Spawning scape for ExoSelf: ~p on Node: ~p~n", [ExoSelf_PId, Node])),
	spawn(Node,?MODULE,prep,[ExoSelf_PId]).

prep(ExoSelf_PId) ->
	receive 
		{ExoSelf_PId,Name} ->
            %fx:log(io_lib:format("Recieved message from ExoSelf: ~p with Name: ~p~n", [ExoSelf_PId, Name])),
            %fx:log(io_lib:format("calling scape:Name for ExoSelf: ~p~n", [ExoSelf_PId])),
			scape:Name(ExoSelf_PId)
	end.

distance(Vector1,Vector2)->
	distance(Vector1,Vector2,0).	
distance([Val1|Vector1],[Val2|Vector2],Acc)->
	distance(Vector1,Vector2,Acc+math:pow(Val2-Val1,2));
distance([],[],Acc)->
	math:sqrt(Acc).
	
fx_sim(Exoself_PId)->
    %fx:log(io_lib:format("Starting FX Simulation for Exoself: ~p~n",[Exoself_PId])),
	fx:sim(Exoself_PId).

%% Live bridge: start/link live_scape and signal live simulation
live_sim(Exoself_PId) ->
    case whereis(live_scape) of
        undefined ->
            case live_scape:start_link() of
                {ok, _Pid} -> ok;
                _ -> ok
            end;
        _Pid -> ok
    end,
    fx:sim(Exoself_PId).
    %live_scape ! {Exoself_PId, live_sim}.
