
-module(actuator).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,prep,[ExoSelf_PId]).

prep(ExoSelf_PId) -> 
	receive 
		{ExoSelf_PId,{Id,Cx_PId,Scape,ActuatorName,Parameters,Fanin_PIds,OpMode}} ->
			put(opmode,OpMode),
			loop(Id,ExoSelf_PId,Cx_PId,Scape,ActuatorName,Parameters,{Fanin_PIds,Fanin_PIds},[])
	end.
%When gen/2 is executed it spawns the actuator element and immediately begins to wait for its initial state message.

loop(Id,ExoSelf_PId,Cx_PId,Scape,AName,Parameters,{[From_PId|Fanin_PIds],MFanin_PIds},Acc) ->
	receive
		{From_PId,forward,Input} ->
			agent_logger:log_message(ExoSelf_PId, actuator, Id, received, {From_PId, forward}),
			loop(Id,ExoSelf_PId,Cx_PId,Scape,AName,Parameters,{Fanin_PIds,MFanin_PIds},lists:append(Input,Acc));
		{ExoSelf_PId,terminate} ->
			agent_logger:log_message(ExoSelf_PId, actuator, Id, received, {ExoSelf_PId, terminate}),
			ok
	end;
loop(Id,ExoSelf_PId,Cx_PId,Scape,AName,Parameters,{[],MFanin_PIds},Acc)->
	agent_logger:log_message(ExoSelf_PId, actuator, Id, all_inputs_received, length(Acc)),
	{Fitness,EndFlag} = actuator:AName(ExoSelf_PId,lists:reverse(Acc),Parameters,Scape),
	agent_logger:log_message(ExoSelf_PId, actuator, Id, sent, {Cx_PId, sync, Fitness, EndFlag}),
	Cx_PId ! {self(),sync,Fitness,EndFlag},
	loop(Id,ExoSelf_PId,Cx_PId,Scape,AName,Parameters,{MFanin_PIds,MFanin_PIds},[]).
%The actuator process gathers the control signals from the neurons, appending them to the accumulator. The order in which the signals are accumulated into a vector is in the same order as the neuron ids are stored within NIds. Once all the signals have been gathered, the actuator sends cortex the sync signal, executes its function, and then again begins to wait for the neural signals from the output layer by reseting the Fanin_PIds from the second copy of the list.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ACTUATORS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pts(ExoSelf_PId,Result,_Scape)->
	io:format("actuator:pts(Result): ~p~n",[Result]),
	{1,0}.
%The pts/2 actuation function simply prints to screen the vector passed to it.


% Actuator allows the agent to send a trade commmand to the Scape and receive the fitness and halt flag in return.
% TradeSignal will be:
% -1 → Short (sell).
% 0 → Hold or close position.
% 1 → Long (buy).
fx_Trade(ExoSelf_PId,Output,Parameters,Scape)->
	[TradeSignal] = Output,
	%io:format("[TRADE] ExoSelf=~p Pair=~p Signal=~p~n", [ExoSelf_PId, config:primary_currency_pair(), TradeSignal]),
	Scape ! {self(),trade,config:primary_currency_pair(),functions:trinary(TradeSignal)},
	receive 
		{Scape,Fitness,HaltFlag}->
			{Fitness,HaltFlag}
	end.
