
-module(cortex).
-compile(export_all).
-include("records.hrl").
-record(state,{id,exoself_pid,spids,npids,apids,cycle_acc=0,fitness_acc=0,endflag=0,status}).

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,prep,[ExoSelf_PId]).

prep(ExoSelf_PId) ->
	{V1,V2,V3} = now(),
	random:seed(V1,V2,V3),
	receive 
		{ExoSelf_PId,Id,SPIds,NPIds,APIds} ->
			agent_logger:log_message(ExoSelf_PId, cortex, Id, received, {ExoSelf_PId, init}),
			put(start_time,now()),
			[begin
				SPId ! {self(),sync},
				agent_logger:log_message(ExoSelf_PId, cortex, Id, sent, {SPId, sync})
			end || SPId <- SPIds],
			loop(Id,ExoSelf_PId,SPIds,{APIds,APIds},NPIds,1,0,0,active)
	end.
%The gen/2 function spawns the cortex element, which immediately starts to wait for a the state message from the same process that spawned it, exoself. The initial state message contains the sensor, actuator, and neuron PId lists. The message also specifies how many total Sense-Think-Act cycles the Cortex should execute before terminating the NN system. Once we implement the learning algorithm, the termination criteria will depend on the fitness of the NN, or some other useful property

loop(Id,ExoSelf_PId,SPIds,{[APId|APIds],MAPIds},NPIds,CycleAcc,FitnessAcc,EFAcc,active) ->
	receive 
		{APId,sync,Fitness,EndFlag} ->
			agent_logger:log_message(ExoSelf_PId, cortex, Id, received, {APId, sync, Fitness, EndFlag}),
			case Fitness == goal_reached of
				true ->
					put(goal_reached,true),
					loop(Id,ExoSelf_PId,SPIds,{APIds,MAPIds},NPIds,CycleAcc,FitnessAcc,EFAcc+EndFlag,active);
				false ->
					loop(Id,ExoSelf_PId,SPIds,{APIds,MAPIds},NPIds,CycleAcc,FitnessAcc+Fitness,EFAcc+EndFlag,active)
			end;
		terminate ->
			%io:format("Cortex:~p is terminating.~n",[Id]),
			[PId ! {self(),terminate} || PId <- SPIds],
			[PId ! {self(),terminate} || PId <- MAPIds],
			[PId ! {self(),terminate} || PId <- NPIds]
	end;
loop(Id,ExoSelf_PId,SPIds,{[],MAPIds},NPIds,CycleAcc,FitnessAcc,EFAcc,active)->
	case EFAcc > 0 of
		true ->%Organism finished evaluation
			agent_logger:log_message(ExoSelf_PId, cortex, Id, sent, {ExoSelf_PId, evaluation_completed, CycleAcc}),
			TimeDif=timer:now_diff(now(),get(start_time)),
			ExoSelf_PId ! {self(),evaluation_completed,FitnessAcc,CycleAcc,TimeDif,get(goal_reached)},
			cortex:loop(Id,ExoSelf_PId,SPIds,{MAPIds,MAPIds},NPIds,CycleAcc,FitnessAcc,EFAcc,inactive);
		false ->
			agent_logger:log_message(ExoSelf_PId, cortex, Id, cycle_complete, CycleAcc),
			[begin
				PId ! {self(),sync},
				agent_logger:log_message(ExoSelf_PId, cortex, Id, sent, {PId, sync})
			end || PId <- SPIds],
			cortex:loop(Id,ExoSelf_PId,SPIds,{MAPIds,MAPIds},NPIds,CycleAcc+1,FitnessAcc,EFAcc,active)
	end;
loop(Id,ExoSelf_PId,SPIds,{MAPIds,MAPIds},NPIds,_CycleAcc,_FitnessAcc,_EFAcc,inactive)->
	%fx:log(io_lib:format("Cortex:~p is inactive, waiting for reactivation.~n",[Id])),
	receive
		{ExoSelf_PId,reactivate}->
			put(start_time,now()),
			[SPId ! {self(),sync} || SPId <- SPIds],
			cortex:loop(Id,ExoSelf_PId,SPIds,{MAPIds,MAPIds},NPIds,1,0,0,active);
		{ExoSelf_PId,terminate}->
			%io:format("Cortex:~p is terminating.~n",[Id]),
			ok
	end.
%The cortex's goal is to synchronize the the NN system such that when the actuators have received all their control signals, the sensors are once again triggered to gather new sensory information. Thus the cortex waits for the sync messages from the actuator PIds in its system, and once it has received all the sync messages, it triggers the sensors and then drops back to waiting for a new set of sync messages. The cortex stores 2 copies of the actuator PIds: the APIds, and the MemoryAPIds (MAPIds). Once all the actuators have sent it the sync messages, it can restore the APIds list from the MAPIds. Finally, there is also the Step variable which decrements every time a full cycle of Sense-Think-Act completes, once this reaches 0, the NN system begins its termination and backup process.
