
-module(sensor).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,prep,[ExoSelf_PId]).

prep(ExoSelf_PId) ->
	receive 
		{ExoSelf_PId,{Id,Cx_PId,Scape,SensorName,VL,Parameters,Fanout_PIds,OpMode}} ->
			put(opmode,OpMode),
			loop(Id,ExoSelf_PId,Cx_PId,Scape,SensorName,VL,Parameters,Fanout_PIds)
	end.
%When gen/2 is executed it spawns the sensor element and immediately begins to wait for its initial state message.

loop(Id,ExoSelf_PId,Cx_PId,Scape,SensorName,VL,Parameters,Fanout_PIds)->
	receive
		{Cx_PId,sync}->
			agent_logger:log_message(ExoSelf_PId, sensor, Id, received, {Cx_PId, sync}),
			SensoryVector = sensor:SensorName(ExoSelf_PId,VL,Parameters,Scape),
			[begin
				Pid ! {self(),forward,SensoryVector},
				agent_logger:log_message(ExoSelf_PId, sensor, Id, sent, {Pid, forward})
			end || Pid <- Fanout_PIds],
			loop(Id,ExoSelf_PId,Cx_PId,Scape,SensorName,VL,Parameters,Fanout_PIds);
		{ExoSelf_PId,terminate} ->
			agent_logger:log_message(ExoSelf_PId, sensor, Id, received, {ExoSelf_PId, terminate}),
			ok;
		Msg ->
			agent_logger:log_message(ExoSelf_PId, sensor, Id, unexpected, Msg),
			loop(Id,ExoSelf_PId,Cx_PId,Scape,SensorName,VL,Parameters,Fanout_PIds)
	end.
%The sensor process accepts only 2 types of messages, both from the cortex. The sensor can either be triggered to begin gathering sensory data based on its sensory role, or terminate if the cortex requests so.

rng(ExoSelf_PId,VL,_Scape)->
	rng1(VL,[]).
rng1(0,Acc)->
	Acc;
rng1(VL,Acc)-> 
	rng1(VL-1,[random:uniform()|Acc]).
%rng/2 is a simple random number generator that produces a vector of random values, each between 0 and 1. The length of the vector is defined by the VL, which itself is specified within the sensor record.

%This function encodes the Price Chart Input (PCI) sensor data.
% A reconstructed visual representation of price data, similar to a candlestick chart.
% Encoded as a reduced-resolution grid (e.g., 100x20 or 10x10) to preserve geometric patterns.
% The grid uses trinary encoding:
% -1 → Background.
% 0 → Body of candlestick.
% 1 → Key features (e.g., candle wicks, highlights).
	fx_PCI(Exoself_Id,VL,Parameters,Scape)->
		[HRes,VRes] = Parameters,
		case get(opmode) of
			gt	->
				%Normal, assuming we have 10000 rows, we start from 1000 to 200
				Scape ! {self(),sense,config:primary_currency_pair(),close,[HRes,VRes,graph_sensor],config:data_start_index(),config:data_end_index()};
		benchmark ->
			Scape ! {self(),sense,config:primary_currency_pair(),close,[HRes,VRes,graph_sensor],config:data_end_index(),config:benchmark_end_index()};
		live_trading ->
			% In live trading, use offsets relative to the latest bar: start=0 (last), end=1000 (1000 bars window)
			Scape ! {self(),sense,config:primary_currency_pair(),close,[HRes,VRes,graph_sensor],0,config:benchmark_end_index()}
	end,
	receive 
		{_From,Result}->
			%fx:log(io_lib:format("***fx_PCI received: ~p~n", [{_From, Result}])),
			Result
	end.

%This function encodes the Price List Input (PLI) sensor data.
% It retrieves the last 200 rows of the EURUSD1 table and outputs the close prices as a vector.
% The function takes the Exoself_Id, VL (vector length), Parameters (which includes the resolution and type of data), and Scape (the process that handles the data
	fx_PLI(Exoself_Id,VL,Parameters,Scape)->
		[HRes,Type] = Parameters,%Type=open|close|high|low
		case get(opmode) of
			gt	->
				%Normal, assuming we have 10000 rows, we start from 1000 to 200
				Scape ! {self(),sense,config:primary_currency_pair(),close,[HRes,list_sensor],config:data_start_index(),config:data_end_index()};
		benchmark ->

			Scape ! {self(),sense,config:primary_currency_pair(),close,[HRes,list_sensor],config:data_end_index(),config:benchmark_end_index()};
		live_trading ->
			% In live trading, use offsets relative to the latest bar: start=0 (last), end=1000 (1000 bars window)
			Scape ! {self(),sense,config:primary_currency_pair(),close,[HRes,list_sensor],0,config:benchmark_end_index()}
	end,
	receive 
		{_From,Result}->
			%io:format("fx_PLI received: ~p~n", [{_From, Result}]),
			%fx:log(io_lib:format("fx_PLI received: ~p~n", [{_From, Result}])),
			normalize(Result)
	end.
	
	normalize(Vector)->
		Normalizer=math:sqrt(lists:sum([Val*Val||Val<-Vector])),
		[Val/Normalizer || Val <- Vector].

fx_Internals(Exoself_Id,VL,Parameters,Scape)->
	Scape ! {self(),sense,internals,Parameters},
	receive
		{PId,Result}->
			Result
	end.
