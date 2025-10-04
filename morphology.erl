
-module(morphology).
-compile(export_all).
-include("records.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get Init Standard Actuators/Sensors %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_InitSensors(Morphology)->
	Sensors = morphology:Morphology(sensors),
	% Only select from PLI and PCI sensors (exclude Internal for now)
	PLI_PCI_Sensors = [S || S <- Sensors, S#sensor.name =/= fx_Internals],
	% Randomly select 1 sensor from PLI or PCI
	RandomSensor = lists:nth(random:uniform(length(PLI_PCI_Sensors)), PLI_PCI_Sensors),
	[RandomSensor].

get_InitActuators(Morphology)->
	Actuators = morphology:Morphology(actuators),
	[lists:nth(1,Actuators)].

get_Sensors(Morphology)->
	morphology:Morphology(sensors).

get_Actuators(Morphology)->
	morphology:Morphology(actuators).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get Init Substrate_CPPs/Substrate_CEPs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_InitSubstrateCPPs(Dimensions,Plasticity)->
	Substrate_CPPs = get_SubstrateCPPs(Dimensions,Plasticity),
	[lists:nth(1,Substrate_CPPs)].

get_InitSubstrateCEPs(Dimensions,Plasticity)->
	Substrate_CEPs = get_SubstrateCEPs(Dimensions,Plasticity),
	[lists:nth(1,Substrate_CEPs)].

get_SubstrateCPPs(Dimensions,Plasticity)->
	io:format("Dimensions:~p, Plasticity:~p~n",[Dimensions,Plasticity]),
	if
		(Plasticity == iterative) or (Plasticity == abcn) ->
			Std=[
				#sensor{name=cartesian,type=substrate,vl=Dimensions*2+3},%{cartesian,Dimensions*2+3},
				#sensor{name=centripital_distances,type=substrate,vl=2+3},%{centripital_distances,2+3},
				#sensor{name=cartesian_distance,type=substrate,vl=1+3},%{cartesian_distance,1+3},
				#sensor{name=cartesian_CoordDiffs,type=substrate,vl=Dimensions+3},%{cartesian_CoordDiffs,Dimensions+3}
				#sensor{name=cartesian_GaussedCoordDiffs,type=substrate,vl=Dimensions+3},%{cartesian_GaussedCoordDiffs,Dimensions+3}
				#sensor{name=iow,type=substrate,vl=3}%{iow,3}
			],
			Adt=case Dimensions of
				2 ->
					[#sensor{name=polar,type=substrate,vl=Dimensions*2+3}];%[{polar,Dimensions*2+3}];
				3 ->
					[#sensor{name=spherical,type=substrate,vl=Dimensions*2+3}];%[{spherical,Dimensions*2+3}]
				_ -> 
					[]
			end,
			lists:append(Std,Adt);
		(Plasticity == none) or (Plasticity == modular_none)->
			Std=[
				#sensor{name=cartesian,type=substrate,vl=Dimensions*2},%{cartesian,Dimensions*2},
				#sensor{name=centripital_distances,type=substrate,vl=2},%{centripital_distances,2},
				#sensor{name=cartesian_distance,type=substrate,vl=1},%{cartesian_distance,1},
				#sensor{name=cartesian_CoordDiffs,type=substrate,vl=Dimensions},%{cartesian_CoordDiffs,Dimensions+3}
				#sensor{name=cartesian_GaussedCoordDiffs,type=substrate,vl=Dimensions}%{cartesian_GaussedCoordDiffs,Dimensions+3}
			],
			Adt=case Dimensions of
				2 ->
					[#sensor{name=polar,type=substrate,vl=Dimensions*2}];%[{polar,Dimensions*2}];
				3 ->
					[#sensor{name=spherical,type=substrate,vl=Dimensions*2}];%[{spherical,Dimensions*2}]
				_ -> 
					[]
			end,
			lists:append(Std,Adt)
	end.

get_SubstrateCEPs(Dimensions,Plasticity)->
	case Plasticity of
		iterative ->
			[#actuator{name=delta_weight,type=substrate,vl=1}]; %[{delta_weight,1}];
		abcn ->
			[#actuator{name=set_abcn,type=substrate,vl=5}]; %[{abcn,4}];
		none ->
			[#actuator{name=set_weight,type=substrate,vl=1}]; %[{weight,1}]
		modular_none ->
			[#actuator{name=weight_expression,type=substrate,vl=2}] %[{weight_conexpr,2}]
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MORPHOLOGIES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	This sets up the link so that when the neural network outputs a value, it calls fx_Trade to send that trade to the Forex simulator.
forex_trader(actuators)->
	[
		#actuator{name=fx_Trade,type=standard,scape={private,fx_sim},format=no_geo,vl=1,parameters=[]}
	];
% Defines the sensors, which provide input data to the agent's neural network.
% 1. PLI Sensor — Price List Input
% 	Provides a sliding window of raw price data with resolution HRes = 10
% 	Format is simple numerical (no geometric structure)
% 	Parameters: window size (HRes) and which price to use (close)
% 2. PCI Sensor — Price Chart Input
% 	Provides reconstructed chart images with horizontal resolution HRes = 50 and vertical VRes = 20
% 	format = symmetric indicates spatial (geometric) structure for substrate encoding
% 	vl = 50 * 20 = 1000 total inputs for this chart
forex_trader(sensors)->
	PLI_Sensors=  [#sensor{name=fx_PLI,type=standard,scape={private,fx_sim},format=no_geo,vl=HRes,parameters=[HRes,close]} || HRes<-config:pli_resolutions()],
	PCI_Sensors = [#sensor{name=fx_PCI,type=standard,scape={private,fx_sim},format={symmetric,[HRes,VRes]},vl=HRes*VRes,parameters=[HRes,VRes]} || HRes <-config:pci_horizontal_resolutions(), VRes<-config:pci_vertical_resolutions()],
	InternalSensors = [#sensor{name=fx_Internals,type=standard,scape={private,fx_sim},format=no_geo,vl=config:internal_sensor_dimensions(),parameters=[config:internal_sensor_dimensions()]}],%[Long|Short|Void],Value
	%PCI_Sensors. %Inital state [BASE]
	%PLI_Sensors++PCI_Sensors++InternalSensors.
	%PLI_Sensors.%++InternalSensors. %qq
	PLI_Sensors. % qq

% New morphology specifically optimized for 1-minute forsex trading
% Uses smaller time windows and higher resolution for quick decision making
forex_trader_1m(actuators)->
	[
		#actuator{name=fx_Trade,type=standard,scape={private,fx_sim},format=no_geo,vl=1,parameters=[]}
	];

forex_trader_1m(sensors)->
	% Smaller time windows for 1-minute data
	PLI_Sensors=[#sensor{name=fx_PLI,type=standard,scape={private,fx_sim},format=no_geo,vl=HRes,parameters=[HRes,close]} || HRes<-config:pli_1m_resolutions()], % Multiple resolutions
	PCI_Sensors = [#sensor{name=fx_PCI,type=standard,scape={private,fx_sim},format={symmetric,[HRes,VRes]},vl=HRes*VRes,parameters=[HRes,VRes]} || HRes <-config:pci_1m_horizontal_resolutions(), VRes<-config:pci_1m_vertical_resolutions()], % Smaller charts for faster processing
	InternalSensors = [#sensor{name=fx_Internals,type=standard,scape={private,fx_sim},format=no_geo,vl=config:internal_sensor_dimensions(),parameters=[config:internal_sensor_dimensions()]}],%[Long|Short|Void],Value
	PLI_Sensors++PCI_Sensors++InternalSensors. % Include all sensor types for 1-minute trading
	
generate_id() ->
	{MegaSeconds,Seconds,MicroSeconds} = now(), 
	1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).
