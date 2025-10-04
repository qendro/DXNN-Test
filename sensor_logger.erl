-module(sensor_logger).
-compile(export_all).
-include("records.hrl").

%% Logs sensor mutations for debugging and tracking purposes
log_sensor_mutation(Agent_Id, SIds) ->
	Sensor_Names = [get_sensor_name(SId) || SId <- SIds],
	Sensor_Summary = categorize_sensors(Sensor_Names),
	fx:log(io_lib:format("MUTATION: Agent ~p now using sensors: ~s~n", [Agent_Id, Sensor_Summary])).

get_sensor_name(SId) ->
	S = genotype:dirty_read({sensor, SId}),
	S#sensor.name.

categorize_sensors(Sensor_Names) ->
	PLI_Count = length([Name || Name <- Sensor_Names, Name =:= fx_PLI]),
	PCI_Count = length([Name || Name <- Sensor_Names, Name =:= fx_PCI]),
	Internal_Count = length([Name || Name <- Sensor_Names, Name =:= fx_Internals]),
	
	Parts = lists:filter(fun(Part) -> Part =/= "" end, [
		case PLI_Count of 0 -> ""; 1 -> "PLI"; _ -> io_lib:format("PLI(~p)", [PLI_Count]) end,
		case PCI_Count of 0 -> ""; 1 -> "PCI"; _ -> io_lib:format("PCI(~p)", [PCI_Count]) end,
		case Internal_Count of 0 -> ""; 1 -> "Internal"; _ -> io_lib:format("Internal(~p)", [Internal_Count]) end
	]),
	
	case Parts of
		[] -> "None";
		_ -> string:join(Parts, " & ")
	end.
