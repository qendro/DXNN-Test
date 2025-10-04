-module(fitness_postprocessor).
-compile(export_all).
-include("records.hrl").
-define(EFF,0.01). %Efficiency.

none(Agent_Summaries)->
	lists:reverse(lists:sort(Agent_Summaries)).

size_proportional(Agent_Summaries)->
	SDX=lists:reverse(lists:sort([{Fitness/math:pow(TotN,?EFF),{Fitness,TotN,Agent_Id}}||{Fitness,TotN,Agent_Id}<-Agent_Summaries])),
	ProperlySorted_AgentSummaries = [Val || {_,Val}<-SDX],
	ProperlySorted_AgentSummaries.
