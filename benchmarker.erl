
-module(benchmarker).
-compile(export_all).
-include("records.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Benchmark Options %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(DIR,"benchmarks/").
% Standard forex trader morphology (uses EURUSD1 with existing sensor configuration)
% Fixed: Use function call instead of macro for dynamic config
get_init_constraints() ->
    [#constraint{morphology=Morphology,connection_architecture=CA, population_evo_alg_f=config:population_evo_alg_f(), neural_pfns=config:neural_plasticity_functions(),agent_encoding_types=config:agent_encoding_types()} || Morphology<-[config:morphology()],CA<-[config:connection_architecture()]].

% Alternative: Use the new 1-minute optimized morphology (uncomment to use)
%-define(INIT_CONSTRAINTS,[#constraint{morphology=Morphology,connection_architecture=CA, population_evo_alg_f=generational, neural_pfns=[none],agent_encoding_types=[substrate]} || Morphology<-[forex_trader_1m],CA<-[feedforward]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Starts and ends Neural Networks with various preset parameters and options, and polls the logger for information about each run.			
start(Id)->
	PMP = #pmp{
		op_mode=benchmark,
		population_id=test,
		survival_percentage=config:survival_percentage(),
		specie_size_limit=config:specie_size_limit(), %qChangeFrom 10,
		init_specie_size=config:init_specie_size(), %qChangeFrom 10,
		polis_id = mathema,
		generation_limit = inf,
		evaluations_limit = config:evaluations_limit(), %qChangeFrom 10000, optimized from 10
		fitness_goal = inf
	},
	E=#experiment{
		id = Id,
		backup_flag = true,
		pm_parameters=PMP,
		init_constraints=get_init_constraints(),
		progress_flag=in_progress,
		run_index=1,
		tot_runs=config:tot_runs(), %qChangeFrom 20,
		started={date(),time()},
		interruptions=[]
	},
	%io:format("Initial constraints: ~p~n", [E#experiment.init_constraints]), %qq
    %io:format("Experiment record: ~p~n", [E]), %qq
	fx:log("Time:~p, RunIndex:~p, tot_runs:0 - Starting~n",[{date(),time()},E#experiment.tot_runs]),
	genotype:write(E),
	register(benchmarker,spawn(benchmarker,prep,[E])).

continue(Id)->
	case genotype:dirty_read({experiment,Id}) of
		undefined ->
			io:format("Can't continue experiment:~p, it's not present in the database.~n",[Id]);
		E ->
			case E#experiment.progress_flag of
				completed ->
					io:format("Experiment:~p already completed:~p~n",[Id,E#experiment.trace_acc]);
				in_progress ->
					Interruptions = E#experiment.interruptions,
					U_Interruptions = [now()|Interruptions],
					U_E = E#experiment{
						interruptions = U_Interruptions
					},
					genotype:write(U_E),
					register(benchmarker,spawn(benchmarker,prep,[U_E]))
			end
	end.

prep(E)->
	PMP = E#experiment.pm_parameters,
	U_PMP = PMP#pmp{benchmarker_pid=self()},
	Constraints = E#experiment.init_constraints,
	Population_Id = PMP#pmp.population_id,
	population_monitor:prep_PopState(U_PMP,Constraints),
	loop(E#experiment{pm_parameters=U_PMP},Population_Id).
	
loop(E,P_Id)->
	receive
		{P_Id,completed,Trace}->
			U_TraceAcc = [Trace|E#experiment.trace_acc],
			U_RunIndex = E#experiment.run_index+1,
			case U_RunIndex > E#experiment.tot_runs of
				true ->
					U_E = E#experiment{
						trace_acc = U_TraceAcc,
						run_index = U_RunIndex,
						completed = {date(),time()},
						progress_flag = completed
					},
					genotype:write(U_E),
					report(U_E#experiment.id,"report"),
					fx:log("Time:~p, RunIndex:~p, tot_runs:~p - Completed~n",[{date(),time()},U_RunIndex,E#experiment.tot_runs]),
					checkpoint_and_exit();
				false ->
					U_E = E#experiment{
						trace_acc = U_TraceAcc,
						run_index = U_RunIndex
					},
					fx:log("Time:~p, RunIndex:~p, tot_runs:~p~n",[{date(),time()},U_RunIndex,E#experiment.tot_runs]),
					genotype:write(U_E),
					PMP = U_E#experiment.pm_parameters,
					Constraints = U_E#experiment.init_constraints,
					population_monitor:prep_PopState(PMP,Constraints),
					checkpoint(),
					loop(U_E,P_Id)
			end;
		terminate ->
			fx:log("Benchmarker received terminate signal, exiting...~n",[]),
			ok
	end.

report(Experiment_Id,FileName)->
	E = genotype:dirty_read({experiment,Experiment_Id}),
	Traces = E#experiment.trace_acc,
	{ok, File} = file:open(?DIR++FileName++"_Trace_Acc", write),
	lists:foreach(fun(X) -> io:format(File, "~p.~n",[X]) end, Traces),
	file:close(File),
	io:format("******** Traces_Acc written to file:~p~n",[?DIR++FileName++"_Trace_Acc"]),
	Graphs = prepare_Graphs(Traces),
	write_Graphs(Graphs,FileName++"_Graphs"),
	Eval_List = [T#trace.tot_evaluations||T<-Traces],
	io:format("Tot Evaluations Avg:~p Std:~p~n",[functions:avg(Eval_List),functions:std(Eval_List)]).

-record(graph,{morphology,avg_neurons=[],neurons_std=[],avg_fitness=[],fitness_std=[],max_fitness=[],min_fitness=[],avg_diversity=[],diversity_std=[],evaluations=[],evaluation_Index=[]}).
-record(avg,{avg_neurons=[],neurons_std=[],avg_fitness=[],fitness_std=[],max_fitness=[],min_fitness=[],avg_diversity=[],diversity_std=[],evaluations=[]}).

prepare_Graphs(Traces)->
		[T|_] = Traces,
		[Stats_List|_] = T#trace.stats,
		Morphologies = [S#stat.morphology || S<-Stats_List],
		Morphology_Graphs = [prep_Traces(Traces,Morphology,[])|| Morphology <- Morphologies],
		[io:format("Graph:~p~n",[Graph])|| Graph<-Morphology_Graphs],
		Morphology_Graphs.
		
	prep_Traces([T|Traces],Morphology,Acc)->
		Morphology_Trace = lists:flatten([[S||S<-Stats,S#stat.morphology == Morphology]||Stats<-T#trace.stats]),
		prep_Traces(Traces,Morphology,[Morphology_Trace|Acc]);
	prep_Traces([],Morphology,Acc)->
		Graph = avg_MorphologicalTraces(lists:reverse(Acc),[],[],[]),
		Graph#graph{morphology=Morphology}.
		
		avg_MorphologicalTraces([S_List|S_Lists],Acc1,Acc2,Acc3)->
			case S_List of
				[S|STail] ->			
					avg_MorphologicalTraces(S_Lists,[STail|Acc1],[S|Acc2],Acc3);
				[] ->
					Graph = avg_statslists(Acc3,#graph{}),
					Graph
			end;
		avg_MorphologicalTraces([],Acc1,Acc2,Acc3)->
			avg_MorphologicalTraces(lists:reverse(Acc1),[],[],[lists:reverse(Acc2)|Acc3]).
		
			avg_statslists([S_List|S_Lists],Graph)->
				Avg = avg_stats(S_List,#avg{}),
				U_Graph = Graph#graph{
						avg_neurons = [Avg#avg.avg_neurons|Graph#graph.avg_neurons],
						neurons_std = [Avg#avg.neurons_std|Graph#graph.neurons_std],
						avg_fitness = [Avg#avg.avg_fitness|Graph#graph.avg_fitness],
						fitness_std = [Avg#avg.fitness_std|Graph#graph.fitness_std],
						max_fitness = [Avg#avg.max_fitness|Graph#graph.max_fitness],
						min_fitness = [Avg#avg.min_fitness|Graph#graph.min_fitness],
						evaluations = [Avg#avg.evaluations|Graph#graph.evaluations],
						avg_diversity = [Avg#avg.avg_diversity|Graph#graph.avg_diversity],
						diversity_std = [Avg#avg.diversity_std|Graph#graph.diversity_std]
					},
				avg_statslists(S_Lists,U_Graph);
			avg_statslists([],Graph)->
				Graph#graph{
						avg_neurons = lists:reverse(Graph#graph.avg_neurons),
						neurons_std = lists:reverse(Graph#graph.neurons_std),
						avg_fitness = lists:reverse(Graph#graph.avg_fitness),
						fitness_std = lists:reverse(Graph#graph.fitness_std),
						max_fitness = lists:reverse(Graph#graph.max_fitness),
						min_fitness = lists:reverse(Graph#graph.min_fitness),
						evaluations = lists:reverse(Graph#graph.evaluations),
						avg_diversity = lists:reverse(Graph#graph.avg_diversity),
						diversity_std = lists:reverse(Graph#graph.diversity_std)
					}.
				
				avg_stats([S|STail],Avg)->
					U_Avg = Avg#avg{
						avg_neurons = [S#stat.avg_neurons|Avg#avg.avg_neurons],
						%neurons_std = [S#stat.neurons_std|Avg#avg.neurons_std],
						avg_fitness = [S#stat.avg_fitness|Avg#avg.avg_fitness],
						%fitness_std = [S#stat.fitness_std|Avg#avg.fitness_std],
						max_fitness = [S#stat.max_fitness|Avg#avg.max_fitness],
						min_fitness = [S#stat.min_fitness|Avg#avg.min_fitness],
						evaluations = [S#stat.evaluations|Avg#avg.evaluations],
						avg_diversity = [S#stat.avg_diversity|Avg#avg.avg_diversity]
					},
					avg_stats(STail,U_Avg);
				avg_stats([],Avg)->
					Avg#avg{
						avg_neurons=functions:avg(Avg#avg.avg_neurons),
						neurons_std=functions:std(Avg#avg.avg_neurons),
						avg_fitness=functions:avg(Avg#avg.avg_fitness),
						fitness_std=functions:std(Avg#avg.avg_fitness),
						max_fitness=lists:max(Avg#avg.max_fitness),
						min_fitness=lists:min(Avg#avg.min_fitness),
						evaluations=functions:avg(Avg#avg.evaluations),
						avg_diversity=functions:avg(Avg#avg.avg_diversity),
						diversity_std=functions:std(Avg#avg.avg_diversity)
					}.

write_Graphs([G|Graphs],Graph_Postfix)->
	Morphology = G#graph.morphology,
	U_G = G#graph{evaluation_Index=[500*Index || Index <-lists:seq(1,length(G#graph.avg_fitness))]},
	{ok, File} = file:open(?DIR++"graph_"++atom_to_list(Morphology)++"_"++Graph_Postfix, write),
	io:format(File,"#Avg Fitness Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y,Std}) -> io:format(File, "~p ~p ~p~n",[X,Y,Std]) end, lists:zip3(U_G#graph.evaluation_Index,U_G#graph.avg_fitness,U_G#graph.fitness_std)),
	io:format(File,"~n~n#Avg Neurons Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y,Std}) -> io:format(File, "~p ~p ~p~n",[X,Y,Std]) end, lists:zip3(U_G#graph.evaluation_Index,U_G#graph.avg_neurons,U_G#graph.neurons_std)),
	io:format(File,"~n~n#Avg Diversity Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y,Std}) -> io:format(File, "~p ~p ~p~n",[X,Y,Std]) end, lists:zip3(U_G#graph.evaluation_Index,U_G#graph.avg_diversity,U_G#graph.diversity_std)),
	io:format(File,"~n~n#Avg. Max Fitness Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y}) -> io:format(File, "~p ~p~n",[X,Y]) end, lists:zip(U_G#graph.evaluation_Index,U_G#graph.max_fitness)),
	io:format(File,"~n~n#Avg. Min Fitness Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y}) -> io:format(File, "~p ~p~n",[X,Y]) end, lists:zip(U_G#graph.evaluation_Index,U_G#graph.min_fitness)),
	io:format(File,"~n~n#Specie-Population Turnover Vs Evaluations, Morphology:~p~n",[Morphology]),
	lists:foreach(fun({X,Y}) -> io:format(File, "~p ~p~n",[X,Y]) end, lists:zip(U_G#graph.evaluation_Index,U_G#graph.evaluations)),
	file:close(File),
	write_Graphs(Graphs,Graph_Postfix);
write_Graphs([],_Graph_Postfix)->
	ok.
		
unconsult(List)->
	{ok, File} = file:open(?DIR++"alife_benchmark", write),
	lists:foreach(fun(X) -> io:format(File, "~p~n",[X]) end, List),
	file:close(File).
	
gen_plot(Lists)->gen_plot(Lists,[],[],[]).

gen_plot([List|Lists],Acc1,Acc2,Acc3)->
	case List of
		[Val|Rem] ->
			gen_plot(Lists,[Rem|Acc1],[Val|Acc2],Acc3);
		[] ->
			print_plot(500,Acc3)
	end;
gen_plot([],Acc1,Acc2,Acc3)->
	gen_plot(Acc1,[],[],[functions:avg(Acc2)|Acc3]).

	genplot(Lists)->genplot(Lists,[]).
	genplot([L|Lists],Acc)->
		genplot(Lists,[lists:max(L)|Acc]);
	genplot([],Acc)->
		print_plot(0,lists:reverse(Acc)).
		
	print_plot(Index,[Val|List])->
		io:format("~p  ~p~n",[Index,Val]),
		print_plot(Index+500,List);
	print_plot(_Index,[])->
		void.
		
trace2graph(TraceFileName)->
	{ok,Traces} = file:consult(TraceFileName),
	io:format("Traces:~p~n",[Traces]),
	Graphs = prepare_Graphs(Traces),
	write_Graphs(Graphs,TraceFileName++"_Graph").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% AWS Spot Instance Support %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Checkpoint with fsync before shutdown
checkpoint_and_exit() ->
	fx:log("Checkpointing and exiting...~n",[]),
	checkpoint(),
	
	%% Graceful shutdown
    init:stop().

checkpoint() ->
    %% Stop new work (best-effort, fast)
    catch gen_server:call(population_monitor, pause, 500),
    
    %% Sync Mnesia and create backup with timestamp
    catch mnesia:sync_log(),
    catch filelib:ensure_dir("/var/lib/dxnn/checkpoints/"),
    
    Timestamp = integer_to_list(erlang:system_time(second)),
    Backup = "/var/lib/dxnn/checkpoints/checkpoint-" ++ Timestamp ++ ".dmp",
    
    case mnesia:backup(Backup) of
        ok -> 
            %% Copy logs to checkpoint directory
            copy_logs_to_checkpoint("/var/lib/dxnn/checkpoints/checkpoint-" ++ Timestamp),
            ok;
        {error, Reason} ->
            error_logger:error_msg("Backup failed: ~p~n", [Reason])
    end.


% Restore from latest checkpoint (no-op if absent)
maybe_restore() ->
    case filelib:wildcard("/var/lib/dxnn/checkpoints/checkpoint-*.dmp") of
        [] -> 
            error_logger:info_msg("No checkpoint files found~n"),
            ok;
        Files ->
            Latest = lists:last(lists:sort(Files)),
            error_logger:info_msg("Restoring from: ~p~n", [Latest]),
            case mnesia:restore(Latest, [{default_op, recreate_tables}]) of
                {atomic, _} -> 
                    error_logger:info_msg("Restore successful~n"),
                    
                    %% Restore logs from checkpoint
                    restore_logs_from_checkpoint(Latest),
                    
                    ok;
                {aborted, Reason} ->
                    error_logger:error_msg("Restore failed: ~p~n", [Reason]),
                    {error, Reason}
            end
    end.

% Copy logs to checkpoint directory
copy_logs_to_checkpoint(CheckpointDir) ->
    case file:read_file_info("logs/dxnn_run.log") of
        {ok, _} ->
            DestFile = CheckpointDir ++ "/dxnn_run.log",
            file:copy("logs/dxnn_run.log", DestFile), 
			fx:log("Logs copied to checkpoint.~n",[]);
        _ -> ok
    end.

% Restore logs from checkpoint
restore_logs_from_checkpoint(CheckpointFile) ->
    BaseName = filename:basename(CheckpointFile, ".dmp"),
    LogFile = "/var/lib/dxnn/checkpoints/" ++ BaseName ++ "/dxnn_run.log",
    case file:read_file_info(LogFile) of
        {ok, _} ->
            file:copy(LogFile, "logs/dxnn_run.log");
        _ -> ok
    end.

% Completion signal for normal training completion
% Creates completion checkpoint with timestamp and backup file info
% NO IMDS, NO S3, NO HTTP dependencies - minimal DXNN change
completion_signal() ->
    error_logger:info_msg("Training completed - creating completion checkpoint~n"),
    
    %% Sync Mnesia and create completion backup
    catch mnesia:sync_log(),
    catch filelib:ensure_dir("/var/lib/dxnn/checkpoints/"),
    
    Timestamp = integer_to_list(erlang:system_time(second)),
    Backup = "/var/lib/dxnn/checkpoints/completion-" ++ Timestamp ++ ".dmp",
    
    case mnesia:backup(Backup) of
        ok -> 
            %% Create completion metadata
            MetadataFile = "/var/lib/dxnn/checkpoints/completion-" ++ Timestamp ++ ".metadata.json",
            {ok, Fd} = file:open(MetadataFile, [write]),
            io:format(Fd, "{\"timestamp\": ~p, \"backup_file\": ~p, \"type\": \"completion\"}~n", 
                     [Timestamp, Backup]),
            file:sync(Fd),
            file:close(Fd),
            error_logger:info_msg("Completion checkpoint created: ~p~n", [Backup]),
            ok;
        {error, Reason} ->
            error_logger:error_msg("Completion backup failed: ~p~n", [Reason]),
            {error, Reason}
    end.
