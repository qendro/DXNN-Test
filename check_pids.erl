-module(check_pids).
-compile(export_all).

%% Check if the IDs->PIDs mapping might have dead entries
check() ->
    %% From exoself process, try to see if there are PID lookups that failed
    Exoself = list_to_pid("<0.1038.0>"),
    
    io:format("~n=== Checking Exoself ~p ===~n", [Exoself]),
    case erlang:process_info(Exoself, [dictionary]) of
        undefined ->
            io:format("Exoself is dead~n");
        Info ->
            Dict = proplists:get_value(dictionary, Info),
            io:format("Exoself dictionary:~n"),
            lists:foreach(fun({K,V}) ->
                io:format("  ~p: ~p~n", [K,V])
            end, Dict)
    end,
    ok.

