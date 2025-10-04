-module(launcher).
-export([start/0,start/1]).

-include("records.hrl").

-define(FX_TABLES, [metadata,'EURUSD1','EURUSD1_LIVE']).
-define(FX_READY_TIMEOUT, 10000).
-define(FX_POLL_INTERVAL, 100).

-define(POLIS_TABLE_SPECS, [
    {population, record_info(fields, population)},
    {specie, record_info(fields, specie)},
    {agent, record_info(fields, agent)},
    {cortex, record_info(fields, cortex)},
    {neuron, record_info(fields, neuron)},
    {sensor, record_info(fields, sensor)},
    {actuator, record_info(fields, actuator)},
    {substrate, record_info(fields, substrate)},
    {experiment, record_info(fields, experiment)}
]).

start() ->
    start(sliding_window_5).

start(BenchmarkId) ->
    ok = ensure_compiled(),
    fx:log("Compilation checked"),
    ok = ensure_fx_ready(),
    fx:log("FX ready"),
    ok = ensure_polis_ready(),
    fx:log("Polis ready"),
    ok = sync_code(),
    fx:log("Code synchronized"),
    benchmarker:start(BenchmarkId).

ensure_compiled() ->
    case make:all([load]) of
        up_to_date -> ok;
        error -> erlang:error({compile_failed, make_all_failed});
        {error, Reason} -> erlang:error({compile_failed, Reason})
    end.

ensure_fx_ready() ->
    ok = ensure_fx_stopped(),
    ok = fx:clear_log(),
    ok = fx:init(),
    ok = ensure_fx_started(),
    ok.

ensure_fx_stopped() ->
    case whereis(fx) of
        undefined -> ok;
        Pid ->
            Ref = erlang:monitor(process, Pid),
            Pid ! stop,
            receive
                {'DOWN', Ref, process, Pid, _Reason} -> ok
            after 5000 ->
                erlang:error({fx_stop_timeout, Pid})
            end
    end.

ensure_fx_started() ->
    case whereis(fx) of
        undefined -> fx:start();
        _Pid -> ok
    end,
    wait_for_fx_tables(?FX_TABLES, ?FX_READY_TIMEOUT).

wait_for_fx_tables(_Tables, Timeout) when Timeout =< 0 ->
    erlang:error({fx_start_timeout, ?FX_TABLES});
wait_for_fx_tables(Tables, Timeout) ->
    case lists:all(fun table_loaded/1, Tables) of
        true -> ok;
        false ->
            timer:sleep(?FX_POLL_INTERVAL),
            wait_for_fx_tables(Tables, Timeout - ?FX_POLL_INTERVAL)
    end.

table_loaded(Table) ->
    case ets:info(Table) of
        undefined -> false;
        _ -> true
    end.

ensure_polis_ready() ->
    ok = ensure_mnesia(),
    ok = ensure_polis_tables(),
    ok = ensure_polis_started(),
    ok.

ensure_mnesia() ->
    case catch mnesia:system_info(is_running) of
        yes -> ok;
        _ -> start_mnesia()
    end.

start_mnesia() ->
    case mnesia:start() of
        ok -> ok;
        {error, {already_started, _Node}} -> ok;
        {error, {no_exists, schema}} ->
            ok = mnesia:create_schema([node()]),
            ok = mnesia:start(),
            ok;
        {error, Reason} -> erlang:error({mnesia_start_failed, Reason})
    end.

ensure_polis_tables() ->
    lists:foreach(fun ensure_table/1, ?POLIS_TABLE_SPECS),
    ok.

ensure_table({Table, Attrs}) ->
    Options = [{disc_copies, [node()]}, {type, set}, {attributes, Attrs}],
    case mnesia:create_table(Table, Options) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, Table}} -> ok;
        {aborted, {already_exists, {Table, _}}} -> ok;
        Other -> erlang:error({create_table_failed, Table, Other})
    end.

ensure_polis_started() ->
    case whereis(polis) of
        undefined ->
            case polis:start() of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok;
                Other -> erlang:error({polis_start_failed, Other})
            end;
        _Pid -> ok
    end.

sync_code() ->
    case polis:sync() of
        up_to_date -> ok;
        error -> erlang:error({sync_failed, make_all_failed});
        {error, Reason} -> erlang:error({sync_failed, Reason});
        _ -> ok
    end.
