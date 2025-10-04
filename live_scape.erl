%% Live Scape - Per-Pair LIVE ETS Tables Using fx #technical Schema
%% - Python handles ALL IB operations.
%% - Erlang stores bars as #technical in per-pair tables: 'EURUSD1_LIVE', 'EURJPY1_LIVE', etc.
%% - Sensors return the same list/graph encodings as fx.erl.

-module(live_scape).

-include("records.hrl").

%% Public API for neural network compatibility
-export([start_link/0, gen/2, prep/1, init_state/5, sense/2, lookup/2, next/2, prev/4,
         trade/3, init_scape/0]).
%% Internal exports
-export([python_port_loop/1]).

%% Default horizontal resolution for readiness init
-define(DEFAULT_HRES, 100).


%% fx.erl compatibility record
-record(state, {table_name, feature, index_start, index_end, index, price_list = []}).

%% ============================================================================
%% PUBLIC API - Neural Network Compatibility
%% ============================================================================

start_link() ->
    Pid = spawn_link(?MODULE, init_scape, []),
    %% Register under the module name to ease future renames
    register(?MODULE, Pid),
    {ok, Pid}.

init_scape() ->
    %% Start Python service port
    PythonPort = start_python_service(),

    %% Wait for initial data readiness
    %% wait_for_readiness(),  % Commented out for testing

    %% Enter main loop
    receive
        {ExoSelfPid, live_sim} ->
            %fx:log(io_lib:format("Starting live simulation for: ~p~n", [ExoSelfPid])),
            live_sim(ExoSelfPid, PythonPort)
    end.

%% Legacy pattern for neural network compatibility
gen(ExoSelfPid, Node) ->
    spawn(Node, ?MODULE, prep, [ExoSelfPid]).

prep(ExoSelfPid) ->
    receive
        {ExoSelfPid, Name} ->
            ?MODULE:Name(ExoSelfPid)
    end.

%% ============================================================================
%% MAIN LOOP - Neural Network Interface
%% ============================================================================

live_sim(ExoSelfPid, PythonPort) ->
    %% Default live table from config (e.g., 'EURUSD1_LIVE')
    DefaultTable = config:primary_currency_pair(),
    %fx:log(io_lib:format("Ensure Live Table: ~p~n", [DefaultTable])),
    ensure_live_table(DefaultTable),
    %fx:log("Live table ensured."),
    LiveState = #live_state{table_name = DefaultTable},
    State = #python_state{live_state = LiveState, python_port = PythonPort},
    loop(ExoSelfPid, State).

loop(ExoSelfPid, State = #python_state{}) ->
    receive
        %% Neural network sensor requests (maintain exact compatibility)
        {From, sense, TableName, Feature, Parameters, _Start, _Finish} ->
            %fx:log(io_lib:format("Received sense request from Exoself: ~p, Table: ~p, Feature: ~p, Parameters: ~p~n",[From, TableName, Feature, Parameters])),
            {Result, NewState} = handle_sense(TableName, Feature, Parameters, State),
            %fx:log(io_lib:format("Result: ~p, NewState: ~p~n", [Result, NewState])),
            %fx:log(io_lib:format("recieved sense request: ~p ~p ~p~n", [TableName, Feature, Parameters])),
            %% Code for getting the data from the live table
            From ! {self(), Result},
            %fx:log("Sent sense response."),
            loop(ExoSelfPid, NewState);
        {From, sense, internals, _Params} ->
            LiveState = State#python_state.live_state,
            From
            ! {self(),
               [LiveState#live_state.current_position,
                LiveState#live_state.entry_price,
                LiveState#live_state.previous_pc]},
            loop(ExoSelfPid, State);
        %% Neural network trade requests
        {From, trade, _TableName, TradeSignal} ->
            {Fitness, Halt, NewState} = handle_trade(TradeSignal, State),
            From ! {self(), Fitness, Halt},
            loop(ExoSelfPid, NewState);
        %% Python service messages
        {python_data, OhlcBar} ->
            NewState1 = handle_ohlc_bar(OhlcBar, State),
            loop(ExoSelfPid, NewState1);
        {python_trade_confirmation, TradeResult} ->
            NewState = handle_trade_confirmation(TradeResult, State),
            loop(ExoSelfPid, NewState);
        terminate ->
            cleanup_python_service(State#python_state.python_port),
            ok
    after 5000 ->
        %% Periodic health check
        loop(ExoSelfPid, State)
    end.

%% ============================================================================
%% NEURAL NETWORK SENSOR INTERFACE (100% Compatibility)
%% ============================================================================

handle_sense(TableName, _Feature, Parameters, State) ->
    %fx:log(io_lib:format("handle_sense called: ~p ~p ~p~n", [TableName, _Feature, Parameters])),
    Table = resolve_table(TableName, State),
    %fx:log(io_lib:format("Resolved table: ~p~n", [Table])),
    case Parameters of
        [HRes, list_sensor] ->
            %fx:log(io_lib:format("Handling list_sensor with HRes: ~p~n", [HRes])),
            {PriceList, NewState} = get_price_list(Table, HRes, State),
            {[Close || {_O, Close, _H, _L} <- PriceList], NewState};
        [HRes, VRes, graph_sensor] ->
            %fx:log(io_lib:format("Handling graph_sensor with HRes: ~p, VRes: ~p~n", [HRes, VRes])),
            {PriceList, NewState} = get_price_list(Table, HRes, State),
            {encode_to_plane(HRes, VRes, PriceList), NewState};
        _ ->
            {[], State}
    end.

get_price_list(Table, HRes, State) ->
    LiveState = State#python_state.live_state,
    ensure_live_table(Table),
    %fx:log(io_lib:format("Getting price list from table: ~p with HRes: ~p~n", [Table, HRes])),

    %% LastSent gating inside this function (non-blocking, mailbox-friendly)
    LastSentKey = {last_sent, Table, HRes},
    %fx:log(io_lib:format("LastSentKey: ~p~n", [LastSentKey])),  
    LastSent = get(LastSentKey),
    %fx:log(io_lib:format("LastSent: ~p~n", [LastSent])),

    Wait = fun Loop(StateAcc) ->
        %fx:log(io_lib:format("Entering Wait loop..., StateAcc: ~p~n", [StateAcc])),
        Size = ets:info(Table, size),
        %fx:log(io_lib:format("all Ets tables: ~p~n", [ets:all()])),
        %fx:log(io_lib:format("Table size: ~p, Table: ~p~n", [Size, Table])),
        EnoughBars = (is_integer(Size) andalso Size >= HRes),
        %fx:log(io_lib:format("EnoughBars: ~p~n", [EnoughBars])),
        {EnoughNew, LastKeyNow} =
            case ets:last(Table) of
                '$end_of_table' -> {false, '$end_of_table'};
                LK ->
                    case LastSent of
                        undefined -> {true, LK};          %% initial fill once we have EnoughBars
                        _ -> {LK > LastSent, LK}          %% fire when one new bar arrives
                    end
            end,

        GateOk = EnoughBars andalso EnoughNew,
        case GateOk of
            true  -> StateAcc;
            false ->
                receive
                    {python_data, OhlcData} ->
                        Loop(handle_ohlc_bar(OhlcData, StateAcc));
                    {python_trade_confirmation, TradeResult} ->
                        Loop(handle_trade_confirmation(TradeResult, StateAcc))
                after 30000 ->
                        Loop(StateAcc)
                end
        end
        end,

    ReadyState = Wait(State),

    %% Fetch latest bars from ETS
    %fx:log(io_lib:format("Fetching last ~p bars from table ~p~n", [HRes, Table])),
    LastKey = ets:last(Table),
    %fx:log(io_lib:format("Last key in table ~p is ~p~n", [Table, LastKey])),
    PriceList =
        case LastKey of
            '$end_of_table' ->
                lists:duplicate(HRes, {1.0, 1.0, 1.0, 1.0});
            _ ->
                collect_recent_bars(Table, LastKey, HRes, [])
        end,

    %% Update LastSent to the most recent key we used
    put(LastSentKey, LastKey),

    NewLiveState =
        (ReadyState#python_state.live_state)#live_state{price_list =
                             lists:keystore(HRes,
                                            2,
                                            (ReadyState#python_state.live_state)#live_state.price_list,
                                            {PriceList, HRes})},
    NewState = ReadyState#python_state{live_state = NewLiveState},
    {PriceList, NewState}.

collect_recent_bars(_Table, '$end_of_table', _Remaining, Acc) ->
    lists:reverse(Acc);
collect_recent_bars(_Table, _Key, 0, Acc) ->
    lists:reverse(Acc);
collect_recent_bars(Table, Key, Remaining, Acc) ->
    case ets:lookup(Table, Key) of
        [#technical{open = O,
                    high = H,
                    low = L,
                    close = C}] ->
            NewAcc = [{O, C, H, L} | Acc],
            PrevKey = ets:prev(Table, Key),
            collect_recent_bars(Table, PrevKey, Remaining - 1, NewAcc);
        [] ->
            %% Skip missing entry
            PrevKey = ets:prev(Table, Key),
            collect_recent_bars(Table, PrevKey, Remaining, Acc)
    end.

encode_to_plane(HRes, VRes, PriceList) ->
    case PriceList of
        [] ->
            lists:duplicate(HRes * VRes, -1);
        _ ->
            Highs = [H || {_O, _C, H, _L} <- PriceList],
            Lows = [L || {_O, _C, _H, L} <- PriceList],
            MaxPrice = lists:max(Highs),
            MinPrice = lists:min(Lows),
            Padding = abs(MaxPrice - MinPrice) / 20,
            VMax = MaxPrice + Padding,
            VMin = MinPrice - Padding,
            VStep = (VMax - VMin) / VRes,
            V0 = VMin + VStep / 2,
            encode_plane_rows(HRes * VRes, PriceList, V0, VStep, [])
    end.

encode_plane_rows(0, _PriceList, _VPos, _VStep, Acc) ->
    lists:reverse(Acc);
encode_plane_rows(N, [{O, C, H, L} | Rest], VPos, VStep, Acc) ->
    {BodyHi, BodyLo} =
        if O > C ->
               {O, C};
           true ->
               {C, O}
        end,
    Val = case VPos + VStep / 2 > BodyLo andalso VPos - VStep / 2 =< BodyHi of
              true ->
                  1;  % Body
              false ->
                  case VPos + VStep / 2 > L andalso VPos - VStep / 2 =< H of
                      true ->
                          0;  % Wick
                      false ->
                          -1  % Empty
                  end
          end,
    encode_plane_rows(N - 1, Rest, VPos, VStep, [Val | Acc]);
encode_plane_rows(N, [], VPos, VStep, Acc) ->
    encode_plane_rows(N - 1, [], VPos + VStep, VStep, [-1 | Acc]).

%% ============================================================================
%% TRADE SIGNAL TRANSMISSION TO PYTHON
%% ============================================================================

handle_trade(TradeSignal, State = #python_state{}) ->
    LiveState = State#python_state.live_state,
    Position = LiveState#live_state.current_position,
    case {Position, TradeSignal} of
        {0, 1} ->
            open_position(1, State);
        {0, -1} ->
            open_position(-1, State);
        {1, 0} ->
            close_position(State);
        {-1, 0} ->
            close_position(State);
        {1, -1} ->
            close_then_open(-1, State);
        {-1, 1} ->
            close_then_open(1, State);
        _ ->
            {0, 0, State}  % No change
    end.

open_position(Signal, State) ->
    Symbol = "EUR.USD",  % Default symbol
    Action =
        case Signal of
            1 -> "BUY";
            -1 -> "SELL"
        end,
    %% IB FX expects quantity in base currency units (e.g., 10_000 = 0.1 lot)
    Quantity = 1000,  % Smaller size: 0.01 lot for testing

    %% Send trade signal to Python service
    TradeMessage =
        #{type => trade_signal,
          symbol => Symbol,
          action => Action,
          quantity => Quantity,
          signal => Signal},
    send_to_python(TradeMessage, State#python_state.python_port),

    %% Return immediately - confirmation will come via separate message
    {0, 0, State}.

close_position(State) ->
    Symbol = "EUR.USD",  % Default symbol

    %% Send close signal to Python service
    TradeMessage =
        #{type => trade_signal,
          symbol => Symbol,
          action => "CLOSE",
          quantity => 0,
          signal => 0},
    send_to_python(TradeMessage, State#python_state.python_port),

    %% Return immediately - confirmation will come via separate message
    {0, 0, State}.

close_then_open(NewSignal, State) ->
    {_P1, _H1, State1} = close_position(State),
    open_position(NewSignal, State1).

handle_trade_confirmation(TradeResult, State) ->
    %% Update state based on trade confirmation from Python
    case maps:get(status, TradeResult, error) of
        success ->
            LiveState = State#python_state.live_state,
            Position = maps:get(position, TradeResult, 0),
            EntryPrice = maps:get(entry_price, TradeResult, 0.0),
            PnL = maps:get(pnl, TradeResult, 0.0),

            NewLiveState = LiveState#live_state{current_position = Position,
                                               entry_price = EntryPrice,
                                               realized_pnl = LiveState#live_state.realized_pnl + PnL},
            State#python_state{live_state = NewLiveState};
        _ ->
            State
    end.

%% ============================================================================
%% PYTHON SERVICE COMMUNICATION
%% ============================================================================

start_python_service() ->
    %% Start Python ib_service.py as external port in Docker environment
    %% Docker runs with --network host so Python can reach IB TWS on host
    PythonCmd = "python3 priv/ib_service.py",
    %% Important: do NOT merge stderr into stdout when using {packet,4}.
    %% Any non-framed log output would corrupt the packet stream.
    Port = open_port({spawn, PythonCmd}, [binary, {packet, 4}]),

    %% Start message handler
    HandlerPid = spawn_link(?MODULE, python_port_loop, [Port]),
    %% Ensure the handler receives all port data
    erlang:port_connect(Port, HandlerPid),
    register(python_port_handler, HandlerPid),
    Port.

python_port_loop(Port) ->
    receive
        {Port, {data, Data}} ->
            try
                Message = parse_json_simple(Data),
                handle_python_message(Message),
                python_port_loop(Port)
            catch
                _:Error ->
                    io:format("JSON parse error: ~p~n", [Error]),
                    python_port_loop(Port)
            end;
        {Port, closed} ->
            io:format("Python port closed~n"),
            ok;
        {send_message, Message} ->
            try
                %io:format("[Erlang->Python] send: ~p~n", [Message]),
                Data = encode_json_simple(Message),
                port_command(Port, Data),
                python_port_loop(Port)
            catch
                _:Error ->
                    io:format("JSON encode error: ~p~n", [Error]),
                    python_port_loop(Port)
            end;
        stop ->
            port_close(Port)
    end.

handle_python_message(Message) ->
    Type = maps:get(<<"type">>, Message, undefined),
    case Type of
        <<"ohlc_bar">> ->
            OhlcData = maps:get(<<"data">>, Message, #{}),
            %% Route to the registered process safely using ?MODULE
            %fx:log(io_lib:format("[Python->Erlang] ohlc_bar: ~p~n", [OhlcData])),
            case whereis(?MODULE) of
                undefined -> ok;
                Pid -> Pid ! {python_data, OhlcData}
            end;
        <<"trade_confirmation">> ->
            TradeData = maps:get(<<"data">>, Message, #{}),
            case whereis(?MODULE) of
                undefined -> ok;
                Pid -> Pid ! {python_trade_confirmation, TradeData}
            end;
        <<"heartbeat">> ->
            ok;
        _ ->
            ok
    end.

send_to_python(Message, _Port) ->
    case whereis(python_port_handler) of
        undefined ->
            io:format("Warning: Python port handler not found~n");
        Pid ->
            Pid ! {send_message, Message}
    end.

cleanup_python_service(Port) ->
    case Port of
        undefined ->
            ok;
        _ ->
            port_close(Port)
    end.

%% ============================================================================
%% CANONICAL OHLC BAR RECEPTION AND STORAGE
%% ============================================================================

handle_ohlc_bar(OhlcData) ->
    io:format("handle_ohlc_bar called: ~p~n", [OhlcData]),
    %% Extract canonical OHLC data
    SymbolBin = maps:get(<<"symbol">>, OhlcData, <<"EUR.USD">>),
    TOpenBin = maps:get(<<"t_open">>, OhlcData, <<>>),
    O = maps:get(<<"o">>, OhlcData, 0.0),
    H = maps:get(<<"h">>, OhlcData, 0.0),
    L = maps:get(<<"l">>, OhlcData, 0.0),
    C = maps:get(<<"c">>, OhlcData, 0.0),
    Vol = maps:get(<<"vol">>, OhlcData, 0),

    %% Determine ETS table name (e.g., 'EURUSD1_LIVE') and ensure table exists
    Table = symbol_to_live_table(SymbolBin),
    ensure_live_table(Table),

    %% Build technical record with parsed ISO timestamp as Id
    TOpenStr = binary_to_list(TOpenBin),
    Id = case parse_iso_utc(TOpenStr) of
             {ok, {{Y,Mo,D},{Hh,Mi,Ss}}} -> {Y,Mo,D,Hh,Mi,Ss,1};
             error -> {0,0,0,0,0,0,1}
         end,
    Rec = #technical{id = Id, open = O, high = H, low = L, close = C, volume = Vol},
    log_ohlc_bar(Id, O, H, L, C, Vol),
    %fx:log(io_lib:format("live_scape: Inserting into ~p: ~p~n", [Table, Rec])),
    ets:insert(Table, Rec).

%% New arity that also updates live state (previous_pc, unrealized_pnl)
handle_ohlc_bar(OhlcData, State = #python_state{live_state = LS0}) ->
    %% Extract canonical OHLC data
    SymbolBin = maps:get(<<"symbol">>, OhlcData, <<"EUR.USD">>),
    TOpenBin = maps:get(<<"t_open">>, OhlcData, <<>>),
    O = maps:get(<<"o">>, OhlcData, 0.0),
    H = maps:get(<<"h">>, OhlcData, 0.0),
    L = maps:get(<<"l">>, OhlcData, 0.0),
    C = maps:get(<<"c">>, OhlcData, 0.0),
    Vol = maps:get(<<"vol">>, OhlcData, 0),

    %% Determine ETS table name (e.g., 'EURUSD1_LIVE') and ensure table exists
    Table = symbol_to_live_table(SymbolBin),
    ensure_live_table(Table),

    %% Build technical record with parsed ISO timestamp as Id
    TOpenStr = binary_to_list(TOpenBin),
    Id = case parse_iso_utc(TOpenStr) of
             {ok, {{Y,Mo,D},{Hh,Mi,Ss}}} -> {Y,Mo,D,Hh,Mi,Ss,1};
             error -> {0,0,0,0,0,0,1}
         end,
    Rec = #technical{id = Id, open = O, high = H, low = L, close = C, volume = Vol},
    log_ohlc_bar(Id, O, H, L, C, Vol),
    %fx:log(io_lib:format("live_scape2: Inserting into ~p: ~p~n", [Table, Rec])),
    ets:insert(Table, Rec),

    %% Update internals when in position
    InPos  = (LS0#live_state.current_position =/= 0),
    Entry  = LS0#live_state.entry_price,
    Qty    = case LS0#live_state.position_qty of undefined -> 0; Q -> Q end,
    NewLS = case InPos andalso Entry > 0 of
        true ->
            PC  = ((C - Entry) / Entry) * 100,
            UPL = (LS0#live_state.current_position) * (C - Entry) * Qty,
            LS0#live_state{previous_pc = PC, unrealized_pnl = UPL, table_name = Table};
        false ->
            LS0#live_state{table_name = Table}
    end,
    State#python_state{live_state = NewLS}.

%% ============================================================================
%% Data Ready? Check 
%% ============================================================================



wait_for_readiness() ->
    %% Wait for ">= MinBars AND last_bar_age < MaxAgeSeconds" on default live table
    MinBars = 10,
    MaxAgeSeconds = 300,  % 5 minutes
    DefaultTable = config:primary_currency_pair(),
    wait_for_readiness_loop(DefaultTable, MinBars, MaxAgeSeconds, 30).  % 30 attempts

wait_for_readiness_loop(_Table, _MinBars, _MaxAgeSeconds, 0) ->
    ok;  % give up
wait_for_readiness_loop(Table, MinBars, MaxAgeSeconds, Attempts) ->
    case check_readiness(Table, MinBars, MaxAgeSeconds) of
        true -> ok;
        false ->
            timer:sleep(1000),
            wait_for_readiness_loop(Table, MinBars, MaxAgeSeconds, Attempts - 1)
    end.

check_readiness(Table, MinBars, _MaxAgeSeconds) ->
    case ets:info(Table, size) of
        undefined -> false;
        Size when is_integer(Size), Size < MinBars -> false;
        _ -> true
    end.

check_bar_age(TOpenStr, MaxAgeSeconds) ->
    case parse_iso_utc(TOpenStr) of
        {ok, DT} ->
            Now = calendar:universal_time(),
            AgeSecs = calendar:datetime_to_gregorian_seconds(Now)
                      - calendar:datetime_to_gregorian_seconds(DT),
            AgeSecs =< MaxAgeSeconds;
        error ->
            false
    end.

%% Accepts "YYYY-MM-DDTHH:MM:SS" optionally ending with "Z" or ".sssZ"
parse_iso_utc(Str0) when is_list(Str0) ->
    parse_iso_utc(list_to_binary(Str0));
parse_iso_utc(Bin) when is_binary(Bin) ->
    %% Trim to first 19 chars if longer (ignore millis and TZ)
    Safe =
        case byte_size(Bin) of
            N when N >= 19 -> binary:part(Bin, 0, 19);
            _ -> Bin
        end,
    case binary_to_list(Safe) of
        [Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2,$T,H1,H2,$:,N1,N2,$:,S1,S2] ->
            try
                Y = list_to_integer([Y1,Y2,Y3,Y4]),
                Mo = list_to_integer([M1,M2]),
                D  = list_to_integer([D1,D2]),
                H  = list_to_integer([H1,H2]),
                Mi = list_to_integer([N1,N2]),
                S  = list_to_integer([S1,S2]),
                {ok, {{Y,Mo,D},{H,Mi,S}}}
            catch _:_ ->
                error
            end;
        _ ->
            error
    end.

%% ============================================================================
%% FX.ERL COMPATIBILITY SHIMS (Maintain 100% Compatibility)
%% ============================================================================

init_state(S, TableName, Feature, live_data, live_data) ->
    Table = resolve_table(TableName, #python_state{live_state = #live_state{table_name = config:primary_currency_pair()}}),
    ensure_live_table(Table),
    IndexEnd = ets:last(Table),
    {IndexStart, IndexEnd1} =
        case IndexEnd of
            '$end_of_table' ->
                {undefined, undefined};
            _ ->
                {find_start_index(Table, IndexEnd, ?DEFAULT_HRES - 1), IndexEnd}
        end,
    S#state{table_name = Table,
            feature = Feature,
            index_start = IndexStart,
            index_end = IndexEnd1,
            index = IndexStart,
            price_list = []}.

sense(S = #state{}, Parameters) ->
    Table = S#state.table_name,
    case Parameters of
        [HRes, list_sensor] ->
            {PriceList, _} = get_price_list(Table, HRes, #python_state{live_state = #live_state{}}),
            {[C || {_O, C, _H, _L} <- PriceList], S};
        [HRes, VRes, graph_sensor] ->
            {PriceList, _} = get_price_list(Table, HRes, #python_state{live_state = #live_state{}}),
            {encode_to_plane(HRes, VRes, PriceList), S};
        _ ->
            {[], S}
    end.

lookup(Table, Index) ->
    case ets:lookup(Table, Index) of
        [Record] -> Record;
        [] -> undefined
    end.

next(Table, Index) ->
    ets:next(Table, Index).

prev(Table, Current, prev, Count) ->
    step_prev(Table, Current, Count);
prev(Table, Current, next, Count) ->
    step_next(Table, Current, Count).

step_prev(_Table, Index, 0) -> Index;
step_prev(Table, Index, N) ->
    case ets:prev(Table, Index) of
        '$end_of_table' -> Index;
        PrevIndex -> step_prev(Table, PrevIndex, N - 1)
    end.

step_next(_Table, Index, 0) -> Index;
step_next(Table, Index, N) ->
    case ets:next(Table, Index) of
        '$end_of_table' -> Index;
        NextIndex -> step_next(Table, NextIndex, N - 1)
    end.

trade(_TableName, TradeSignal, LiveState) ->
    handle_trade(TradeSignal, LiveState).

%% ============================================================================
%% SIMPLE JSON (no deps): parse only what we use; encode only what we send
%% ============================================================================

parse_json_simple(Bin) when is_binary(Bin) ->
    %% We support messages like:
    %% {"type":"ohlc_bar","data":{"symbol":"EUR.USD","t_open":"...","o":1.0,"h":...,"l":...,"c":...,"vol":0,"source":"live"}, "timestamp":...}
    %% {"type":"trade_confirmation","data":{...}}
    %% {"type":"heartbeat", ...}
    Type = get_json_string(Bin, <<"type">>),
    case Type of
        <<"ohlc_bar">> ->
            DataBin = get_json_object_blob(Bin, <<"data">>),
            #{<<"type">> => <<"ohlc_bar">>,
              <<"data">> => #{
                    <<"symbol">> => get_json_string(DataBin, <<"symbol">>, <<"EUR.USD">>),
                    <<"t_open">> => get_json_string(DataBin, <<"t_open">>, <<>>),
                    <<"o">> => get_json_number(DataBin, <<"o">>, 0.0),
                    <<"h">> => get_json_number(DataBin, <<"h">>, 0.0),
                    <<"l">> => get_json_number(DataBin, <<"l">>, 0.0),
                    <<"c">> => get_json_number(DataBin, <<"c">>, 0.0),
                    <<"vol">> => trunc(get_json_number(DataBin, <<"vol">>, 0)),
                    <<"source">> => get_json_string(DataBin, <<"source">>, <<"live">>)
                }};
        <<"trade_confirmation">> ->
            DataBin2 = get_json_object_blob(Bin, <<"data">>),
            %% Pass through what we commonly care about
            #{<<"type">> => <<"trade_confirmation">>,
              <<"data">> => #{
                    <<"status">> => get_json_string(DataBin2, <<"status">>, <<"error">>),
                    <<"position">> => trunc(get_json_number(DataBin2, <<"position">>, 0)),
                    <<"entry_price">> => get_json_number(DataBin2, <<"entry_price">>, 0.0),
                    <<"pnl">> => get_json_number(DataBin2, <<"pnl">>, 0.0)
                }};
        <<"heartbeat">> ->
            #{<<"type">> => <<"heartbeat">>};
        _ ->
            #{<<"type">> => Type}
    end.

encode_json_simple(#{type := trade_signal,
                     symbol := Symbol,
                     action := Action,
                     quantity := Quantity,
                     signal := Signal}) ->
    %% Encode only the trade_signal we send to Python
    %% {"type":"trade_signal","symbol":"EUR.USD","action":"BUY","quantity":0.1,"signal":1}
    Cid = erlang:unique_integer([monotonic]),
    list_to_binary(io_lib:format(
        "{\"type\":\"trade_signal\",\"symbol\":\"~s\",\"action\":\"~s\",\"quantity\":~p,\"signal\":~p,\"cid\":~p}",
        [Symbol, Action, Quantity, Signal, Cid]
    ));
encode_json_simple(Map) when is_map(Map) ->
    %% Fallback: minimal {"type":"unknown"} to avoid crashes
    Type = case maps:get(type, Map, unknown) of
               T when is_atom(T) -> atom_to_list(T);
               T when is_list(T) -> T;
               T when is_binary(T) -> binary_to_list(T);
               _ -> "unknown"
           end,
    list_to_binary(io_lib:format("{\"type\":\"~s\"}", [Type])).

%% ---- Tiny JSON helpers (string/number/object[data]) ------------------------

get_json_string(Bin, Key) ->
    get_json_string(Bin, Key, <<>>).

get_json_string(Bin, Key, Default) ->
    %% Match "Key":"value"
    Pat = <<$\", Key/binary, $\", $:, $\", (<<"([^\"]*)">>)/binary, $\">>,
    case re:run(Bin, Pat, [{capture, all_but_first, binary}, unicode]) of
        {match, [Val]} -> Val;
        _ -> Default
    end.

get_json_number(Bin, Key, Default) ->
    %% Match "Key":-?123(.456)?
    Pat = <<$\", Key/binary, $\", $:, (<<"\\s*(-?\\d+(?:\\.\\d+)?)">>)/binary>>,
    case re:run(Bin, Pat, [{capture, all_but_first, list}, unicode]) of
        {match, [NumList]} ->
            try list_to_float(NumList)
            catch _:_ ->
                try list_to_integer(NumList)
                catch _:_ -> Default
                end
            end;
        _ -> Default
    end.

get_json_object_blob(Bin, Key) ->
    %% Very simple: capture inside first {...} after "Key":
    %% Works because `data` is a flat object without nested braces.
    Pat = <<$\", Key/binary, $\", $:, $\\, ${, (<<"([^}]*)">>)/binary, $}>>,
    case re:run(Bin, Pat, [{capture, all_but_first, binary}, unicode, dotall]) of
        {match, [Inner]} -> Inner;
        _ -> <<>>
    end.

%% ============================================================================
%% ETS TABLE MANAGEMENT & HELPERS
%% ============================================================================

ensure_live_table(Table) when is_atom(Table) ->
    case ets:info(Table) of
        undefined ->
            %fx:log(io_lib:format("Creating ETS table: ~p~n", [Table])),
            ets:new(Table, [ordered_set, public, named_table, {keypos, 2}]);
        _ -> 
            %fx:log(io_lib:format("ETS table already exists: ~p~n", [Table])),
            ok
    end.

find_start_index(Table, EndIndex, Count) ->
    find_start_index_loop(Table, EndIndex, Count).

find_start_index_loop(_Table, Index, 0) ->
    Index;
find_start_index_loop(Table, Index, N) ->
    case ets:prev(Table, Index) of
        '$end_of_table' ->
            Index;
        PrevIndex ->
            find_start_index_loop(Table, PrevIndex, N - 1)
    end.

%% Resolve requested table or fallback to live state's default
resolve_table(TableName, State) when is_atom(TableName) ->
    Name = atom_to_list(TableName),
    case lists:reverse(Name) of
        [$E,$V,$I,$L,$_|_] -> 
            %fx:log(io_lib:format("Table name ~p already ends with _LIVE:~p, Name: ~p~n", [TableName, Name, lists:reverse(Name)])),
            TableName;              %% ends with "_LIVE"
        _ -> 
            %fx:log(io_lib:format("Table name ~p does not end with _LIVE, converting to ~p~n", [TableName, Name ++ "_LIVE"])),
            list_to_atom(Name ++ "_LIVE")
    end;
resolve_table(_Other, State = #python_state{}) ->
    State#python_state.live_state#live_state.table_name;
resolve_table(_Other, #live_state{table_name = T}) -> T.

%% Convert 'EUR.USD' to 'EURUSD1_LIVE'
symbol_to_live_table(Bin) when is_binary(Bin) ->
    symbol_to_live_table(binary_to_list(Bin));
symbol_to_live_table(List) when is_list(List) ->
    Pair = [C || C <- List, C =/= $.],
    list_to_atom(Pair ++ "1_LIVE").

log_ohlc_bar(Id, O, H, L, C, Vol) ->
    Timestamp = lists:flatten(format_timestamp(Id)),
    Line = io_lib:format("~s,~p,~p,~p,~p,~p~n", [Timestamp, O, H, L, C, Vol]),
    file:write_file("live_bars.log", Line, [append]),
    ok.

format_timestamp({Y,Mo,D,Hh,Mi,Ss,_}) ->
    io_lib:format(" ~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y,Mo,D,Hh,Mi,Ss]);
format_timestamp(_) ->
    " ".
