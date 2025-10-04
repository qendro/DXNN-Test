```bash
make all
make shell
```
``` erlang
make_all:all().
```

# Build once
```bash
docker build -t erlang-dev .
docker build -t erlang-dev -f .devcontainer/Dockerfile . #Moved to .devcontainer folder


# Run your neural network system
docker run -it --rm -v ${PWD}:/app -w /app erlang-dev
docker run -it --rm --network host -v ${PWD}:/app -w /app erlang-dev

make:all([load]).
launcher:start().
```
``` erlang
# Inside container:
make:all().
make:all([load]).
fx:clear_log().
mnesia:create_schema([node()]).
mnesia:start().
fx:init().
fx:start().
polis:create().
polis:start().
polis:sync().
benchmarker:start(sliding_window_5).

benchmarker:start(chart_plane_5x10).
# ... your neural network commands

```
## Reseting Mnesia
```bash
find . -name "*.beam" -delete
```
```erlang
mnesia:stop().
mnesia:delete_schema([node()]).
q().
```

4. **Print the best genotype:**
   ```erlang
  
   rr("records.hrl").
   % Print the best genotype from the default 'test' population
   genotype_utils:print_best_genotype().

   genotype_utils:print_best_genotype(all).
   
   % Or specify a population ID
   genotype_utils:print_best_genotype(your_population_id).
   
   % List all agents with their fitness scores
   genotype_utils:list_all_agents().
   
   % Print top N agents
   genotype_utils:print_top_agents(5).
   genotype_utils:print_top_agents(1, all).
   
   % Get agent statistics
   genotype_utils:get_agent_stats().

   BestAgentId = {5.699247180669372e-10,agent}.
   exoself:start(Best_Agent_Id, self(),benchmark).
   exoself:start(Best_Agent_Id, self(),live_trading).

    genotype:print({5.683653067369458e-10,agent})
   % 1. Load and start your best agent
   Best_Agent_Id = {5.693207755943648e-10,agent}.  % Update with your latest
   Agent_PId = exoself:start(Best_Agent_Id, self()).

   % 2. Monitor the agent
   is_process_alive(Agent_PId).
   is_process_alive(<0.567.0>).

   % 3. Simple test run
   run_best_agent() ->
      {atomic, Best_Agent_Id} = genotype_utils:find_best_agent(all),
      Agent_PId = exoself:start(Best_Agent_Id, self()),
      timer:sleep(30000),  % Run for 30 seconds
      Agent_PId ! {self(), terminate}.





    benchmarker:start(sliding_window_10).
    benchmarker:start(sliding_window_20).
    benchmarker:start(sliding_window_50).
    benchmarker:start(sliding_window_100).
    benchmarker:start(chart_plane_5x10).
    benchmarker:start(chart_plane_5x20).
    benchmarker:start(chart_plane_10x10).
    benchmarker:start(chart_plane_10x20).
    benchmarker:start(chart_plane_20x10).
    benchmarker:start(chart_plane_20x20).
    benchmarker:start(chart_plane_50x10).
    benchmarker:start(chart_plane_50x20).
    benchmarker:start(chart_plane_100x10).

    make:all([load]).
    fx:clear_log().
    mnesia:start().
    polis:start().
    polis:sync().
    {atomic, Best} = genotype_utils:find_best_agent(all).
    exoself:start(Best, self(), live_trading).
    <CX_ID> ! {<ActuatorPID>, sync, 0.5, 1}.
    <0.337.0> ! {<0.339.0>, sync, 0.5, 0}.

    {atomic, Best} = genotype_utils:find_best_agent(all).
    exoself:start(Best, self(), benchmark).

    live_scape:start_link().
    fx:sim(<0.323.0>).
   ```

## IB Python Service Calls

### Connection & Setup
```python
# Start the comprehensive IB service
python3 priv/ib_service.py

# Key service operations (handled automatically):
await ib_service.start_service()
await connection_manager.connect(host="host.docker.internal", port=7497, client_id=101)
await historical_loader.load_weeks_of_data("EUR.USD", weeks=4, bar_size="1 min")
```

### Trading Operations
```python
# Execute trades with risk management
await trade_executor.execute_trade("EUR.USD", "BUY", 1000, "MKT")
await trade_executor.execute_trade("EUR.USD", "SELL", 1000, "LMT", limit_price=1.0850)

# Risk management controls
trade_executor.activate_kill_switch("Emergency stop")
trade_executor.deactivate_kill_switch()
trade_executor.reset_daily_counters()
```

### Data & Monitoring
```python
# Get service status
connection_manager.get_connection_status()
trade_executor.get_trading_status()

# Live data streaming (automatic)
tick_aggregator.process_tick("EUR.USD", price, volume, timestamp)
bridge.send_ohlc_bar(ohlc_bar)
```

## Live_Scape Complete Setup & Trading Guide

### 1. System Initialization & Compilation
```erlang
% Compile all modules and load them
make:all([load]).

% Load record definitions (required for data structures)
rr("records.hrl").

% Initialize Mnesia database if not already done
mnesia:create_schema([node()]).
mnesia:start().

% Start the live trading system
{ok, Pid} = live_scape:start_link().

% Verify system is running
is_process_alive(Pid).
whereis(live_scape).
```

### 2. Python Service Setup & Market Data Loading
```erlang
% The Python IB service should be running in background
% Start it externally: python3 priv/ib_service.py

% Check if Python port handler is running
whereis(python_port_handler).

% Wait for initial market data to load (automatic via Python service)
% Check ETS table for data availability
ets:info(ohlc_data, size).

% If no data, wait a few moments for Python service to load historical data
timer:sleep(5000).
ets:info(ohlc_data, size).
```

### 3. Verify Market Data in ETS
```erlang
% Check if we have OHLC data
ets:info(ohlc_data).

% Get the latest data key
LastKey = ets:last(ohlc_data).

% Look at the most recent bar
case LastKey of
    '$end_of_table' -> 
        io:format("No data available yet~n");
    Key ->
        Bar = ets:lookup(ohlc_data, Key),
        io:format("Latest bar: ~p~n", [Bar])
end.

% Get last 10 bars to verify data flow
collect_recent_bars(ohlc_data, ets:last(ohlc_data), 10, []).
```

### 4. Start Live Simulation Mode
```erlang
% Create an ExoSelf process ID (neural network coordinator)
ExoSelfPid = self().

% Start live simulation - this connects the system to live data
live_scape ! {ExoSelfPid, live_sim}.

% The system is now ready to receive sensor requests and trade signals
```

### 5. Neural Network Sensor Operations (Market Data Access)
```erlang
% Test sensor functionality - get price list for neural network
% Request 100 bars of close prices (list_sensor format)
live_scape ! {self(), sense, ohlc_data, close, [100, list_sensor], undefined, undefined}.

% Wait for response
FromPid = whereis(live_scape),
receive
    {FromPid, PriceList} ->
        io:format("Received ~p price points~n", [length(PriceList)]),
        io:format("Latest prices: ~p~n", [lists:sublist(PriceList, 5)])
after 5000 ->
    io:format("Timeout waiting for sensor data~n")
end.

% Test graph sensor (for substrate neural networks)
live_scape ! {self(), sense, ohlc_data, close, [20, 10, graph_sensor], undefined, undefined}.

FromPid2 = whereis(live_scape),
receive
    {FromPid2, GraphData} ->
        io:format("Received graph data with ~p points~n", [length(GraphData)])
after 5000 ->
    io:format("Timeout waiting for graph sensor data~n")
end.

% Get internal state (position, entry price, P&L)
live_scape ! {self(), sense, internals, []}.

FromPid3 = whereis(live_scape),
receive
    {FromPid3, [Position, EntryPrice, PreviousPC]} ->
        io:format("Position: ~p, Entry: ~p, Previous P&L: ~p~n", [Position, EntryPrice, PreviousPC])
after 5000 ->
    io:format("Timeout waiting for internals~n")
end.
```

### 6. Direct ETS Data Access (Advanced)
```erlang
% Get the latest OHLC data key
LastKey = ets:last(ohlc_data).

% Look up specific OHLC bar by key {Symbol, Timestamp}
% Note: Timestamp format is ISO string like "2024-01-01T10:00:00"
case LastKey of
    '$end_of_table' -> 
        io:format("No data in table~n");
    {Symbol, Timestamp} ->
        io:format("Latest key: ~p~n", [{Symbol, Timestamp}]),
        Bar = live_scape:lookup(ohlc_data, LastKey),
        io:format("Latest bar: ~p~n", [Bar])
end.

% Navigate through data
NextKey = live_scape:next(ohlc_data, LastKey).
PrevKey = live_scape:prev(ohlc_data, LastKey, prev, 1).

% Get multiple previous bars
PrevKey10 = live_scape:prev(ohlc_data, LastKey, prev, 10).

% Check table size and info
ets:info(ohlc_data, size).
ets:info(ohlc_data, memory).

% WARNING: Only use this for small datasets
% ets:tab2list(ohlc_data).  % Shows all data - can be large!
```

### 7. Trading Operations (Live Trading Signals)
```erlang
% IMPORTANT: Ensure Python IB service is connected to Interactive Brokers
% and ALLOW_LIVE_ORDERS environment variable is set if using live trading

% Send BUY signal (go long)
live_scape ! {self(), trade, ohlc_data, 1}.

% Wait for trade confirmation
FromPid4 = whereis(live_scape),
receive
    {FromPid4, Fitness, Halt} ->
        io:format("Trade result - Fitness: ~p, Halt: ~p~n", [Fitness, Halt])
after 10000 ->
    io:format("Timeout waiting for trade confirmation~n")
end.

% Send SELL signal (go short)
live_scape ! {self(), trade, ohlc_data, -1}.

FromPid5 = whereis(live_scape),
receive
    {FromPid5, Fitness, Halt} ->
        io:format("Trade result - Fitness: ~p, Halt: ~p~n", [Fitness, Halt])
after 10000 ->
    io:format("Timeout waiting for trade confirmation~n")
end.

% Close position (exit current trade)
live_scape ! {self(), trade, ohlc_data, 0}.

FromPid6 = whereis(live_scape),
receive
    {FromPid6, Fitness, Halt} ->
        io:format("Close position result - Fitness: ~p, Halt: ~p~n", [Fitness, Halt])
after 10000 ->
    io:format("Timeout waiting for close confirmation~n")
end.

% Check current position and P&L
live_scape ! {self(), sense, internals, []}.

FromPid7 = whereis(live_scape),
receive
    {FromPid7, [Position, EntryPrice, PreviousPC]} ->
        io:format("Current Position: ~p~n", [Position]),
        io:format("Entry Price: ~p~n", [EntryPrice]),
        io:format("Previous P&L Change: ~p~n", [PreviousPC])
after 5000 ->
    io:format("Timeout waiting for position info~n")
end.
```

### 8. Complete Trading Session Example
```erlang
% Complete workflow from start to trade execution
make:all([load]).
rr("records.hrl").
mnesia:start().

% Start live system
{ok, Pid} = live_scape:start_link().

% Wait for data to load
timer:sleep(10000).

% Check data availability
DataSize = ets:info(ohlc_data, size),
io:format("Available bars: ~p~n", [DataSize]).

% Start live simulation
ExoSelfPid = self(),
live_scape ! {ExoSelfPid, live_sim}.

% Get market data for decision making
live_scape ! {self(), sense, ohlc_data, close, [50, list_sensor], undefined, undefined}.

FromPid8 = whereis(live_scape),
receive
    {FromPid8, PriceList} ->
        io:format("Got ~p prices, latest: ~p~n", [length(PriceList), lists:last(PriceList)]),
        
        % Simple trading logic example
        [Latest | _] = lists:reverse(PriceList),
        [Previous | _] = lists:reverse(lists:sublist(PriceList, length(PriceList)-1)),
        
        if Latest > Previous ->
            io:format("Price rising, sending BUY signal~n"),
            live_scape ! {self(), trade, ohlc_data, 1};
        Latest < Previous ->
            io:format("Price falling, sending SELL signal~n"),
            live_scape ! {self(), trade, ohlc_data, -1};
        true ->
            io:format("Price stable, no trade~n")
        end
after 5000 ->
    io:format("No market data received~n")
end.

% Wait for trade result
FromPid9 = whereis(live_scape),
receive
    {FromPid9, Fitness, Halt} ->
        io:format("Trade executed - Fitness: ~p, Halt: ~p~n", [Fitness, Halt])
after 15000 ->
    io:format("Trade timeout~n")
end.
```

### 9. Integration with Evolved Neural Networks
```erlang
% Load the best evolved agent for live trading
rr("records.hrl").

% Find the best agent from evolution
Best_Agent_Id = case genotype_utils:find_best_agent(all) of
    {atomic, AgentId} -> AgentId;
    _ -> {5.693207755943648e-10, agent}  % Fallback example ID
end.

% Start the best agent in live trading mode
% This connects the evolved neural network to live_scape
Agent_PId = exoself:start(Best_Agent_Id, self(), live_trading).

% Monitor the agent
is_process_alive(Agent_PId).

% The agent will now automatically:
% 1. Request market data via live_scape sensors
% 2. Process data through its evolved neural network
% 3. Send trading signals via live_scape actuators
% 4. Receive real-time P&L feedback

% Check agent performance
timer:sleep(60000),  % Let it trade for 1 minute
Agent_PId ! {self(), get_backup}.

receive
    {Agent_PId, backup, AgentBackup} ->
        io:format("Agent backup received: ~p~n", [AgentBackup])
after 5000 ->
    io:format("No backup received~n")
end.
```

### 10. System Monitoring & Troubleshooting
```erlang
% Check all system components
io:format("=== System Status ===~n"),
io:format("live_scape: ~p~n", [whereis(live_scape)]),
io:format("python_port_handler: ~p~n", [whereis(python_port_handler)]),
io:format("OHLC data size: ~p~n", [ets:info(ohlc_data, size)]),
io:format("Mnesia running: ~p~n", [mnesia:system_info(is_running)]).

% Check for recent market data
case ets:last(ohlc_data) of
    '$end_of_table' ->
        io:format("No market data available~n");
    LastKey ->
        [LastBar] = ets:lookup(ohlc_data, LastKey),
        io:format("Latest bar: ~p~n", [LastBar])
end.

% Restart components if needed
% live_scape ! terminate.  % Stop current instance
% {ok, NewPid} = live_scape:start_link().  % Start fresh

% Force Python service restart (if needed)
% Kill existing: pkill -f "python3 priv/ib_service.py"
% Then restart: python3 priv/ib_service.py &
```

### 11. Environment Variables for Trading
```bash
# Set these before starting Python service for live trading
export ALLOW_LIVE_ORDERS=1        # Enable live order execution
export AUTO_BACKFILL=1             # Auto-load historical data
export IB_HOST=host.docker.internal # IB TWS host
export IB_PORT=7497                # Paper trading (7496 for live)
export IB_CLIENT_ID=101            # Unique client ID

# Start Python service with environment
python3 priv/ib_service.py
```
