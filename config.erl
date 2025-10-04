%% Configuration file for FX Trading System
%% Centralized configuration for all system parameters
%% Options listed in comments show available alternatives

-module(config).
-compile(export_all).

%% === Trading Parameters ===
account_leverage() -> 50.                    % Options: 1-500 (typical: 10, 50, 100, 200)
account_initial_balance() -> 300.            % Options: any positive number (typical: 100-10000)
account_lot_size() -> 10000.                 % Options: 1000, 10000, 100000 (micro, mini, standard lots)
account_spread() -> 0.000150.                % Options: 0.00001-0.001 (1-100 pips, typical: 0.00015)
account_margin() -> 0.                       % Options: 0-1.0 (percentage, 0=no margin requirement)
order_size_percentage() -> 0.2.              % Options: 0.01-1.0 (1%-100% of balance per trade)
buy_money_fixed() -> 100.                    % Options: any positive number (fixed trade size)
min_profit_threshold() -> 0.000150.          % Options: 0.00001-0.01 (minimum profit in pips)

%% === Data Parameters ===
primary_currency_pair() -> 'EURUSD1'.         % Options: 'EURUSD1', 'EURUSD1_LIVE', 'EURUSD15', 'EURUSD30', 'EURUSD60'
data_start_index() -> 1000.                  % Options: 1-N (starting row for training data)
data_end_index() -> 200.                     % Options: data_start_index+1 to N (ending row for training)
benchmark_end_index() -> last.                % Options: last, or specific number (ending row for benchmark)
market_props_start() -> 800.                 % Options: 1-N (market analysis start point)
market_props_end() -> 200.                   % Options: market_props_start+1 to N (market analysis end)

%% === Sensor Configuration - Standard Forex Trader ===
pli_resolutions() -> [20].                   % Options: [5], [10], [20], [5,10], [10,20], [5,10,20], etc.
pci_horizontal_resolutions() -> [90].        % Options: [10], [20], [50], [10,20], [20,50], etc.
pci_vertical_resolutions() -> [20].          % Options: [10], [15], [20], [10,15], [15,20], etc.

%% === Sensor Configuration - 1-Minute Optimized ===
pli_1m_resolutions() -> [5,10,20].           % Options: [5], [10], [20], [5,10], [10,20], [5,10,20], [3,5,10,20]
pci_1m_horizontal_resolutions() -> [20,30].  % Options: [10], [20], [30], [10,20], [20,30], [10,20,30]
pci_1m_vertical_resolutions() -> [10,15].    % Options: [5], [10], [15], [5,10], [10,15], [5,10,15]
internal_sensor_dimensions() -> 3.           % Options: 1-10 (trading state dimensions: position, profit, time)

%% === Evolution Parameters ===
specie_size_limit() -> 50.                    % Options: 1-100 (max agents per species)
init_specie_size() -> 50.                     % Options: 1-50 (initial agents per species)
evaluations_limit() -> 1000.                    % Options: 10-100000 (max evaluations per run)
survival_percentage() -> 0.5.                % Options: 0.1-0.9 (percentage of agents that survive)
tot_runs() -> 1000.                             % Options: 1-100 (number of benchmark runs)

%% === Neural Network Parameters ===
morphology() -> forex_trader.             % Options: forex_trader, forex_trader_1m
tuning_duration() -> {const,10}.             % Options: {const,N}, {linear,N}, {exponential,N}
annealing_parameters() -> [1].               % Options: [0.1], [0.5], [1], [0.5,1], [0.1,0.5,1]
perturbation_ranges() -> [1].                % Options: [0.1], [0.5], [1], [2], [0.5,1], [1,2]
neural_activation_functions() -> [tanh,cos,gaussian,absolute]. % Options: tanh, cos, sin, gaussian, absolute, sigmoid, sqrt
neural_plasticity_functions() -> [none].     % Options: none, hebbian, ojas, self_modulation
agent_encoding_types() -> [substrate].          % Options: neural, substrate

%% === Benchmarker Constraint Parameters ===
population_evo_alg_f() -> generational.      % Options: generational, steady_state
connection_architecture() -> recurrent.    % Options: feedforward, recurrent

%% === Additional Neural Network Parameters ===
substrate_plasticities() -> [none].          % Options: none, hebbian, ojas, self_modulation
substrate_linkforms() -> [l2l_feedforward].  % Options: l2l_feedforward, jordan_recurrent, fully_connected
neural_aggregation_functions() -> [dot_product]. % Options: dot_product, mult_product, diff
tuning_selection_functions() -> [dynamic_random]. % Options: all, all_random, recent, recent_random, lastgen, lastgen_random
heredity_types() -> [darwinian].             % Options: darwinian, lamarckian
tot_topological_mutations_functions() -> [{ncount_exponential,0.5}]. % Options: {ncount_exponential,0.5}, {ncount_linear,1}
population_fitness_postprocessor_f() -> size_proportional. % Options: none, size_proportional
population_selection_f() -> competition.     % Options: competition, top3

%% === Mutation Operators Configuration ===
mutation_operators() -> [
    {mutate_weights,1},
    {add_bias,1},
    {remove_bias,1},
    {mutate_af,1},
    {add_outlink,40},
    {add_inlink,40},
    {add_neuron,40},
    {outsplice,4},
    {add_sensor,100},
    {add_actuator,1},
    {mutate_plasticity_parameters,1},
    {add_cpp,1},
    {add_cep,1}
]. % Options: adjust weights and probabilities as needed

%% === System Configuration ===
fx_tables_directory() -> "fx_tables/".  % Options: any valid directory path
source_directory() -> "fx_tables/".     % Options: any valid directory path  
actuator_debug_tag() -> false.               % Options: true, false (enables trade-by-trade debug output)
sensor_debug_tag() -> false.                 % Options: true, false (enables sensor debug output)
