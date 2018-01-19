%%%-------------------------------------------------------------------
%%% @author rahulsinha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2018 9:59 PM
%%%-------------------------------------------------------------------
-module(simplest_nn_sup).
-author("rahulsinha").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 30,
  MaxSecondsBetweenRestarts = 60,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,
  Weights = [random:uniform()-0.5,random:uniform()-0.5,random:uniform()-0.5],
  Neuron = {neuron, {'simplest_nn', create, [neuron,Weights]},
    Restart, Shutdown, Type, [simplest_nn]},
  Sensor = {sensor, {'simplest_nn', create, [sensor,Weights]},
    Restart, Shutdown, Type, [simplest_nn]},
  Actuator = {actuator, {'simplest_nn', create, [actuator,Weights]},
    Restart, Shutdown, Type, [simplest_nn]},
  Cortex = {cortex, {'simplest_nn', create, [cortex,Weights]},
    Restart, Shutdown, Type, [simplest_nn]},
  {ok, {SupFlags, [Neuron,Sensor,Actuator,Cortex]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
