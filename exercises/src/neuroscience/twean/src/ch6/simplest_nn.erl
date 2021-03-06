%%%-------------------------------------------------------------------
%%% @author rahulsinha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jan 2018 9:14 PM
%%%-------------------------------------------------------------------
-module(simplest_nn).
-author("rahulsinha").

-behaviour(gen_server).

%% API
-compile(export_all).

create(Name,W) ->
  io:format("~nStarting ~p~n",[Name]),
  %% To know when the parent shuts down
  process_flag(trap_exit, true),
  {ok,N_PId} = gen_server:start_link({local, Name}, ?MODULE, [Name,W], []).
  %% gen_server callbacks


-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([cortex,W]) ->
  {ok,[cortex]};
init([neuron,W]) ->
  {ok, [neuron,W]};
init([sensor,W]) ->
  {ok,[sensor]};
init([actuator,W]) ->
  {ok,[actuator]}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call(R,F,[sensor]) ->
  case R of
    sync ->
      io:format("Sensor received sync signal,"),
      Sensory_Signal = [random:uniform(),random:uniform()],
      io:format("****Sensing****:~n Signal from the environment
      ~p~n",[Sensory_Signal]),
      gen_server:call(neuron,{self(),forward,Sensory_Signal}),
      {reply,ok,[sensor]}
  end;

handle_call(R,F,[neuron,Weights]) ->
  io:format("~nneuron handle call  ~p ~n~n",[Weights]),
  case R of
    {S_PId, forward, Input} ->
      io:format("~nneuron received forward message~n"),
      Dot_Product = dot(Input,Weights,0),
      Output = [math:tanh(Dot_Product)],
      gen_server:call(actuator,{self(),forward,Output}),
      {reply,ok,[neuron,Weights]};

    stop ->
      io:format("~nneuron stopping~n"),
      {stop,stopped_by_cortex,[]}
  end;


handle_call(R,F,[actuator]) ->
  case R of
    stop ->
      io:format("~nactuator stopping~n"),
      {stop,stop,[]};
    _ ->
      io:format("~nIn actuator ~p ~n",[R]),
      {reply,ok, [actuator]}
  end;


handle_call(R,F,[cortex]) ->
  io:format("~ncortex handle call~n "),
  case R of
    sense_think_act ->
      gen_server:call(sensor,sync),
      {reply,ok,[cortex]};
    stop ->gen_server:stop(neuron),
           gen_server:stop(actuator),
           gen_server:stop(sensor),
           io:format("shutting down"),
           {stop,stopping,[]}
  end;


handle_call(R,F,[sensor]) ->
  case R of
    stop ->
       io:format("~nsensor stopping~n"),
       {stop,stopped_by_cortex,[]};

    _ ->
     io:format("~nsensor handle call~n "),
     {reply,ok,[sensor]}
  end;

handle_call(_Request, _From, State) ->
  io:format("~ngeneral handle call~n"),
  {reply, ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(Request, [sensor]) ->
  case Request of
    stop ->
      {stop,stopped_by_message, [sensor]};
    _->
      io:format("ignoring")
  end;

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
dot([I|Input],[W|Weights],Acc) ->
  io:format("In dot I=~p,W=~p",[I,W]),
  dot(Input,Weights,I*W + Acc);
dot([],[Bias],Acc)->
  Acc + Bias.