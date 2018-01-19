%%%-------------------------------------------------------------------
%%% @author rahulsinha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2018 8:46 PM
%%%-------------------------------------------------------------------
-module(simplest_nn_test).
-author("rahulsinha").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  simplest_nn:create(),
  gen_server:call(cortex,sense_think_act),
  ?assert(true).
sup_test() ->
  simplest_nn_sup:start_link(),
  gen_server:call(cortex,sense_think_act),
  gen_server:cast(sensor,stop),
  gen_server:call(cortex,sense_think_act).
