%%%-------------------------------------------------------------------
%%% @author rahulsinha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jan 2018 10:56 PM
%%%-------------------------------------------------------------------
-module(simple_neuron_test).
-author("rahulsinha").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  simple_neuron:create(),
  Result = simple_neuron:send([1.0,1.0]),
  io:format("~nResult = ~p",[Result]),
  ?assert(length(Result) =:= 1),
  ?assertEqual(hd(Result),0.5458432305609977).
 %%% ?assert(true).
