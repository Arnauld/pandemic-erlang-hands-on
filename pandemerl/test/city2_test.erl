%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. févr. 2017 09:27
%%%-------------------------------------------------------------------
-module(city2_test).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_not_be_infected_by_default__test() ->
  {ok, _Pid} = city2:start(london, []),
  try
    Level = city2:infection_level(london, blue),
    ?assertEqual({infection_level, london, blue, 0}, Level)
  after
    unregister(london)
  end.

should_increase_infection_level_when_infected__test() ->
  {ok, _Pid} = city2:start(london, []),
  try
    city2:infect(london, blue, self()),
    receive
      Msg ->
        ?assertEqual({infected, london, blue, 1}, Msg)
    after 1000 ->
      ?assert(timeout)
    end,

    Level = city2:infection_level(london, blue),
    ?assertEqual({infection_level, london, blue, 1}, Level)
  after
    unregister(london)
  end.

should_outbreak_when_infection_level_reaches_the_threashold__test() ->
  {ok, _Pid} = city2:start(london, [paris, essen]),
  try
    city2:infect(london, blue),
    city2:infect(london, blue),
    city2:infect(london, blue, self()),
    receive
      Msg1 ->
        ?assertEqual({infected, london, blue, 3}, Msg1)
    after 1000 ->
      ?assert(timeout)
    end,

    city2:infect(london, blue, self()),
    receive
      Msg2 ->
        ?assertEqual({outbreak, london, blue, [paris, essen]}, Msg2)
    after 1000 ->
      ?assert(timeout)
    end
  after
    unregister(london)
  end.

