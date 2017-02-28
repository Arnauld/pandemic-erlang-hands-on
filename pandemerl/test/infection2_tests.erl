%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. févr. 2017 21:46
%%%-------------------------------------------------------------------
-module(infection2_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_infect_a_city__test() ->
  try
    city2:start(london, []),
    infection2:infect(london, blue),
    Level = city2:infection_level(london, blue),
    ?assertEqual({infection_level, london, blue, 1}, Level)
  after
    unregister(london)
  end.


should_propagate_outbreak_to_linked_cities__test() ->
  try
    city2:start(london, [paris, essen]),
    city2:start(paris, []),
    city2:start(essen, []),
    infection2:infect(london, blue),
    infection2:infect(london, blue),
    infection2:infect(london, blue),
    infection2:infect(london, blue),
    ?assertEqual({infection_level, london, blue, 3}, city2:infection_level(london, blue)),
    ?assertEqual({infection_level, paris, blue, 1}, city2:infection_level(paris, blue)),
    ?assertEqual({infection_level, essen, blue, 1}, city2:infection_level(essen, blue))
  after
    unregister(london),
    unregister(paris),
    unregister(essen)
  end.
