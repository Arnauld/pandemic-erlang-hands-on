%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. févr. 2017 21:46
%%%-------------------------------------------------------------------
-module(infection_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_infect_a_city__test() ->
  try
    PidLondon = city_proc:start_registered(london),
    infection:infect(london, blue),
    ?assertEqual(1, city_proc:infection_level(london, blue))
  after
    unregister(london)
  end.


should_propagate_outbreak_to_linked_cities__test() ->
  try
    city_proc:start_registered(london, [paris, essen]),
    city_proc:start_registered(paris, []),
    city_proc:start_registered(essen, []),
    infection:infect(london, blue),
    infection:infect(london, blue),
    infection:infect(london, blue),
    infection:infect(london, blue),
    ?assertEqual(1, city_proc:infection_level(paris, blue)),
    ?assertEqual(1, city_proc:infection_level(essen, blue))
  after
    unregister(london),
    unregister(paris),
    unregister(essen)
  end.
