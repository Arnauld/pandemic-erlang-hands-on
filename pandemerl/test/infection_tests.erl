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
  PidLondon = city_proc:start_registered(london),
  infection:infect(london, blue),
  ?assertEqual(1, city_proc:infection_level(london, blue)).
