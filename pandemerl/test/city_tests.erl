%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. févr. 2017 09:31
%%%-------------------------------------------------------------------
-module(city_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_be_able_to_create_a_city__test() ->
  City = city:new(london),
  ?assertEqual(london, city:name_of(City)).

should_increase_infection_level_when_infected__test() ->
  City0 = city:new(london),
  City1 = city:infect(City0, blue),
  ?assertEqual(1, city:infection_level(City1, blue)).

