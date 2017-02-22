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
  {ok, City} = city:new(london),
  ?assertEqual(london, city:name_of(City)).

should_not_be_infected_by_default__test() ->
  {ok, City} = city:new(london),
  ?assertEqual(0, city:infection_level(City, blue)).

should_increase_infection_level_when_infected__test() ->
  {ok, City0} = city:new(london),
  {ok, City1} = city:infect(City0, blue),
  ?assertEqual(1, city:infection_level(City1, blue)).

should_outbreak_when_infection_level_reaches_the_threashold__test() ->
  {ok, City0} = city:new(london),
  {ok, City1} = city:infect(City0, blue),
  {ok, City2} = city:infect(City1, blue),
  {ok, City3} = city:infect(City2, blue),
  ?assertEqual(3, city:infection_level(City3, blue)),
  outbreak = city:infect(City3, blue),
  ?assertEqual(3, city:infection_level(City3, blue)).

