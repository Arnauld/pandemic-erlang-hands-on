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
