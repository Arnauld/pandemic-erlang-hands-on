%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. févr. 2017 09:31
%%%-------------------------------------------------------------------
-module(city).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new/1, name_of/1, infection_level/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
new(City) ->
  {City, []}.

name_of(City) ->
  {Name, _Levels} = City,
  Name.

infection_level(City, Disease) ->
  {_Name, Levels} = City,
  Level = find_infection_level(Levels, Disease, 0),
  Level.

find_infection_level([], _, DefaultValue) -> DefaultValue;
find_infection_level([{Disease, Level} | _], Disease, _) -> Level;
find_infection_level([_ | OtherLevels], Disease, DefaultValue) ->
  find_infection_level(OtherLevels, Disease, DefaultValue).