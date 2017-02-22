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

-export([new/1, name_of/1, infection_level/2, infect/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
new(City) ->
  CityState = {City, []},
  {ok, CityState}.

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

infect(City, Disease) ->
  {Name, Levels} = City,
  Level = find_infection_level(Levels, Disease, 0),
  case Level of
    3 ->
      outbreak;

    _ ->
      NewLevel = Level + 1,
      NewLevels = set_infection_level(Levels, Disease, NewLevel),
      NewCity = {Name, NewLevels},
      {ok, NewCity}
  end.

set_infection_level(Levels, Disease, NewLevel) ->
  set_infection_level(Levels, Disease, NewLevel, []).

set_infection_level([], Disease, NewLevel, Acc) -> [{Disease, NewLevel} | Acc];
set_infection_level([{Disease, Level} | Others], Disease, NewLevel, Acc) -> [{Disease, NewLevel} | Acc] ++ Others;
set_infection_level([Other | Others], Disease, NewLevel, Acc) ->
  set_infection_level(Others, Disease, NewLevel, [Other | Acc]).