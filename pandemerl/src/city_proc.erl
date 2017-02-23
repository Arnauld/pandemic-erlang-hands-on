%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. févr. 2017 20:21
%%%-------------------------------------------------------------------
-module(city_proc).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1, log_state/1, infection_level/2]).
-export([loop/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(City) ->
  {ok, State} = city:new(City),
  spawn(?MODULE, loop, [State]).

log_state(City) when is_pid(City) ->
  City ! log_state.

infection_level(City, Disease) when is_pid(City) ->
  City ! {infection_level, self(), Disease},
  receive
    {infection_level_response, Disease, Level} ->
      Level
  end.

loop(State) ->
  receive
    log_state ->
      Name = city:name_of(State),
      BlueLevel = city:infection_level(State, blue),
      error_logger:info_msg("Name: ~p, blue: ~p", [Name, BlueLevel]),
      loop(State);

    {infection_level, From, Disease} ->
      Level = city:infection_level(State, Disease),
      From ! {infection_level_response, Disease, Level},
      loop(State)
  end.
