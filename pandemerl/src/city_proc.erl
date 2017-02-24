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

-export([start/1, start_registered/1, start_registered/2]).
-export([log_state/1, infection_level/2, infect/2]).
-export([loop/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(City) ->
  start(City, []).

start(City, Links) ->
  {ok, State} = city:new(City, Links),
  spawn(?MODULE, loop, [State]).

start_registered(City) ->
  start_registered(City, []).

start_registered(City, Links) ->
  Pid = start(City, Links),
  register(City, Pid),
  Pid.

log_state(City) when is_pid(City) orelse is_atom(City) ->
  City ! log_state.

infection_level(City, Disease) when is_pid(City) orelse is_atom(City) ->
  City ! {infection_level, self(), Disease},
  receive
    {infection_level_response, Disease, Level} ->
      Level
  end.

infect(City, Disease) when is_pid(City) orelse is_atom(City) ->
  City ! {infect, self(), Disease},
  receive
    {infect_response, Response} ->
      Response
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
      loop(State);

    {infect, From, Disease} ->
      case city:infect(State, Disease) of
        outbreak ->
          Links = city:links_of(State),
          From ! {infect_response, {outbreak, Links}},
          loop(State);

        {ok, NewState} ->
          From ! {infect_response, infected},
          loop(NewState)
      end
  end.
