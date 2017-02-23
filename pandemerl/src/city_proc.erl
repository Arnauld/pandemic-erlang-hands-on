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

-export([start/1, log_state/1]).
-export([loop/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(City) ->
  {ok, State} = city:new(City),
  spawn(?MODULE, loop, [State]).

log_state(City) when is_pid(City) ->
  City ! log_state.


loop(State) ->
  receive
    log_state ->
      Name = city:name_of(State),
      BlueLevel = city:infection_level(State, blue),
      error_logger:info_msg("Name: ~p, blue: ~p", [Name, BlueLevel]),
      loop(State)
  end.
