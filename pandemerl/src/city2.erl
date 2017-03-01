%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. févr. 2017 19:22
%%%-------------------------------------------------------------------
-module(city2).
-author("Arnauld").

-record(state, {name, links = [], infection_levels = []}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/2]).
-export([infect/2, infect/3, infection_level/2, infection_level/3]).
-export([loop/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start(City, Links) ->
  State = #state{name = City, links = Links},
  Pid = spawn(?MODULE, loop, [City, State]),
  register(City, Pid),
  {ok, Pid}.

infect(City, Disease) ->
  infect(City, Disease, undefined).

infect(City, Disease, ResponseHandler) ->
  City ! {infect, ResponseHandler, Disease}.

infection_level(City, Disease) ->
  City ! {infection_level, self(), Disease},
  receive
    Msg ->
      Msg
  end.

infection_level(City, Disease, Level) ->
  City ! {infection_level, self(), Disease, Level}.

loop(City, State) ->
  receive
    {infect, From, Disease} ->
      Levels = State#state.infection_levels,
      Level = proplists:get_value(Disease, Levels, 0),
      case Level of
        3 ->
          reply(From, {outbreak, City, Disease, State#state.links}),
          loop(City, State);

        _ ->
          NewLevel = Level + 1,
          NewLevels = [{Disease, NewLevel} | proplists:delete(Disease, Levels)],
          NewState = State#state{infection_levels = NewLevels},
          reply(From, {infected, City, Disease, NewLevel}),
          loop(City, NewState)
      end;

    {infection_level, From, Disease, NewLevel} ->
      Levels = State#state.infection_levels,
      NewLevels = [{Disease, NewLevel} | proplists:delete(Disease, Levels)],
      NewState = State#state{infection_levels = NewLevels},
      loop(City, NewState);

    {infection_level, From, Disease} ->
      Levels = State#state.infection_levels,
      Level = proplists:get_value(Disease, Levels, 0),
      From ! {infection_level, City, Disease, Level},
      loop(City, State)

  end.

reply(undefined, _) -> ok;
reply(From, Message) -> From ! Message.