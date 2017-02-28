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
-export([infect/2, infect/3, infection_level/2]).
-export([loop/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start(City, Links) ->
  State = #state{name = City, links = Links},
  Pid = spawn(?MODULE, loop, [State]),
  register(City, Pid),
  {ok, Pid}.

infect(City, Disease) ->
  infect(City, Disease, undefined).

infect(City, Disease, ResponseHandler) ->
  City ! {infect, ResponseHandler, Disease}.

infection_level(City, Disease) ->
  City ! {infection_level, self(), Disease},
  receive
    {infection_level, Disease, Level} ->
      {infection_level, Disease, Level}
  end.

loop(State) ->
  receive
    {infect, From, Disease} ->
      Levels = State#state.infection_levels,
      Level = proplists:get_value(Disease, Levels, 0),
      case Level of
        3 ->
          reply(From, {outbreak, State#state.links}),
          loop(State);

        _ ->
          NewLevel = Level + 1,
          NewLevels = [{Disease, NewLevel} | proplists:delete(Disease, Levels)],
          NewState = State#state{infection_levels = NewLevels},
          reply(From, {infected, NewLevel}),
          loop(NewState)
      end;

    {infection_level, From, Disease} ->
      Levels = State#state.infection_levels,
      Level = proplists:get_value(Disease, Levels, 0),
      From ! {infection_level, Disease, Level},
      loop(State)

  end.

reply(undefined, _) -> ok;
reply(From, Message) -> From ! Message.