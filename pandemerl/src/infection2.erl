%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. févr. 2017 10:04
%%%-------------------------------------------------------------------
-module(infection2).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([infect/2]).

-record(to_infect, {city, generation}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
infect(City, Disease) ->
  Outbreaks = sets:new(),
  ToInfect = [#to_infect{city = City, generation = 0}],
  WaitingFor = [],
  Events = [],
  infect(Disease, ToInfect, Outbreaks, WaitingFor, Events).

infect(Disease, [], Outbreaks, [], Events) ->
  {infect, Disease, Events, sets:to_list(Outbreaks)};

infect(Disease, [], Outbreaks, WaitingFor, Events) ->
  receive
    {outbreak, City, Disease, Links} ->
      {#to_infect{generation = Generation}, NewWaitingFor} = remove_waiting_for(WaitingFor, City),
      NewEvents = [{outbreak, City, Disease, Generation} | Events],
      ToInfect = lists:map(
        fun(Linked) ->
          #to_infect{city = Linked, generation = Generation + 1}
        end, Links),
      NewOutbreaks = sets:add_element(City, Outbreaks),
      infect(Disease, ToInfect, NewOutbreaks, NewWaitingFor, NewEvents);

    {infected, City, Disease, NewLevel} ->
      {#to_infect{generation = Generation}, NewWaitingFor} = remove_waiting_for(WaitingFor, City),
      NewEvents = [{infected, City, Disease, NewLevel} | Events],
      infect(Disease, [], Outbreaks, NewWaitingFor, NewEvents)
  end;

infect(Disease, [ToInfect | ToInfects], Outbreaks, WaitingFor, Events) ->
  #to_infect{city = City, generation = Generation} = ToInfect,
  case sets:is_element(City, Outbreaks) of
    true ->
      NewEvents = [{infection_skipped, City, Disease, Generation} | Events],
      infect(Disease, ToInfects, Outbreaks, WaitingFor, NewEvents);
    false ->
      city2:infect(City, Disease, self()),
      NewEvents = [{infect, City, Disease, Generation} | Events],
      NewWaitingFor = [ToInfect | WaitingFor],
      infect(Disease, ToInfects, Outbreaks, NewWaitingFor, NewEvents)
  end.

remove_waiting_for(ToInfect, City) ->
  remove_waiting_for(ToInfect, City, [], not_found).

remove_waiting_for([], City, Acc, Found) -> {Found, Acc};
remove_waiting_for([ToInfect = #to_infect{city = City} | Others], City, Acc, not_found) ->
  remove_waiting_for(Others, City, Acc, ToInfect);
remove_waiting_for([ToInfect = #to_infect{city = City} | Others], City, Acc, Found) ->
  case ToInfect#to_infect.generation of
    Generation when Generation < Found#to_infect.generation ->
      remove_waiting_for(Others, City, [Found | Acc], ToInfect);
    _ ->
      remove_waiting_for(Others, City, [ToInfect | Acc], Found)

  end;
remove_waiting_for([ToInfect | Others], City, Acc, Found) ->
  remove_waiting_for(Others, City, [ToInfect | Acc], Found).