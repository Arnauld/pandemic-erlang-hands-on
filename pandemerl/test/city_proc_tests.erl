%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. févr. 2017 09:31
%%%-------------------------------------------------------------------
-module(city_proc_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_be_able_to_start_a_city__test() ->
  log_collector:start_link(),
  log_collector:listen_error_logger(),
  try
    % starts
    Pid = city_proc:start(london),
    % log the stats
    city_proc:log_state(Pid),
    % wait and grab published event
    log_collector:waits_for_event(),
    Events = filter_event(log_collector:get_events()),
    ?assertEqual([{"Name: ~p, blue: ~p", [london, 0]}], Events)
  after
    log_collector:stop_listening_error_logger()
  end.

should_not_be_infected_by_default__test() ->
  Pid = city_proc:start(london),
  ?assertEqual(0, city_proc:infection_level(Pid, blue)).

should_increase_infection_level_when_infected__test() ->
  Pid = city_proc:start(london),
  city_proc:infect(Pid, blue),
  ?assertEqual(1, city_proc:infection_level(Pid, blue)).

should_outbreak_when_infection_level_reaches_the_threashold__test() ->
  Pid = city_proc:start(london),
  city_proc:infect(Pid, blue),
  city_proc:infect(Pid, blue),
  city_proc:infect(Pid, blue),
  ?assertEqual(3, city_proc:infection_level(Pid, blue)),
  {outbreak, _Links} = city_proc:infect(Pid, blue),
  ?assertEqual(3, city_proc:infection_level(Pid, blue)).


filter_event(Events) ->
  filter_event(Events, []).

filter_event([], Acc) -> Acc;
filter_event([Event | Events], Acc) ->
  case Event of
    {info_msg, _Pid1, {_Pid2, Message, Args}} ->
      filter_event(Events, [{Message, Args} | Acc]);
    _ ->
      filter_event(Events, Acc)
  end.