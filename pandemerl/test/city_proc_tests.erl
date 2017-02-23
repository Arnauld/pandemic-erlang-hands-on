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
  % starts
  Pid = city_proc:start(london),
  % log the stats
  city_proc:log_state(Pid),
  % wait and grab published event
  log_collector:waits_for_event(),
  Events = filter_event(log_collector:get_events()),
  ?assertEqual([{"Name: ~p, blue: ~p", [london, 0]}], Events).


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