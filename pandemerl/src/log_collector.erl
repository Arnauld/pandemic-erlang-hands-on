%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. févr. 2017 10:35
%%%-------------------------------------------------------------------
-module(log_collector).
-author("Arnauld").

-behaviour(gen_event).

%% API
-export([start_link/0, listen_error_logger/0, get_events/0, waits_for_event/0]).

%% gen_event callbacks
-export([init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  log_collector_srv:start_link().

listen_error_logger() ->
  error_logger:add_report_handler(?MODULE).

get_events() ->
  log_collector_srv:get_events().

waits_for_event() ->
  log_collector_srv:waits_for_event().

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_event(Event, State) ->
  log_collector_srv:log_event(Event),
  {ok, State}.

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Arg, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
