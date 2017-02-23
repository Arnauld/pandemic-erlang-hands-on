%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. févr. 2017 10:34
%%%-------------------------------------------------------------------
-module(log_collector_srv).
-author("Arnauld").

-behaviour(gen_server).

%% API
-export([start_link/0, log_event/1,
  get_events/0,
  waits_for_event/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {events = [], waiters = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

log_event(Event) ->
  gen_server:cast(?SERVER, {log_event, Event}).

get_events() ->
  gen_server:call(?SERVER, get_events).

waits_for_event() ->
  gen_server:cast(?SERVER, {waits_for_event, self()}),
  receive
    Events ->
      Events
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  {ok, #state{}}.

handle_call(get_events, _From, State) ->
  Reply = State#state.events,
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({waits_for_event, Waiter}, State = #state{waiters = Waiters}) ->
  NewState = State#state{waiters = [Waiter | Waiters]},
  {noreply, NewState};
handle_cast({log_event, Event}, State = #state{events = Events, waiters = Waiters}) ->
  NewState = State#state{events = [Event | Events]},
  lists:foreach(fun(Waiter) -> Waiter ! Event end, Waiters),
  {noreply, NewState};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
