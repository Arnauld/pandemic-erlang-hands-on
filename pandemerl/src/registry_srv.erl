%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. janv. 2017 19:40
%%%-------------------------------------------------------------------
-module(registry_srv).
-author("Arnauld").

-behaviour(gen_server).

%% API
-export([start_link/0, register_local/1, get_locals/0, synchronize_locals/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(REMOTE_ATTEMPT, 3).
-define(REMOTE_TIMEOUT, 1000).

-record(state, {
  locals = [],
  nodes = []
}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_local({Type, Name}) ->
  gen_server:cast(?SERVER, {register, Type, Name}).

get_locals() ->
  gen_server:call(?SERVER, get_locals).

synchronize_locals() ->
  gen_server:cast(?SERVER, synchronize_locals).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  error_logger:info_msg("Distributed registry initializing ~p~n", [self()]),
  net_kernel:monitor_nodes(true),
  {ok, #state{}}.

handle_call(get_locals, _From, State) ->
  {reply, State#state.locals, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(synchronize_locals, State = #state{locals = Locals}) ->
  lists:foreach(
    fun(Node) ->
      Rep = gen_server:cast({?SERVER, Node}, {fetch_locals, node(), Locals}),
      error_logger:info_msg("Broadcasting locals to ~p: ~p~n", [Node, Rep])
    end,
    nodes()),
  {noreply, State};
handle_cast({fetch_locals, Node, NodeLocals}, State = #state{locals = Locals}) ->
  error_logger:info_msg("Received a 'fetch_locals' from node ~p containing: ~p", [Node, NodeLocals]),
  gen_server:cast({?SERVER, Node}, {fetch_locals_response, node(), Locals}),
  NewState = update_node_locals(State, Node, NodeLocals),
  {noreply, NewState};
handle_cast({fetch_locals_response, Node, NodeLocals}, State) ->
  error_logger:info_msg("Received a 'fetch_locals_response' from node ~p containing: ~p", [Node, NodeLocals]),
  NewState = update_node_locals(State, Node, NodeLocals),
  {noreply, NewState};

handle_cast({register, Type, Name}, State) ->
  NewLocals = [{Type, Name}, State#state.locals],
  NewState = State#state{locals = NewLocals},
  {noreply, NewState};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({nodeup, Node}, State) ->
  error_logger:info_msg("Node joined: ~p~n", [Node]),
  {noreply, State};
handle_info({nodedown, Node}, State) ->
  error_logger:info_msg("Node left: ~p~n", [Node]),
  NewState = remove_locals_of_node(State, Node),
  {noreply, NewState};
handle_info(Info, State) ->
  error_logger:info_msg("Message received: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_node_locals(State, Node, NodeLocals) ->
  Nodes = State#state.nodes,
  NewNodes0 = discard_locals_of_node(Node, Nodes),
  NewNodes1 = append_locals_of_node(Node, NodeLocals, NewNodes0),
  State#state{nodes = NewNodes1}.

remove_locals_of_node(State = #state{nodes = NodesLocals}, NodeToDiscard) ->
  NewNodesLocals = discard_locals_of_node(NodeToDiscard, NodesLocals),
  State#state{nodes = NewNodesLocals}.

discard_locals_of_node(NodeToDiscard, NodesLocals) ->
  discard_locals_of_node(NodeToDiscard, NodesLocals, []).

discard_locals_of_node(_NodeToDiscard, [], Acc) -> Acc;
discard_locals_of_node(NodeToDiscard, [{NodeToDiscard, _} | Others], Acc) ->
  discard_locals_of_node(NodeToDiscard, Others, Acc);
discard_locals_of_node(NodeToDiscard, [NodeLocal | Others], Acc) ->
  discard_locals_of_node(NodeToDiscard, Others, [NodeLocal | Acc]).



append_locals_of_node(_Node, [], Acc) -> Acc;
append_locals_of_node(Node, [NodeLocal | Others], Acc) ->
  append_locals_of_node(Node, Others, [{Node, NodeLocal} | Acc]).