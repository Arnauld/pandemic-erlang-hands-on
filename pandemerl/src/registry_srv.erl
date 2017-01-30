%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% PROTOCOL:
%%% =========
%%%
%%%
%%%                      [a:registry]                [x:registry]
%%%   :synchronize_locals   |                           :
%%%  ---------------------->||    ∀ n ∈ nodes()         :
%%%                         ||------------------------->|| {from, from_locals}
%%%                         ||    :fetch_locals         ||
%%%                         :                           ||
%%%                         :          from             ||
%%%                         ||<-------------------------||
%%%                              :fetch_locals_response
%%%
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
  nodes = [],
  pending_nodes = []
}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_local({Type, Name}) ->
  gen_server:call(?SERVER, {register, Type, Name}).

get_locals() ->
  gen_server:call(?SERVER, get_locals).

synchronize_locals() ->
  gen_server:cast(?SERVER, synchronize_locals).

schedule_check_pending_nodes() ->
  spawn(fun() ->
    timer:sleep(5 * 1000),
    check_pending_nodes()
        end).

check_pending_nodes() ->
  gen_server:cast(?SERVER, check_pending_nodes).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  error_logger:info_msg("Distributed registry initializing ~p~n", [self()]),
  net_kernel:monitor_nodes(true),
  {ok, #state{}}.

handle_call(get_locals, _From, State) ->
  {reply, State#state.locals, State};
handle_call({register, Type, Name}, _From, State) ->
  NewLocals = [{Type, Name}, State#state.locals],
  NewState = State#state{locals = NewLocals},
  {reply, ok, NewState};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(check_pending_nodes, State = #state{pending_nodes = PendingNodes}) ->
  case PendingNodes of
    [] ->
      error_logger:info_msg("No pending node to check~n", []),
      ok;
    _ ->
      broadcast_fetch_locals(State),
      schedule_check_pending_nodes()
  end,
  {noreply, State};
handle_cast(synchronize_locals, State) ->
  NewState = State#state{pending_nodes = nodes()},
  broadcast_fetch_locals(NewState),
  schedule_check_pending_nodes(),
  {noreply, NewState};
handle_cast({fetch_locals, Node, NodeLocals}, State = #state{locals = Locals}) ->
  error_logger:info_msg("Received a 'fetch_locals' from node ~p containing: ~p", [Node, NodeLocals]),
  gen_server:cast({?SERVER, Node}, {fetch_locals_response, node(), Locals}),
  NewState = update_node_locals(State, Node, NodeLocals),
  {noreply, NewState};
handle_cast({fetch_locals_response, Node, NodeLocals}, State) ->
  error_logger:info_msg("Received a 'fetch_locals_response' from node ~p containing: ~p", [Node, NodeLocals]),
  NewState = update_node_locals(State, Node, NodeLocals),
  {noreply, NewState};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({nodeup, Node}, State = #state{pending_nodes = PendingNodes}) ->
  error_logger:info_msg("Node joined: ~p~n", [Node]),
  NewPendingNodes = add_node(PendingNodes, Node),
  NewState = State#state{pending_nodes = NewPendingNodes},
  schedule_check_pending_nodes(),
  {noreply, NewState};
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
  #state{nodes = Nodes, pending_nodes = PendingNodes} = State,
  NewNodes0 = discard_locals_of_node(Node, Nodes),
  NewNodes1 = append_locals_of_node(Node, NodeLocals, NewNodes0),
  NewPendingNodes = remove_node(PendingNodes, Node),
  State#state{nodes = NewNodes1, pending_nodes = NewPendingNodes}.

remove_locals_of_node(State = #state{nodes = NodesLocals, pending_nodes = PendingNodes}, NodeToDiscard) ->
  NewNodesLocals = discard_locals_of_node(NodeToDiscard, NodesLocals),
  NewPendingNodes = remove_node(PendingNodes, NodeToDiscard),
  State#state{nodes = NewNodesLocals, pending_nodes = NewPendingNodes}.

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

broadcast_fetch_locals(#state{locals = Locals, pending_nodes = Nodes}) ->
  case Nodes of
    [] ->
      error_logger:info_msg("No node known to synchronize with", []);
    _ ->
      lists:foreach(
        fun(Node) ->
          Rep = gen_server:cast({?SERVER, Node}, {fetch_locals, node(), Locals}),
          error_logger:info_msg("Broadcasting locals to ~p: ~p~n", [Node, Rep])
        end,
        Nodes)
  end.

add_node(Nodes, Node) ->
  add_node(Nodes, Node, []).

add_node([], Node, Acc) -> [Node | Acc];
add_node([Node | Others], Node, Acc) ->
  add_node(Others, Node, Acc);
add_node([Other | Others], Node, Acc) ->
  add_node(Others, Node, [Other | Acc]).


remove_node(Nodes, Node) ->
  remove_node(Nodes, Node, []).

remove_node([], _Node, Acc) -> Acc;
remove_node([Node | Others], Node, Acc) ->
  remove_node(Others, Node, Acc);
remove_node([Other | Others], Node, Acc) ->
  remove_node(Others, Node, [Other | Acc]).