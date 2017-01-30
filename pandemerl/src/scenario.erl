%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. janv. 2017 19:10
%%%-------------------------------------------------------------------
-module(scenario).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([usecase/2, usecase1/0, usecase2/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
usecase1() ->
  usecase([essen, london], 'term2@Mentem').

usecase2() ->
  usecase([paris, algiers], 'term1@Mentem').

usecase(Cities, Remote) ->
  registry_sup:start_link(),
  lists:foreach(
    fun(City) ->
      registry_srv:register({city, City})
    end,
    Cities),
  net_kernel:connect_node(Remote),
  registry_srv:synchronize_locals().
