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

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
infect(City, Disease) ->
  city2:infect(City, Disease, self()),
  receive
    {outbreak, City, Disease, Links} ->
      lists:foreach(fun(LinkedCity) -> infect(LinkedCity, Disease) end, Links);

    {infected, City, Disease, _Level} ->
      ok
  end.