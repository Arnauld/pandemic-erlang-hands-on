%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. févr. 2017 10:04
%%%-------------------------------------------------------------------
-module(infection).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([infect/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
infect(City, Disease) ->
  case city_proc:infect(City, Disease) of
    {outbreak, Links} ->
      lists:foreach(fun(LinkedCity) -> infect(LinkedCity, Disease) end, Links);

    _ ->
      ok
  end.