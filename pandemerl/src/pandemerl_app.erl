%%%-------------------------------------------------------------------
%% @doc pandemerl public API
%% @end
%%%-------------------------------------------------------------------

-module(pandemerl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	start_web_server(),
    pandemerl_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_web_server() ->
	Port = 8080,
	Dispatch = cowboy_router:compile([
        %% {HostMatch, list({PathMatch, Handler, InitialState})}
        {'_', [{"/", pe_web_handler, []}]}
    ]),

    %% Name, NbAcceptors, TransOpts, ProtoOpts
    {ok, _} = cowboy:start_clear(my_http_listener, 100,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ).