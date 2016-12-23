%%%-------------------------------------------------------------------
%% @doc pandemerl public API
%% @end
%%%-------------------------------------------------------------------

-module(pandemerl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(APP_DIR, pandemerl).

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
	Port = port(),
	Dispatch = cowboy_router:compile([
        %% {HostMatch, list({PathMatch, Handler, InitialState})}
        {'_', [
            {"/", cowboy_static, {priv_file, ?APP_DIR, "static/index.html"}},
            {"/assets/[...]", cowboy_static, {priv_dir, ?APP_DIR, "static/assets"}},
            {"/api/[...]", pe_web_handler, []}]}
    ]),

    %% Name, NbAcceptors, TransOpts, ProtoOpts
    {ok, _} = cowboy:start_clear(my_http_listener, 100,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    io:format("Http server started on port ~p~n", [Port]).

%%
%% Retrieve the absolute path of the static resources
%%
static_dir() ->
    {ok, StaticDir} = application:get_env(static_dir),
    StaticDir.

%%
%% Retrieve the PORT either from an os environment variable
%% e.g. in Heroku environment, or from the application conf.
%%
port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.
