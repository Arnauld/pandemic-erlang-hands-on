-module(pe_web_handler).

-export([init/2]).

-define(TEXT_PLAIN, <<"text/plain">>).

init(Req0, State) ->
  	{Method, Req1} = cowboy_req:method(Req0),
  	{Path,   Req2} = cowboy_req:path(Req1),

    Req = cowboy_req:reply(200,
        #{<<"content-type">> => ?TEXT_PLAIN},
        <<"Hello Erlang!">>,
        Req0),
    {ok, Req, State}.