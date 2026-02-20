%% @doc Health check handler — GET /health
-module(bc_http_health_h).

-export([init/2]).

init(Req, State) ->
    Body = jsx:encode(#{status => <<"ok">>,
                        ts     => erlang:system_time(second)}),
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req),
    {ok, Req2, State}.
