%% @doc Prometheus metrics scrape handler — GET /metrics
-module(bc_http_metrics_h).

-export([init/2]).

init(Req, State) ->
    %% TODO: collect metrics from prometheus registry
    Body = <<"# BeamClaw metrics\n">>,
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain; version=0.0.4">>},
        Body, Req),
    {ok, Req2, State}.
