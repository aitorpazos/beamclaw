%% @doc Cowboy HTTP listener wrapper.
%%
%% Routes:
%%   GET  /health                 → bc_http_health_h
%%   GET  /metrics                → bc_http_metrics_h
%%   POST /v1/chat/completions    → bc_http_completions_h
%%   GET  /ws                     → bc_ws_h
%%   POST /webhook/telegram       → bc_webhook_telegram_h
-module(bc_gateway_cowboy).

-export([start_link/0]).

start_link() ->
    Port   = maps:get(port, bc_config:get(beamclaw_gateway, http, #{port => 8080}), 8080),
    Routes = cowboy_router:compile([
        {'_', [
            {"/health",               bc_http_health_h,      []},
            {"/metrics",              bc_http_metrics_h,     []},
            {"/v1/chat/completions",  bc_http_completions_h, []},
            {"/ws",                   bc_ws_h,               []},
            {"/webhook/telegram",     bc_webhook_telegram_h, []}
        ]}
    ]),
    cowboy:start_clear(bc_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Routes}}).
