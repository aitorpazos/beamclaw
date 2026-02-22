%%
%% Copyright Péter Dimitrov 2026, All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

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
