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

-module(beamclaw_mcp_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case beamclaw_mcp_sup:start_link() of
        {ok, Pid} ->
            %% Start one bc_mcp_server per configured MCP server.
            %% Done here (not in the supervisor init) because simple_one_for_one
            %% children must be started via start_child after the sup is running.
            Servers = application:get_env(beamclaw_mcp, servers, []),
            lists:foreach(fun(Config) ->
                case bc_mcp_servers_sup:start_server(Config) of
                    {ok, _} -> ok;
                    {error, Reason} ->
                        logger:warning("[mcp] failed to start server ~p: ~p",
                                       [maps:get(name, Config, unknown), Reason])
                end
            end, Servers),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.
