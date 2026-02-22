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

%% @doc Pure path-resolution functions for agent workspaces.
%%
%% Placed in beamclaw_tools (not beamclaw_core) so that tools can resolve
%% workspace paths without introducing a dependency cycle
%% (beamclaw_core → beamclaw_tools already exists).
-module(bc_workspace_path).

-export([base_dir/0, agent_dir/1, bootstrap_file/2, memory_dir/1, daily_log_file/2]).

%% @doc Return the base directory for all agent workspaces.
-spec base_dir() -> string().
base_dir() ->
    case os:getenv("BEAMCLAW_HOME") of
        false ->
            Home = os:getenv("HOME"),
            filename:join([Home, ".beamclaw", "agents"]);
        Dir ->
            filename:join([Dir, "agents"])
    end.

%% @doc Return the directory for a specific agent.
-spec agent_dir(binary()) -> string().
agent_dir(AgentId) ->
    filename:join(base_dir(), binary_to_list(AgentId)).

%% @doc Return the full path to a bootstrap file for an agent.
-spec bootstrap_file(binary(), binary()) -> string().
bootstrap_file(AgentId, Filename) ->
    filename:join(agent_dir(AgentId), binary_to_list(Filename)).

%% @doc Return the memory subdirectory for daily logs.
-spec memory_dir(binary()) -> string().
memory_dir(AgentId) ->
    filename:join(agent_dir(AgentId), "memory").

%% @doc Return the full path to a daily log file.
%% Date is a <<"YYYY-MM-DD">> binary.
-spec daily_log_file(binary(), binary()) -> string().
daily_log_file(AgentId, Date) ->
    Filename = <<Date/binary, ".md">>,
    filename:join(memory_dir(AgentId), binary_to_list(Filename)).
