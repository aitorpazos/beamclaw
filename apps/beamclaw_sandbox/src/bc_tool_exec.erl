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

-module(bc_tool_exec).
-moduledoc """
Built-in tool: execute a script inside a Docker sandbox container.

The agent writes a script (Python or Bash) that runs inside an isolated
Docker container with access to BeamClaw tools via the bridge module.
The script can discover tools via `search_tools()`, call them via
`call_tool(name, **args)`, and process/filter results before returning
output. This achieves significant token reduction for data-heavy workflows.

Requires beamclaw_sandbox to be enabled in configuration.

Parameters:
  - script (required): Script content to execute
  - language (optional): "python" (default) or "bash"
  - timeout (optional): Override timeout in seconds
  - skill (optional): Load script from a saved skill by name
  - save_as (optional): Save the script as a skill after successful execution
""".
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"exec">>,
      description => <<"Execute a script in an isolated Docker sandbox with access to "
                       "BeamClaw tools via the bridge module. The script can discover "
                       "tools (search_tools), get schemas (get_tool), and call tools "
                       "(call_tool) — processing results locally before returning output. "
                       "Supports Python (default) and Bash.">>,
      parameters  => #{
          type       => object,
          properties => #{
              script   => #{type => string,
                           description => <<"Script content to execute. For Python, "
                                           "you can import beamclaw_bridge to access "
                                           "search_tools(), get_tool(), call_tool().">>},
              language => #{type => string,
                           description => <<"Script language: 'python' (default) or 'bash'">>,
                           default => <<"python">>},
              timeout  => #{type => integer,
                           description => <<"Timeout in seconds (default: from config)">>},
              skill    => #{type => string,
                           description => <<"Load script from a saved skill by name">>},
              save_as  => #{type => string,
                           description => <<"Save script as a skill after successful execution">>}
          },
          required   => []
      },
      source => builtin}.

execute(Args, SessionRef, Context) ->
    %% Check if sandbox is enabled
    case application:get_env(beamclaw_sandbox, enabled, false) of
        false ->
            {error, <<"Sandbox is not enabled. Set {beamclaw_sandbox, [{enabled, true}]} "
                      "in config and ensure Docker is available.">>};
        true ->
            do_execute(Args, SessionRef, Context)
    end.

requires_approval() -> true.

min_autonomy() -> supervised.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

do_execute(Args, SessionRef, Context) ->
    Script = resolve_script(Args),
    case Script of
        {error, Reason} ->
            {error, Reason};
        {ok, ScriptBin} ->
            Language = maps:get(<<"language">>, Args, <<"python">>),
            SessionId = element(2, SessionRef),  %% #bc_session_ref.session_id
            Scope = application:get_env(beamclaw_sandbox, scope, session),

            %% Build tool bridge function from Context
            ToolBridgeFn = make_tool_bridge(Context),

            %% Get or create sandbox for this session
            SandboxConfig = build_sandbox_config(SessionRef),
            case bc_sandbox_registry:get_or_create(SessionId, Scope, SandboxConfig) of
                {ok, SandboxPid} ->
                    case bc_sandbox:exec_script(SandboxPid, ScriptBin,
                                                Language, ToolBridgeFn) of
                        {ok, Output} ->
                            maybe_save_skill(Args, ScriptBin, Language),
                            {ok, Output};
                        {error, Reason} ->
                            {error, iolist_to_binary(
                                io_lib:format("Sandbox execution failed: ~p", [Reason]))}
                    end;
                {error, Reason} ->
                    {error, iolist_to_binary(
                        io_lib:format("Failed to create sandbox: ~p", [Reason]))}
            end
    end.

resolve_script(#{<<"script">> := Script}) when is_binary(Script), Script =/= <<>> ->
    {ok, Script};
resolve_script(#{<<"skill">> := SkillName}) when is_binary(SkillName) ->
    %% Try to load script from skill system
    case bc_sandbox_skills:load_script(SkillName) of
        {ok, ScriptBin} -> {ok, ScriptBin};
        {error, _} -> {error, <<"Skill not found: ", SkillName/binary>>}
    end;
resolve_script(_) ->
    {error, <<"Either 'script' or 'skill' parameter is required">>}.

make_tool_bridge(Context) ->
    case maps:get(tool_bridge_fn, Context, undefined) of
        undefined ->
            %% Fallback: create bridge from registries directly
            fun(Name, ToolArgs) ->
                case bc_tool_registry:lookup(Name) of
                    {ok, {Mod, _Def}} ->
                        %% Create a minimal session ref for tool execution
                        Mod:execute(ToolArgs, undefined, #{});
                    {error, not_found} ->
                        {error, <<"Tool not found: ", Name/binary>>}
                end
            end;
        Fn ->
            Fn
    end.

build_sandbox_config(_SessionRef) ->
    Image = application:get_env(beamclaw_sandbox, docker_image,
                                "beamclaw-sandbox:latest"),
    MemLimit = application:get_env(beamclaw_sandbox, memory_limit, "512m"),
    CpuLimit = application:get_env(beamclaw_sandbox, cpu_limit, "1.0"),
    Network = application:get_env(beamclaw_sandbox, network, none),
    WsMount = application:get_env(beamclaw_sandbox, workspace_mount, ro),
    BridgeSocketDir = application:get_env(beamclaw_sandbox, bridge_socket_dir,
                                          "/tmp/beamclaw-bridges"),
    #{docker_image => Image,
      memory_limit => MemLimit,
      cpu_limit => CpuLimit,
      network => Network,
      workspace_mount => WsMount,
      bridge_socket_dir => BridgeSocketDir}.

maybe_save_skill(#{<<"save_as">> := SkillName}, Script, Language)
  when is_binary(SkillName), SkillName =/= <<>> ->
    catch bc_sandbox_skills:save_script(SkillName, Script, Language);
maybe_save_skill(_, _, _) ->
    ok.
