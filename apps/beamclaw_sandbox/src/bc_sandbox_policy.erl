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

-module(bc_sandbox_policy).
-moduledoc """
Tool access policy for sandbox bridge requests.

Determines whether a tool call from within the sandbox container should
be allowed or denied. Rules are evaluated in order; first match wins.

Pattern matching:
  - Exact name: <<"read_file">> matches only "read_file"
  - Wildcard suffix: <<"mcp:*">> matches any tool starting with "mcp:"
  - Default action: applied when no rule matches (allow or deny)

Default policy: allow built-in tools, deny MCP tools unless explicitly allowed.
""".

-export([check/2, check_with_config/1, default_rules/0]).

-doc """
Check whether a tool call is allowed by the given rules.

Rules is a list of {allow, Pattern} | {deny, Pattern} tuples.
Returns allow | deny.
""".
-spec check(ToolName :: binary(),
            Rules :: [{allow | deny, binary()}]) -> allow | deny.
check(ToolName, Rules) ->
    check_rules(ToolName, Rules).

-doc """
Check a tool using the application config policy.
Reads rules and default_action from beamclaw_sandbox config.
""".
-spec check_with_config(ToolName :: binary()) -> allow | deny.
check_with_config(ToolName) ->
    PolicyConfig = application:get_env(beamclaw_sandbox, policy, #{}),
    Rules = maps:get(rules, PolicyConfig, default_rules()),
    DefaultAction = maps:get(default_action, PolicyConfig, allow),
    case check_rules(ToolName, Rules) of
        no_match -> DefaultAction;
        Result   -> Result
    end.

-doc "Default policy rules: deny dangerous tools.".
-spec default_rules() -> [{allow | deny, binary()}].
default_rules() ->
    [{deny, <<"bash">>},
     {deny, <<"terminal">>},
     {deny, <<"exec">>}].

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

check_rules(_ToolName, []) ->
    no_match;
check_rules(ToolName, [{Action, Pattern} | Rest]) ->
    case matches(ToolName, Pattern) of
        true  -> Action;
        false -> check_rules(ToolName, Rest)
    end.

matches(Name, Pattern) ->
    case binary:last(Pattern) of
        $* ->
            %% Wildcard prefix match
            Prefix = binary:part(Pattern, 0, byte_size(Pattern) - 1),
            case binary:match(Name, Prefix) of
                {0, _} -> true;
                _      -> false
            end;
        _ ->
            %% Exact match
            Name =:= Pattern
    end.
