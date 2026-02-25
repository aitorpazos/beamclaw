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

-module(bc_sandbox_env).
-moduledoc """
Environment variable filtering for sandbox containers.

Ensures no secrets leak into the container via environment variables.
Two strategies:
  - Allowlist: only explicitly listed vars are passed through
  - Blocklist: explicitly listed vars are never passed through

The allowlist takes precedence. If an allowlist is configured, only
those vars are included. The blocklist is then applied as a safety net.

Tools that need API keys get them injected server-side by the bridge
dispatcher, never through the container environment.
""".

-export([filter_env/1, filter_env/2, default_allowlist/0, default_blocklist/0,
         is_dangerous/1]).

-doc """
Filter environment variables using application config.
Returns a list of {Key, Value} string tuples safe for the container.
""".
-spec filter_env(Env :: [{string(), string()}]) -> [{string(), string()}].
filter_env(Env) ->
    Allowlist = application:get_env(beamclaw_sandbox, env_allowlist,
                                    default_allowlist()),
    Blocklist = application:get_env(beamclaw_sandbox, env_blocklist,
                                    default_blocklist()),
    filter_env(Env, #{allowlist => Allowlist, blocklist => Blocklist}).

-doc """
Filter environment variables using explicit allowlist/blocklist.
Config: #{allowlist => [binary()], blocklist => [binary()]}
""".
-spec filter_env(Env :: [{string(), string()}],
                 Config :: map()) -> [{string(), string()}].
filter_env(Env, Config) ->
    Allowlist = maps:get(allowlist, Config, default_allowlist()),
    Blocklist = maps:get(blocklist, Config, default_blocklist()),
    AllowSet = sets:from_list([binary_to_list(B) || B <- Allowlist]),
    BlockSet = sets:from_list([binary_to_list(B) || B <- Blocklist]),
    lists:filter(fun({Key, _Value}) ->
        InAllow = sets:is_element(Key, AllowSet),
        InBlock = sets:is_element(Key, BlockSet),
        InAllow andalso (not InBlock)
    end, Env).

-doc "Default env var allowlist — safe variables to pass into containers.".
-spec default_allowlist() -> [binary()].
default_allowlist() ->
    [<<"PATH">>, <<"HOME">>, <<"LANG">>, <<"TERM">>,
     <<"LC_ALL">>, <<"TZ">>, <<"USER">>].

-doc "Default env var blocklist — dangerous variables that must never leak.".
-spec default_blocklist() -> [binary()].
default_blocklist() ->
    [<<"OPENROUTER_API_KEY">>,
     <<"OPENAI_API_KEY">>,
     <<"TELEGRAM_BOT_TOKEN">>,
     <<"AWS_SECRET_ACCESS_KEY">>,
     <<"AWS_ACCESS_KEY_ID">>,
     <<"GITHUB_TOKEN">>,
     <<"GH_TOKEN">>,
     <<"BEAMCLAW_EMBEDDING_API_KEY">>,
     <<"SLACK_TOKEN">>,
     <<"DISCORD_TOKEN">>,
     <<"DATABASE_URL">>,
     <<"REDIS_URL">>,
     <<"SECRET_KEY_BASE">>].

-doc "Check if an environment variable name is in the blocklist.".
-spec is_dangerous(VarName :: binary() | string()) -> boolean().
is_dangerous(VarName) when is_binary(VarName) ->
    is_dangerous(binary_to_list(VarName));
is_dangerous(VarName) when is_list(VarName) ->
    Blocklist = default_blocklist(),
    BlockSet = sets:from_list([binary_to_list(B) || B <- Blocklist]),
    sets:is_element(VarName, BlockSet).
