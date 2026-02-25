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

-module(bc_sandbox_env_tests).
-include_lib("eunit/include/eunit.hrl").

%% ---------------------------------------------------------------------------
%% filter_env/2 tests
%% ---------------------------------------------------------------------------

allowlist_filters_test() ->
    Env = [{"PATH", "/usr/bin"}, {"HOME", "/home/user"},
           {"SECRET_KEY", "s3cret"}, {"OPENAI_API_KEY", "sk-xxx"}],
    Config = #{allowlist => [<<"PATH">>, <<"HOME">>],
               blocklist => []},
    Result = bc_sandbox_env:filter_env(Env, Config),
    ?assertEqual([{"PATH", "/usr/bin"}, {"HOME", "/home/user"}], Result).

blocklist_removes_test() ->
    Env = [{"PATH", "/usr/bin"}, {"OPENAI_API_KEY", "sk-xxx"}],
    Config = #{allowlist => [<<"PATH">>, <<"OPENAI_API_KEY">>],
               blocklist => [<<"OPENAI_API_KEY">>]},
    Result = bc_sandbox_env:filter_env(Env, Config),
    ?assertEqual([{"PATH", "/usr/bin"}], Result).

empty_env_test() ->
    Config = #{allowlist => [<<"PATH">>], blocklist => []},
    ?assertEqual([], bc_sandbox_env:filter_env([], Config)).

allowlist_precedence_test() ->
    %% If not in allowlist, even non-blocked vars are excluded
    Env = [{"PATH", "/usr/bin"}, {"CUSTOM", "value"}],
    Config = #{allowlist => [<<"PATH">>], blocklist => []},
    Result = bc_sandbox_env:filter_env(Env, Config),
    ?assertEqual([{"PATH", "/usr/bin"}], Result).

%% ---------------------------------------------------------------------------
%% is_dangerous/1 tests
%% ---------------------------------------------------------------------------

is_dangerous_known_key_test() ->
    ?assert(bc_sandbox_env:is_dangerous(<<"OPENAI_API_KEY">>)),
    ?assert(bc_sandbox_env:is_dangerous(<<"AWS_SECRET_ACCESS_KEY">>)),
    ?assert(bc_sandbox_env:is_dangerous(<<"TELEGRAM_BOT_TOKEN">>)),
    ?assert(bc_sandbox_env:is_dangerous("GITHUB_TOKEN")).

is_dangerous_safe_key_test() ->
    ?assertNot(bc_sandbox_env:is_dangerous(<<"PATH">>)),
    ?assertNot(bc_sandbox_env:is_dangerous(<<"HOME">>)),
    ?assertNot(bc_sandbox_env:is_dangerous("LANG")).

is_dangerous_unknown_key_test() ->
    ?assertNot(bc_sandbox_env:is_dangerous(<<"MY_CUSTOM_VAR">>)).

%% ---------------------------------------------------------------------------
%% default lists tests
%% ---------------------------------------------------------------------------

default_allowlist_not_empty_test() ->
    ?assert(length(bc_sandbox_env:default_allowlist()) > 0).

default_blocklist_not_empty_test() ->
    ?assert(length(bc_sandbox_env:default_blocklist()) > 0).
