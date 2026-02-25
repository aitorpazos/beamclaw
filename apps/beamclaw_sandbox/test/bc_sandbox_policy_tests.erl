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

-module(bc_sandbox_policy_tests).
-include_lib("eunit/include/eunit.hrl").

%% ---------------------------------------------------------------------------
%% check/2 tests
%% ---------------------------------------------------------------------------

allow_exact_match_test() ->
    Rules = [{allow, <<"read_file">>}],
    ?assertEqual(allow, bc_sandbox_policy:check(<<"read_file">>, Rules)).

deny_exact_match_test() ->
    Rules = [{deny, <<"bash">>}],
    ?assertEqual(deny, bc_sandbox_policy:check(<<"bash">>, Rules)).

wildcard_match_test() ->
    Rules = [{allow, <<"mcp:*">>}],
    ?assertEqual(allow, bc_sandbox_policy:check(<<"mcp:weather">>, Rules)).

wildcard_no_match_test() ->
    Rules = [{allow, <<"mcp:*">>}],
    ?assertEqual(no_match, bc_sandbox_policy:check(<<"read_file">>, Rules)).

first_match_wins_test() ->
    Rules = [{deny, <<"bash">>}, {allow, <<"bash">>}],
    ?assertEqual(deny, bc_sandbox_policy:check(<<"bash">>, Rules)).

no_match_test() ->
    Rules = [{deny, <<"bash">>}],
    ?assertEqual(no_match, bc_sandbox_policy:check(<<"curl">>, Rules)).

empty_rules_test() ->
    ?assertEqual(no_match, bc_sandbox_policy:check(<<"anything">>, [])).

multiple_rules_test() ->
    Rules = [{allow, <<"read_file">>}, {allow, <<"curl">>},
             {deny, <<"bash">>}, {deny, <<"terminal">>}],
    ?assertEqual(allow, bc_sandbox_policy:check(<<"read_file">>, Rules)),
    ?assertEqual(allow, bc_sandbox_policy:check(<<"curl">>, Rules)),
    ?assertEqual(deny, bc_sandbox_policy:check(<<"bash">>, Rules)),
    ?assertEqual(deny, bc_sandbox_policy:check(<<"terminal">>, Rules)),
    ?assertEqual(no_match, bc_sandbox_policy:check(<<"jq">>, Rules)).

deny_exec_in_defaults_test() ->
    Rules = bc_sandbox_policy:default_rules(),
    ?assertEqual(deny, bc_sandbox_policy:check(<<"exec">>, Rules)).

wildcard_prefix_only_test() ->
    Rules = [{allow, <<"abc:*">>}],
    ?assertEqual(no_match, bc_sandbox_policy:check(<<"xabc:foo">>, Rules)).
