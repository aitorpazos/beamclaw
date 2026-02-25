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

-module(bc_tool_exec_tests).
-include_lib("eunit/include/eunit.hrl").

%% ---------------------------------------------------------------------------
%% definition tests
%% ---------------------------------------------------------------------------

definition_has_name_test() ->
    Def = bc_tool_exec:definition(),
    ?assertEqual(<<"exec">>, maps:get(name, Def)).

definition_has_parameters_test() ->
    Def = bc_tool_exec:definition(),
    Params = maps:get(parameters, Def),
    Props = maps:get(properties, Params),
    ?assert(maps:is_key(script, Props)),
    ?assert(maps:is_key(language, Props)),
    ?assert(maps:is_key(timeout, Props)),
    ?assert(maps:is_key(skill, Props)),
    ?assert(maps:is_key(save_as, Props)).

definition_source_builtin_test() ->
    Def = bc_tool_exec:definition(),
    ?assertEqual(builtin, maps:get(source, Def)).

%% ---------------------------------------------------------------------------
%% approval / autonomy tests
%% ---------------------------------------------------------------------------

requires_approval_test() ->
    ?assertEqual(true, bc_tool_exec:requires_approval()).

min_autonomy_test() ->
    ?assertEqual(supervised, bc_tool_exec:min_autonomy()).

%% ---------------------------------------------------------------------------
%% execute tests (no Docker needed — sandbox disabled)
%% ---------------------------------------------------------------------------

execute_sandbox_disabled_test() ->
    %% With sandbox disabled (default), exec should return error
    application:set_env(beamclaw_sandbox, enabled, false),
    Args = #{<<"script">> => <<"print('hello')">>},
    {error, Msg} = bc_tool_exec:execute(Args, undefined, #{}),
    ?assert(binary:match(Msg, <<"not enabled">>) =/= nomatch).

execute_missing_script_test() ->
    application:set_env(beamclaw_sandbox, enabled, true),
    Args = #{},
    {error, Msg} = bc_tool_exec:execute(Args, undefined, #{}),
    ?assert(binary:match(Msg, <<"required">>) =/= nomatch),
    application:set_env(beamclaw_sandbox, enabled, false).

execute_skill_not_found_test() ->
    application:set_env(beamclaw_sandbox, enabled, true),
    Args = #{<<"skill">> => <<"nonexistent-skill">>},
    {error, Msg} = bc_tool_exec:execute(Args, undefined, #{}),
    ?assert(binary:match(Msg, <<"not found">>) =/= nomatch
            orelse binary:match(Msg, <<"Skill not found">>) =/= nomatch
            orelse binary:match(Msg, <<"Failed">>) =/= nomatch),
    application:set_env(beamclaw_sandbox, enabled, false).
