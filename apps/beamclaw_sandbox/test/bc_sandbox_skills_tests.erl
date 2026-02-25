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

-module(bc_sandbox_skills_tests).
-include_lib("eunit/include/eunit.hrl").

%% ---------------------------------------------------------------------------
%% generate_skill_md tests
%% ---------------------------------------------------------------------------

generate_skill_md_format_test() ->
    Content = bc_sandbox_skills:generate_skill_md(
        <<"data-fetch">>,
        <<"import json\nprint('hello')">>,
        <<"python">>),
    ?assert(binary:match(Content, <<"---\n">>) =/= nomatch),
    ?assert(binary:match(Content, <<"name: data-fetch">>) =/= nomatch),
    ?assert(binary:match(Content, <<"type: sandbox_script">>) =/= nomatch),
    ?assert(binary:match(Content, <<"python">>) =/= nomatch),
    ?assert(binary:match(Content, <<"import json">>) =/= nomatch).

generate_skill_md_has_hash_test() ->
    Content = bc_sandbox_skills:generate_skill_md(
        <<"test">>, <<"script">>, <<"python">>),
    ?assert(binary:match(Content, <<"hash">>) =/= nomatch).

generate_skill_md_language_test() ->
    Content = bc_sandbox_skills:generate_skill_md(
        <<"test">>, <<"echo hi">>, <<"bash">>),
    ?assert(binary:match(Content, <<"bash">>) =/= nomatch).

%% ---------------------------------------------------------------------------
%% extract_script tests
%% ---------------------------------------------------------------------------

extract_script_valid_test() ->
    Input = <<"---\nname: test\ntype: sandbox_script\n---\nprint('hello')\n">>,
    {ok, Script} = bc_sandbox_skills:extract_script(Input),
    ?assertEqual(<<"print('hello')\n">>, Script).

extract_script_no_frontmatter_test() ->
    ?assertEqual(error, bc_sandbox_skills:extract_script(<<"no frontmatter">>)).

extract_script_unclosed_test() ->
    ?assertEqual(error, bc_sandbox_skills:extract_script(<<"---\nname: test\n">>)).

%% ---------------------------------------------------------------------------
%% roundtrip tests
%% ---------------------------------------------------------------------------

skill_md_roundtrip_test() ->
    Script = <<"import json\nresult = json.dumps({'key': 'value'})\nprint(result)">>,
    Content = bc_sandbox_skills:generate_skill_md(
        <<"json-test">>, Script, <<"python">>),
    {ok, Extracted} = bc_sandbox_skills:extract_script(Content),
    ?assertEqual(Script, Extracted).

%% ---------------------------------------------------------------------------
%% save/load tests (using temp directory)
%% ---------------------------------------------------------------------------

save_load_test() ->
    %% Override base_dir to temp location
    TmpDir = "/tmp/bc_skill_test_" ++
             integer_to_list(erlang:unique_integer([positive])),
    application:set_env(beamclaw_tools, workspace_base_dir, TmpDir),
    Script = <<"print('saved skill')">>,
    ok = bc_sandbox_skills:save_script(<<"my-skill">>, Script, <<"python">>),
    {ok, Loaded} = bc_sandbox_skills:load_script(<<"my-skill">>),
    ?assertEqual(Script, Loaded),
    %% Cleanup
    os:cmd("rm -rf " ++ TmpDir),
    application:unset_env(beamclaw_tools, workspace_base_dir).

load_not_found_test() ->
    ?assertEqual({error, not_found},
                 bc_sandbox_skills:load_script(<<"nonexistent-skill">>)).
