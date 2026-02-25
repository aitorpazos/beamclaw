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

-module(bc_sandbox_discovery_tests).
-include_lib("eunit/include/eunit.hrl").

%% These tests require bc_tool_registry to be running
discovery_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun generate_index_t/1,
      fun generate_tool_dir_t/1,
      fun generate_full_t/1,
      fun index_contains_names_t/1,
      fun schema_is_valid_json_t/1,
      fun description_file_t/1]}.

setup() ->
    {ok, _} = bc_tool_registry:start_link(),
    TmpDir = "/tmp/bc_discovery_test_" ++
             integer_to_list(erlang:unique_integer([positive])),
    TmpDir.

cleanup(TmpDir) ->
    %% Remove temp directory
    os:cmd("rm -rf " ++ TmpDir),
    %% Stop registry
    case whereis(bc_tool_registry) of
        undefined -> ok;
        Pid ->
            unlink(Pid),
            Ref = erlang:monitor(process, Pid),
            exit(Pid, shutdown),
            receive {'DOWN', Ref, process, Pid, _} -> ok
            after 5000 -> ok
            end
    end.

generate_index_t(TmpDir) ->
    [fun() ->
        ok = bc_sandbox_discovery:generate_index(TmpDir),
        IndexPath = filename:join(TmpDir, "index.txt"),
        ?assert(filelib:is_file(IndexPath)),
        {ok, Content} = file:read_file(IndexPath),
        ?assert(byte_size(Content) > 0)
    end].

generate_tool_dir_t(TmpDir) ->
    [fun() ->
        Def = #{name => <<"test_tool">>,
                description => <<"A test tool">>,
                parameters => #{type => object}},
        ok = bc_sandbox_discovery:generate_tool_dir(TmpDir, {<<"test_tool">>, Def}),
        DescPath = filename:join([TmpDir, "test_tool", "description.txt"]),
        SchemaPath = filename:join([TmpDir, "test_tool", "schema.json"]),
        ?assert(filelib:is_file(DescPath)),
        ?assert(filelib:is_file(SchemaPath)),
        {ok, Desc} = file:read_file(DescPath),
        ?assertEqual(<<"A test tool">>, Desc)
    end].

generate_full_t(TmpDir) ->
    [fun() ->
        ok = bc_sandbox_discovery:generate(TmpDir),
        IndexPath = filename:join(TmpDir, "index.txt"),
        ?assert(filelib:is_file(IndexPath))
    end].

index_contains_names_t(TmpDir) ->
    [fun() ->
        ok = bc_sandbox_discovery:generate_index(TmpDir),
        {ok, Content} = file:read_file(filename:join(TmpDir, "index.txt")),
        %% Should contain built-in tool names
        ?assert(binary:match(Content, <<"bash">>) =/= nomatch),
        ?assert(binary:match(Content, <<"read_file">>) =/= nomatch)
    end].

schema_is_valid_json_t(TmpDir) ->
    [fun() ->
        Def = #{name => <<"json_test">>, description => <<"Test">>,
                parameters => #{type => object, properties => #{}}},
        ok = bc_sandbox_discovery:generate_tool_dir(TmpDir, {<<"json_test">>, Def}),
        {ok, Content} = file:read_file(filename:join([TmpDir, "json_test", "schema.json"])),
        %% Should be valid JSON
        Map = jsx:decode(Content, [return_maps]),
        ?assertEqual(<<"json_test">>, maps:get(<<"name">>, Map))
    end].

description_file_t(TmpDir) ->
    [fun() ->
        Def = #{name => <<"desc_test">>,
                description => <<"Multi-line\ndescription">>,
                parameters => #{}},
        ok = bc_sandbox_discovery:generate_tool_dir(TmpDir, {<<"desc_test">>, Def}),
        {ok, Content} = file:read_file(
            filename:join([TmpDir, "desc_test", "description.txt"])),
        ?assertEqual(<<"Multi-line\ndescription">>, Content)
    end].
