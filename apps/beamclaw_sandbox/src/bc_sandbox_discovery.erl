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

-module(bc_sandbox_discovery).
-moduledoc """
Tool discovery filesystem generation for sandbox containers.

Generates a /tools/ directory structure mountable into the container:
  /tools/index.txt              — one tool name per line
  /tools/<name>/description.txt — tool description
  /tools/<name>/schema.json     — full tool definition as JSON

Sources: bc_tool_registry (built-in tools) + MCP tools.
The directory is refreshed when tool registry changes.
""".

-export([generate/1, generate_index/1, generate_tool_dir/2]).

-doc """
Generate the full /tools/ directory tree at the given base path.
Returns ok or {error, Reason}.
""".
-spec generate(BasePath :: string()) -> ok | {error, term()}.
generate(BasePath) ->
    Tools = bc_tool_registry:list(),
    case generate_index(BasePath) of
        ok ->
            Results = [generate_tool_dir(BasePath, {Name, Def})
                       || {Name, _Mod, Def} <- Tools],
            case lists:filter(fun(R) -> R =/= ok end, Results) of
                []     -> ok;
                Errors -> {error, Errors}
            end;
        Error ->
            Error
    end.

-doc "Generate /tools/index.txt with one tool name per line.".
-spec generate_index(BasePath :: string()) -> ok | {error, term()}.
generate_index(BasePath) ->
    Tools = bc_tool_registry:list(),
    Names = [Name || {Name, _Mod, _Def} <- Tools],
    IndexPath = filename:join(BasePath, "index.txt"),
    ok = filelib:ensure_dir(IndexPath),
    Content = iolist_to_binary(lists:join(<<"\n">>, Names)),
    file:write_file(IndexPath, Content).

-doc "Generate /tools/<name>/description.txt and schema.json for a single tool.".
-spec generate_tool_dir(BasePath :: string(),
                        {Name :: binary(), Def :: map()}) ->
    ok | {error, term()}.
generate_tool_dir(BasePath, {Name, Def}) ->
    NameStr = binary_to_list(Name),
    ToolDir = filename:join(BasePath, NameStr),
    ok = filelib:ensure_dir(filename:join(ToolDir, "dummy")),
    DescPath = filename:join(ToolDir, "description.txt"),
    SchemaPath = filename:join(ToolDir, "schema.json"),
    Description = maps:get(description, Def, <<>>),
    SchemaJson = jsx:encode(Def),
    case file:write_file(DescPath, Description) of
        ok ->
            file:write_file(SchemaPath, SchemaJson);
        Error ->
            Error
    end.
