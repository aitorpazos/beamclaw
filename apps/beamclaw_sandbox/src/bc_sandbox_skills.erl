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

-module(bc_sandbox_skills).
-moduledoc """
Sandbox script persistence as skills.

Converts successful sandbox scripts into SKILL.md files compatible with
the BeamClaw skill system (M16-M17). Scripts are saved with metadata
`type: sandbox_script` and can be loaded by name from the exec tool.

SKILL.md format for sandbox scripts:
  ---
  name: <skill-name>
  description: Sandbox script (<language>)
  type: sandbox_script
  metadata: {"language": "<language>", "hash": "<sha256>"}
  ---
  <script content>
""".

-export([save_script/3, load_script/1, generate_skill_md/3, extract_script/1]).

-doc "Save a script as a skill in the global skills directory.".
-spec save_script(Name :: binary(), Script :: binary(),
                  Language :: binary()) -> ok | {error, term()}.
save_script(Name, Script, Language) ->
    Content = generate_skill_md(Name, Script, Language),
    SkillDir = skill_dir(Name),
    SkillPath = filename:join(SkillDir, "SKILL.md"),
    ok = filelib:ensure_dir(SkillPath),
    file:write_file(SkillPath, Content).

-doc "Load a script from a saved skill by name.".
-spec load_script(Name :: binary()) -> {ok, binary()} | {error, not_found}.
load_script(Name) ->
    SkillPath = filename:join(skill_dir(Name), "SKILL.md"),
    case file:read_file(SkillPath) of
        {ok, Content} ->
            case extract_script(Content) of
                {ok, Script} -> {ok, Script};
                error -> {error, not_found}
            end;
        {error, _} ->
            {error, not_found}
    end.

-doc "Generate SKILL.md content for a sandbox script.".
-spec generate_skill_md(Name :: binary(), Script :: binary(),
                        Language :: binary()) -> binary().
generate_skill_md(Name, Script, Language) ->
    Hash = script_hash(Script),
    Metadata = jsx:encode(#{<<"language">> => Language, <<"hash">> => Hash}),
    iolist_to_binary([
        <<"---\n">>,
        <<"name: ">>, Name, <<"\n">>,
        <<"description: Sandbox script (">>, Language, <<")\n">>,
        <<"type: sandbox_script\n">>,
        <<"metadata: ">>, Metadata, <<"\n">>,
        <<"---\n">>,
        Script
    ]).

-doc "Extract script content from a SKILL.md file (everything after frontmatter).".
-spec extract_script(Content :: binary()) -> {ok, binary()} | error.
extract_script(Content) ->
    case binary:match(Content, <<"---\n">>) of
        {Start, Len} ->
            Rest = binary:part(Content, Start + Len, byte_size(Content) - Start - Len),
            case binary:match(Rest, <<"---\n">>) of
                {Start2, Len2} ->
                    Script = binary:part(Rest, Start2 + Len2,
                                         byte_size(Rest) - Start2 - Len2),
                    {ok, Script};
                nomatch ->
                    error
            end;
        nomatch ->
            error
    end.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

skill_dir(Name) ->
    BaseDir = bc_workspace_path:base_dir(),
    NameStr = binary_to_list(Name),
    filename:join([BaseDir, "skills", NameStr]).

script_hash(Script) ->
    Digest = crypto:hash(sha256, Script),
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || <<B>> <= Digest]).
