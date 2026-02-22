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

%% @doc EUnit tests for bc_skill_installer.
-module(bc_skill_installer_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

make_skill(Metadata) ->
    #bc_skill{
        name = <<"test">>,
        description = <<"test skill">>,
        content = <<"content">>,
        source = global,
        metadata = Metadata,
        path = "/tmp/test"
    }.

%% ---- no install specs ----

no_specs_test() ->
    Skill = make_skill(#{}),
    ?assertEqual({error, no_install_specs}, bc_skill_installer:install(Skill)).

%% ---- available_install_specs returns empty for no metadata ----

no_specs_available_test() ->
    Skill = make_skill(#{}),
    ?assertEqual([], bc_skill_installer:available_install_specs(Skill)).

%% ---- available_install_specs filters by compatibility ----

filter_compatible_test() ->
    Skill = make_skill(#{<<"beamclaw">> => #{
        <<"install">> => [
            #{<<"kind">> => <<"apt">>, <<"package">> => <<"jq">>},
            #{<<"kind">> => <<"nonexistent_manager">>, <<"package">> => <<"foo">>}
        ]
    }}),
    Specs = bc_skill_installer:available_install_specs(Skill),
    %% nonexistent_manager should be filtered out; apt may or may not be present
    lists:foreach(fun(S) ->
        Kind = maps:get(<<"kind">>, S),
        ?assertNotEqual(<<"nonexistent_manager">>, Kind)
    end, Specs).

%% ---- invalid install specs ----

invalid_specs_test() ->
    Skill = make_skill(#{<<"beamclaw">> => #{
        <<"install">> => not_a_list
    }}),
    ?assertEqual({error, invalid_install_specs}, bc_skill_installer:install(Skill)).

%% ---- empty install list ----

empty_install_list_test() ->
    Skill = make_skill(#{<<"beamclaw">> => #{
        <<"install">> => []
    }}),
    ?assertEqual({error, no_install_specs}, bc_skill_installer:install(Skill)).
