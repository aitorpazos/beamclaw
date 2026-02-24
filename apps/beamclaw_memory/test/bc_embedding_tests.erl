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

-module(bc_embedding_tests).
-moduledoc "EUnit tests for bc_embedding.".

-include_lib("eunit/include/eunit.hrl").

%% Test that embed returns error when no API key is configured
no_api_key_test() ->
    %% Ensure env vars are unset
    os:unsetenv("BEAMCLAW_EMBEDDING_API_KEY"),
    os:unsetenv("OPENAI_API_KEY"),
    ?assertEqual({error, no_api_key}, bc_embedding:embed(<<"hello">>)).

no_api_key_batch_test() ->
    os:unsetenv("BEAMCLAW_EMBEDDING_API_KEY"),
    os:unsetenv("OPENAI_API_KEY"),
    ?assertEqual({error, no_api_key}, bc_embedding:embed_batch([<<"hello">>])).

empty_input_test() ->
    ?assertEqual({error, empty_input}, bc_embedding:embed_batch([])).

is_configured_false_test() ->
    os:unsetenv("BEAMCLAW_EMBEDDING_API_KEY"),
    os:unsetenv("OPENAI_API_KEY"),
    ?assertEqual(false, bc_embedding:is_configured()).

is_configured_true_test() ->
    os:putenv("OPENAI_API_KEY", "sk-test-key"),
    ?assertEqual(true, bc_embedding:is_configured()),
    os:unsetenv("OPENAI_API_KEY").

is_configured_beamclaw_key_test() ->
    os:unsetenv("OPENAI_API_KEY"),
    os:putenv("BEAMCLAW_EMBEDDING_API_KEY", "test-key"),
    ?assertEqual(true, bc_embedding:is_configured()),
    os:unsetenv("BEAMCLAW_EMBEDDING_API_KEY").
