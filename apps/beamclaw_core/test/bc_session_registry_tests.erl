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

-module(bc_session_registry_tests).
-moduledoc "Tests for bc_session_registry — derive_session_id/2,3 + isolation modes.".

-include_lib("eunit/include/eunit.hrl").

%% ---- Fixtures ----

setup() ->
    application:set_env(beamclaw_core, session_sharing, shared, [{persistent, true}]),
    ok.

teardown(_) ->
    ok.

%% ---- Test suite ----

session_registry_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun deterministic/0,
        fun same_user_agent_same_id/0,
        fun different_user_different_id/0,
        fun different_agent_different_id/0,
        fun session_id_format/0,
        fun shared_mode_ignores_channel/0,
        fun per_channel_mode_differs/0,
        fun canonical_user_same_session_across_channels/0
    ]}.

deterministic() ->
    Id1 = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>),
    Id2 = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>),
    ?assertEqual(Id1, Id2).

same_user_agent_same_id() ->
    Id1 = bc_session_registry:derive_session_id(<<"local:bob">>, <<"mybot">>),
    Id2 = bc_session_registry:derive_session_id(<<"local:bob">>, <<"mybot">>),
    ?assertEqual(Id1, Id2).

different_user_different_id() ->
    IdA = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>),
    IdB = bc_session_registry:derive_session_id(<<"bob">>,   <<"default">>),
    ?assertNotEqual(IdA, IdB).

different_agent_different_id() ->
    IdA = bc_session_registry:derive_session_id(<<"alice">>, <<"agent1">>),
    IdB = bc_session_registry:derive_session_id(<<"alice">>, <<"agent2">>),
    ?assertNotEqual(IdA, IdB).

session_id_format() ->
    Id = bc_session_registry:derive_session_id(<<"test">>, <<"default">>),
    ?assert(is_binary(Id)),
    ?assertMatch(<<"session-", _/binary>>, Id),
    %% 16 bytes hex = 32 chars
    <<"session-", Hex/binary>> = Id,
    ?assertEqual(32, byte_size(Hex)).

shared_mode_ignores_channel() ->
    application:set_env(beamclaw_core, session_sharing, shared, [{persistent, true}]),
    Id1 = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>, tui),
    Id2 = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>, telegram),
    Id3 = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>, http),
    ?assertEqual(Id1, Id2),
    ?assertEqual(Id2, Id3).

per_channel_mode_differs() ->
    application:set_env(beamclaw_core, session_sharing, per_channel, [{persistent, true}]),
    Id1 = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>, tui),
    Id2 = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>, telegram),
    ?assertNotEqual(Id1, Id2),
    %% Reset to shared for other tests
    application:set_env(beamclaw_core, session_sharing, shared, [{persistent, true}]).

%% When BEAMCLAW_USER is set, all channels use the same unprefixed user_id.
%% This test verifies that the same canonical user_id (no channel prefix)
%% produces identical session IDs regardless of channel — the fix for
%% cross-channel session sharing.
canonical_user_same_session_across_channels() ->
    application:set_env(beamclaw_core, session_sharing, shared, [{persistent, true}]),
    %% Simulate canonical user: all channels pass "peter" with no prefix
    Canonical = <<"peter">>,
    Id1 = bc_session_registry:derive_session_id(Canonical, <<"default">>, tui),
    Id2 = bc_session_registry:derive_session_id(Canonical, <<"default">>, telegram),
    Id3 = bc_session_registry:derive_session_id(Canonical, <<"default">>, http),
    Id4 = bc_session_registry:derive_session_id(Canonical, <<"default">>, websocket),
    ?assertEqual(Id1, Id2),
    ?assertEqual(Id2, Id3),
    ?assertEqual(Id3, Id4).
