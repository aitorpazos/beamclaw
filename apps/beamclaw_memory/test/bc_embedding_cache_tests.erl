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

-module(bc_embedding_cache_tests).
-moduledoc "EUnit tests for bc_embedding_cache.".

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun setup/0, fun cleanup/1, fun F/1}).

setup() ->
    {ok, Pid} = bc_embedding_cache:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

put_get_test_() -> ?setup(put_get_t).
put_get_t(_Pid) ->
    Vec = [1.0, 2.0, 3.0],
    ok = bc_embedding_cache:put(<<"agent1">>, <<"source1">>, <<"hash1">>, Vec),
    Result = bc_embedding_cache:get(<<"agent1">>, <<"source1">>, <<"hash1">>),
    [?_assertEqual({ok, Vec}, Result)].

miss_test_() -> ?setup(miss_t).
miss_t(_Pid) ->
    Result = bc_embedding_cache:get(<<"agent1">>, <<"nosource">>, <<"nohash">>),
    [?_assertEqual(miss, Result)].

invalidate_agent_test_() -> ?setup(invalidate_agent_t).
invalidate_agent_t(_Pid) ->
    Vec1 = [1.0, 2.0],
    Vec2 = [3.0, 4.0],
    ok = bc_embedding_cache:put(<<"agent1">>, <<"s1">>, <<"h1">>, Vec1),
    ok = bc_embedding_cache:put(<<"agent2">>, <<"s2">>, <<"h2">>, Vec2),
    bc_embedding_cache:invalidate_agent(<<"agent1">>),
    %% Allow cast to be processed
    timer:sleep(50),
    R1 = bc_embedding_cache:get(<<"agent1">>, <<"s1">>, <<"h1">>),
    R2 = bc_embedding_cache:get(<<"agent2">>, <<"s2">>, <<"h2">>),
    [?_assertEqual(miss, R1),
     ?_assertEqual({ok, Vec2}, R2)].

content_hash_eviction_test_() -> ?setup(content_hash_eviction_t).
content_hash_eviction_t(_Pid) ->
    Vec1 = [1.0, 2.0],
    Vec2 = [3.0, 4.0],
    %% Store with hash1, then overwrite with hash2 for same source
    ok = bc_embedding_cache:put(<<"a1">>, <<"src">>, <<"hash1">>, Vec1),
    ok = bc_embedding_cache:put(<<"a1">>, <<"src">>, <<"hash2">>, Vec2),
    R1 = bc_embedding_cache:get(<<"a1">>, <<"src">>, <<"hash1">>),
    R2 = bc_embedding_cache:get(<<"a1">>, <<"src">>, <<"hash2">>),
    %% hash1 is still cached (different key), hash2 has new value
    [?_assertEqual({ok, Vec1}, R1),
     ?_assertEqual({ok, Vec2}, R2)].
