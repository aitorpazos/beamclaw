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

-module(bc_memory_ets_search_tests).
-moduledoc "EUnit tests for BM25 search in bc_memory_ets.".

-include_lib("eunit/include/eunit.hrl").

%% ---- recall with query ----

recall_query_matches_test() ->
    {ok, State} = bc_memory_ets:init(#{}, <<"s1">>),
    {ok, S1} = bc_memory_ets:store(<<"k1">>, <<"erlang otp beam virtual machine">>, core, State),
    {ok, S2} = bc_memory_ets:store(<<"k2">>, <<"python django web framework">>, core, S1),
    {ok, S3} = bc_memory_ets:store(<<"k3">>, <<"erlang processes concurrency">>, core, S2),
    {ok, S4} = bc_memory_ets:store(<<"k4">>, <<"javascript react frontend">>, core, S3),
    {ok, S5} = bc_memory_ets:store(<<"k5">>, <<"golang channels goroutines">>, core, S4),
    {ok, Results, _} = bc_memory_ets:recall(<<"erlang processes">>, 10, all, S5),
    %% Should return entries containing "erlang" or "processes"
    Keys = [maps:get(key, E) || E <- Results],
    ?assert(lists:member(<<"k1">>, Keys)),
    ?assert(lists:member(<<"k3">>, Keys)),
    %% Should NOT return unrelated entries
    ?assertNot(lists:member(<<"k2">>, Keys)),
    ?assertNot(lists:member(<<"k4">>, Keys)),
    ?assertNot(lists:member(<<"k5">>, Keys)).

recall_empty_query_returns_all_test() ->
    {ok, State} = bc_memory_ets:init(#{}, <<"s2">>),
    {ok, S1} = bc_memory_ets:store(<<"k1">>, <<"hello">>, core, State),
    {ok, S2} = bc_memory_ets:store(<<"k2">>, <<"world">>, core, S1),
    {ok, Results, _} = bc_memory_ets:recall(<<>>, 10, all, S2),
    ?assertEqual(2, length(Results)).

recall_undefined_query_returns_all_test() ->
    {ok, State} = bc_memory_ets:init(#{}, <<"s3">>),
    {ok, S1} = bc_memory_ets:store(<<"k1">>, <<"test">>, core, State),
    {ok, Results, _} = bc_memory_ets:recall(undefined, 10, all, S1),
    ?assertEqual(1, length(Results)).

recall_query_with_category_filter_test() ->
    {ok, State} = bc_memory_ets:init(#{}, <<"s4">>),
    {ok, S1} = bc_memory_ets:store(<<"k1">>, <<"erlang programming language">>, core, State),
    {ok, S2} = bc_memory_ets:store(<<"k2">>, <<"erlang actor model">>, daily, S1),
    {ok, Results, _} = bc_memory_ets:recall(<<"erlang">>, 10, core, S2),
    ?assertEqual(1, length(Results)),
    ?assertEqual(<<"k1">>, maps:get(key, hd(Results))).

%% ---- search/4 ----

search_returns_scored_results_test() ->
    {ok, State} = bc_memory_ets:init(#{}, <<"s5">>),
    {ok, S1} = bc_memory_ets:store(<<"k1">>, <<"erlang otp beam">>, core, State),
    {ok, S2} = bc_memory_ets:store(<<"k2">>, <<"python django">>, core, S1),
    {ok, Results, _} = bc_memory_ets:search(<<"erlang beam">>, 10, #{}, S2),
    ?assertEqual(1, length(Results)),
    [{Score, Entry}] = Results,
    ?assert(Score > 0.0),
    ?assertEqual(<<"k1">>, maps:get(key, Entry)).

search_no_results_test() ->
    {ok, State} = bc_memory_ets:init(#{}, <<"s6">>),
    {ok, S1} = bc_memory_ets:store(<<"k1">>, <<"hello world">>, core, State),
    {ok, Results, _} = bc_memory_ets:search(<<"erlang">>, 10, #{}, S1),
    ?assertEqual([], Results).
