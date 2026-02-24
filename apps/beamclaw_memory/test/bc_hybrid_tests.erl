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

-module(bc_hybrid_tests).
-moduledoc "EUnit tests for bc_hybrid.".

-include_lib("eunit/include/eunit.hrl").

bm25_only_fallback_test() ->
    BM25 = [{doc1, 2.0}, {doc2, 1.0}, {doc3, 0.5}],
    Results = bc_hybrid:merge(BM25, #{min_score => 0.0}),
    ?assertEqual(3, length(Results)),
    %% doc1 should rank first
    ?assertMatch([{doc1, _} | _], Results).

vector_weighting_test() ->
    BM25   = [{doc1, 1.0}, {doc2, 2.0}],
    Vector = [{doc1, 2.0}, {doc2, 1.0}],
    %% 70% vector + 30% BM25
    Results = bc_hybrid:merge(BM25, #{
        vector_scores => Vector,
        vector_weight => 0.7,
        bm25_weight   => 0.3,
        min_score     => 0.0
    }),
    %% doc1: 0.7*1.0 + 0.3*0.0 = 0.7 (BM25 normalized: doc1=0, doc2=1)
    %% doc2: 0.7*0.0 + 0.3*1.0 = 0.3 (Vector normalized: doc1=1, doc2=0)
    %% Wait - let me think again:
    %% BM25 normalized: doc1 → 0.0, doc2 → 1.0
    %% Vector normalized: doc1 → 1.0, doc2 → 0.0
    %% doc1: 0.7*1.0 + 0.3*0.0 = 0.7
    %% doc2: 0.7*0.0 + 0.3*1.0 = 0.3
    ?assertMatch([{doc1, _}, {doc2, _}], Results).

min_score_filter_test() ->
    BM25 = [{doc1, 2.0}, {doc2, 0.01}],
    Results = bc_hybrid:merge(BM25, #{min_score => 0.5}),
    %% doc1 normalized → 1.0 (passes), doc2 → 0.0 (filtered)
    ?assertEqual(1, length(Results)),
    ?assertMatch([{doc1, _}], Results).

limit_test() ->
    BM25 = [{doc1, 3.0}, {doc2, 2.0}, {doc3, 1.0}],
    Results = bc_hybrid:merge(BM25, #{min_score => 0.0, limit => 2}),
    ?assertEqual(2, length(Results)).

empty_bm25_test() ->
    Results = bc_hybrid:merge([], #{}),
    ?assertEqual([], Results).

single_score_normalize_test() ->
    BM25 = [{doc1, 5.0}],
    Results = bc_hybrid:merge(BM25, #{min_score => 0.0}),
    ?assertEqual(1, length(Results)),
    %% Single score normalizes to 1.0
    ?assertMatch([{doc1, 1.0}], Results).

hybrid_with_partial_overlap_test() ->
    %% BM25 has doc1 and doc2, vector only has doc1
    BM25   = [{doc1, 1.0}, {doc2, 2.0}],
    Vector = [{doc1, 3.0}],
    Results = bc_hybrid:merge(BM25, #{
        vector_scores => Vector,
        vector_weight => 0.7,
        bm25_weight   => 0.3,
        min_score     => 0.0
    }),
    %% All docs should be present
    Keys = [K || {K, _} <- Results],
    ?assert(lists:member(doc1, Keys)),
    ?assert(lists:member(doc2, Keys)).
