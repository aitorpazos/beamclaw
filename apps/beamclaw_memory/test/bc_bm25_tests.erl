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

-module(bc_bm25_tests).
-moduledoc "EUnit tests for bc_bm25.".

-include_lib("eunit/include/eunit.hrl").

%% ---- tokenize ----

tokenize_basic_test() ->
    Tokens = bc_bm25:tokenize(<<"Hello World! The fox is here.">>),
    ?assert(lists:member(<<"hello">>, Tokens)),
    ?assert(lists:member(<<"world">>, Tokens)),
    ?assert(lists:member(<<"fox">>, Tokens)),
    %% "the" and "is" are stopwords
    ?assertNot(lists:member(<<"the">>, Tokens)),
    ?assertNot(lists:member(<<"is">>, Tokens)).

tokenize_stopwords_removed_test() ->
    Tokens = bc_bm25:tokenize(<<"the quick brown fox is in the garden">>),
    ?assertNot(lists:member(<<"the">>, Tokens)),
    ?assertNot(lists:member(<<"is">>, Tokens)),
    ?assertNot(lists:member(<<"in">>, Tokens)),
    ?assert(lists:member(<<"quick">>, Tokens)),
    ?assert(lists:member(<<"brown">>, Tokens)),
    ?assert(lists:member(<<"fox">>, Tokens)),
    ?assert(lists:member(<<"garden">>, Tokens)).

tokenize_empty_test() ->
    ?assertEqual([], bc_bm25:tokenize(<<>>)),
    ?assertEqual([], bc_bm25:tokenize(undefined)).

tokenize_single_char_removed_test() ->
    %% Single-character tokens (length <= 1) should be removed
    Tokens = bc_bm25:tokenize(<<"I am a developer">>),
    ?assertNot(lists:member(<<"i">>, Tokens)),
    ?assertNot(lists:member(<<"a">>, Tokens)).

tokenize_punctuation_split_test() ->
    Tokens = bc_bm25:tokenize(<<"user-agent: Mozilla/5.0; foo_bar">>),
    ?assert(lists:member(<<"user">>, Tokens)),
    ?assert(lists:member(<<"agent">>, Tokens)),
    ?assert(lists:member(<<"mozilla">>, Tokens)),
    ?assert(lists:member(<<"foo">>, Tokens)),
    ?assert(lists:member(<<"bar">>, Tokens)).

%% ---- corpus_stats ----

corpus_stats_empty_test() ->
    Stats = bc_bm25:corpus_stats([]),
    ?assertEqual(0, maps:get(n, Stats)),
    ?assertEqual(0.0, maps:get(avg_dl, Stats)).

corpus_stats_single_doc_test() ->
    Stats = bc_bm25:corpus_stats([[<<"hello">>, <<"world">>]]),
    ?assertEqual(1, maps:get(n, Stats)),
    ?assertEqual(2.0, maps:get(avg_dl, Stats)),
    IDF = maps:get(idf, Stats),
    ?assert(maps:is_key(<<"hello">>, IDF)),
    ?assert(maps:is_key(<<"world">>, IDF)).

corpus_stats_idf_values_test() ->
    %% Two docs: "hello world" and "hello erlang"
    %% "hello" appears in both (DF=2), "world" in 1, "erlang" in 1
    Stats = bc_bm25:corpus_stats([
        [<<"hello">>, <<"world">>],
        [<<"hello">>, <<"erlang">>]
    ]),
    IDF = maps:get(idf, Stats),
    %% "hello" has higher DF → lower IDF
    ?assert(maps:get(<<"hello">>, IDF) < maps:get(<<"world">>, IDF)).

%% ---- score ----

score_zero_no_overlap_test() ->
    Stats = bc_bm25:corpus_stats([[<<"apple">>, <<"banana">>]]),
    S = bc_bm25:score([<<"cherry">>], [<<"apple">>, <<"banana">>], Stats),
    ?assertEqual(0.0, S).

score_positive_overlap_test() ->
    Stats = bc_bm25:corpus_stats([[<<"erlang">>, <<"otp">>, <<"beam">>],
                                   [<<"python">>, <<"django">>]]),
    S = bc_bm25:score([<<"erlang">>], [<<"erlang">>, <<"otp">>, <<"beam">>], Stats),
    ?assert(S > 0.0).

%% ---- rank ----

rank_ordering_test() ->
    Docs = [
        {doc1, <<"erlang otp beam virtual machine">>},
        {doc2, <<"python django web framework">>},
        {doc3, <<"erlang processes actors concurrency">>}
    ],
    Ranked = bc_bm25:rank(<<"erlang processes">>, Docs),
    %% doc3 should rank highest (has both "erlang" and "processes")
    ?assertMatch([{doc3, _} | _], Ranked),
    %% doc2 should not appear (no matching terms)
    Keys = [K || {K, _} <- Ranked],
    ?assertNot(lists:member(doc2, Keys)).

rank_empty_query_test() ->
    Docs = [{doc1, <<"hello world">>}],
    ?assertEqual([], bc_bm25:rank(<<>>, Docs)).

rank_empty_corpus_test() ->
    ?assertEqual([], bc_bm25:rank(<<"hello">>, [])).

rank_all_stopwords_query_test() ->
    Docs = [{doc1, <<"the quick brown fox">>}],
    %% Query of only stopwords should tokenize to [] → no results
    ?assertEqual([], bc_bm25:rank(<<"the and is">>, Docs)).
