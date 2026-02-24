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

-module(bc_bm25).
-moduledoc """
Pure-function BM25 keyword search.

Tokenize text, compute TF-IDF corpus statistics, and score documents using the
BM25 ranking function (k1=1.2, b=0.75). No external dependencies.
""".

-export([tokenize/1, rank/2, corpus_stats/1, score/3]).

-define(K1, 1.2).
-define(B, 0.75).

-define(STOPWORDS, [
    <<"a">>, <<"an">>, <<"and">>, <<"are">>, <<"as">>, <<"at">>, <<"be">>,
    <<"but">>, <<"by">>, <<"for">>, <<"if">>, <<"in">>, <<"into">>, <<"is">>,
    <<"it">>, <<"no">>, <<"not">>, <<"of">>, <<"on">>, <<"or">>, <<"such">>,
    <<"that">>, <<"the">>, <<"their">>, <<"then">>, <<"there">>, <<"these">>,
    <<"they">>, <<"this">>, <<"to">>, <<"was">>, <<"will">>, <<"with">>
]).

-doc "Tokenize text: lowercase, split on non-alphanumeric, remove stopwords.".
-spec tokenize(binary()) -> [binary()].
tokenize(Text) when is_binary(Text) ->
    Lower = string:lowercase(Text),
    LowerBin = unicode:characters_to_binary(Lower),
    Tokens = re:split(LowerBin, <<"[^a-z0-9]+">>, [{return, binary}]),
    [T || T <- Tokens, T =/= <<>>, byte_size(T) > 1,
     not lists:member(T, ?STOPWORDS)];
tokenize(_) ->
    [].

-doc """
Rank documents against a query using BM25.

Takes a query binary and a list of `{Key, Text}` pairs. Returns
`[{Key, Score}]` sorted by score descending, with zero-score entries removed.
""".
-spec rank(binary(), [{term(), binary()}]) -> [{term(), float()}].
rank(Query, Docs) when is_binary(Query), is_list(Docs) ->
    QueryTokens = tokenize(Query),
    case QueryTokens of
        [] -> [];
        _ ->
            DocTokenized = [{Key, tokenize(Text)} || {Key, Text} <- Docs],
            AllTokenLists = [Tokens || {_, Tokens} <- DocTokenized],
            Stats = corpus_stats(AllTokenLists),
            Scored = [{Key, score(QueryTokens, DocTokens, Stats)}
                      || {Key, DocTokens} <- DocTokenized],
            Filtered = [{K, S} || {K, S} <- Scored, S > 0.0],
            lists:sort(fun({_, A}, {_, B}) -> A >= B end, Filtered)
    end;
rank(_, _) ->
    [].

-doc """
Compute corpus statistics: IDF map, average document length, document count.
""".
-spec corpus_stats([[binary()]]) -> map().
corpus_stats(DocTokenLists) ->
    N = length(DocTokenLists),
    case N of
        0 ->
            #{idf => #{}, avg_dl => 0.0, n => 0};
        _ ->
            TotalLen = lists:sum([length(Tokens) || Tokens <- DocTokenLists]),
            AvgDl = TotalLen / N,
            %% Compute document frequency for each term
            DF = lists:foldl(fun(Tokens, Acc) ->
                Unique = lists:usort(Tokens),
                lists:foldl(fun(T, A) ->
                    maps:update_with(T, fun(V) -> V + 1 end, 1, A)
                end, Acc, Unique)
            end, #{}, DocTokenLists),
            %% Compute IDF for each term
            IDF = maps:map(fun(_Term, DocFreq) ->
                math:log((N - DocFreq + 0.5) / (DocFreq + 0.5) + 1.0)
            end, DF),
            #{idf => IDF, avg_dl => AvgDl, n => N}
    end.

-doc "Compute BM25 score for a single document against query tokens.".
-spec score([binary()], [binary()], map()) -> float().
score(QueryTokens, DocTokens, #{idf := IDF, avg_dl := AvgDl}) ->
    DL = length(DocTokens),
    %% Build term frequency map for this document
    TF = lists:foldl(fun(T, Acc) ->
        maps:update_with(T, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, DocTokens),
    lists:foldl(fun(QTerm, Sum) ->
        TermIDF = maps:get(QTerm, IDF, 0.0),
        TermTF  = maps:get(QTerm, TF, 0),
        case TermTF of
            0 -> Sum;
            _ ->
                Numerator   = TermTF * (?K1 + 1),
                Denominator = TermTF + ?K1 * (1 - ?B + ?B * (DL / max(AvgDl, 0.001))),
                Sum + TermIDF * (Numerator / Denominator)
        end
    end, 0.0, QueryTokens);
score(_, _, _) ->
    0.0.
