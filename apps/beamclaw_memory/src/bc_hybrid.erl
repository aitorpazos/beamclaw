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

-module(bc_hybrid).
-moduledoc """
Merge BM25 and vector similarity scores with configurable weights.

Algorithm (matching OpenClaw's hybrid.ts):
1. Min-max normalize each score set to [0, 1]
2. Merge: Score = VectorWeight * VectorScore + BM25Weight * BM25Score
3. Filter by min_score, return top limit results
4. Fallback: if no vector scores, use pure BM25 (weight 1.0)
""".

-export([merge/2]).

-doc """
Merge BM25 and vector scores.

Options:
  bm25_scores    :: [{Key, float()}]  — required
  vector_scores  :: [{Key, float()}]  — optional, empty means BM25-only
  vector_weight  :: float()           — default 0.7
  bm25_weight    :: float()           — default 0.3
  min_score      :: float()           — default 0.35
  limit          :: pos_integer()     — default 6

Returns [{Key, Score}] sorted by score descending.
""".
-spec merge([{term(), float()}], map()) -> [{term(), float()}].
merge(BM25Scores, Options) ->
    VectorScores = maps:get(vector_scores, Options, []),
    MinScore     = maps:get(min_score, Options, 0.35),
    Limit        = maps:get(limit, Options, 6),
    case VectorScores of
        [] ->
            %% Pure BM25 fallback
            Normalized = min_max_normalize(BM25Scores),
            Filtered = [{K, S} || {K, S} <- Normalized, S >= MinScore],
            Sorted = lists:sort(fun({_, A}, {_, B}) -> A >= B end, Filtered),
            lists:sublist(Sorted, Limit);
        _ ->
            VectorWeight = maps:get(vector_weight, Options, 0.7),
            BM25Weight   = maps:get(bm25_weight, Options, 0.3),
            NormBM25     = min_max_normalize(BM25Scores),
            NormVector   = min_max_normalize(VectorScores),
            BM25Map   = maps:from_list(NormBM25),
            VectorMap = maps:from_list(NormVector),
            AllKeys = lists:usort([K || {K, _} <- NormBM25] ++
                                  [K || {K, _} <- NormVector]),
            Merged = [{K, VectorWeight * maps:get(K, VectorMap, 0.0) +
                          BM25Weight * maps:get(K, BM25Map, 0.0)}
                      || K <- AllKeys],
            Filtered = [{K, S} || {K, S} <- Merged, S >= MinScore],
            Sorted = lists:sort(fun({_, A}, {_, B}) -> A >= B end, Filtered),
            lists:sublist(Sorted, Limit)
    end.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

min_max_normalize([]) -> [];
min_max_normalize([{K, S}]) -> [{K, if S > 0.0 -> 1.0; true -> 0.0 end}];
min_max_normalize(Scores) ->
    Values = [S || {_, S} <- Scores],
    Min = lists:min(Values),
    Max = lists:max(Values),
    Range = Max - Min,
    case Range == 0.0 of
        true ->
            %% All scores are equal
            case Max > 0.0 of
                true  -> [{K, 1.0} || {K, _} <- Scores];
                false -> [{K, 0.0} || {K, _} <- Scores]
            end;
        false ->
            [{K, (S - Min) / Range} || {K, S} <- Scores]
    end.
