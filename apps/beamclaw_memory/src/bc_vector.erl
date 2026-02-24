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

-module(bc_vector).
-moduledoc """
Pure-function vector operations: cosine similarity, L2 normalize, dot product.
""".

-export([cosine_similarity/2, dot_product/2, l2_normalize/1, magnitude/1]).

-doc "Compute cosine similarity between two vectors. Returns 0.0 for zero-magnitude vectors.".
-spec cosine_similarity([float()], [float()]) -> float().
cosine_similarity(A, B) when length(A) =:= length(B), length(A) > 0 ->
    Dot = dot_product(A, B),
    MagA = magnitude(A),
    MagB = magnitude(B),
    Denom = MagA * MagB,
    case Denom == 0.0 of
        true  -> 0.0;
        false -> Dot / Denom
    end;
cosine_similarity(_, _) ->
    0.0.

-doc "Compute dot product of two vectors.".
-spec dot_product([float()], [float()]) -> float().
dot_product(A, B) when length(A) =:= length(B) ->
    lists:sum(lists:zipwith(fun(X, Y) -> X * Y end, A, B));
dot_product(_, _) ->
    0.0.

-doc "L2-normalize a vector to unit length. Returns zero vector for zero-magnitude input.".
-spec l2_normalize([float()]) -> [float()].
l2_normalize(V) ->
    Mag = magnitude(V),
    case Mag == 0.0 of
        true  -> [0.0 || _ <- V];
        false -> [X / Mag || X <- V]
    end.

-doc "Compute the L2 magnitude (Euclidean norm) of a vector.".
-spec magnitude([float()]) -> float().
magnitude(V) ->
    math:sqrt(lists:sum([X * X || X <- V])).
