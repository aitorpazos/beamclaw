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

-module(bc_vector_tests).
-moduledoc "EUnit tests for bc_vector.".

-include_lib("eunit/include/eunit.hrl").

cosine_identical_test() ->
    V = [1.0, 2.0, 3.0],
    Sim = bc_vector:cosine_similarity(V, V),
    ?assert(Sim > 0.999).

cosine_orthogonal_test() ->
    A = [1.0, 0.0, 0.0],
    B = [0.0, 1.0, 0.0],
    Sim = bc_vector:cosine_similarity(A, B),
    ?assert(abs(Sim) < 0.001).

cosine_opposite_test() ->
    A = [1.0, 0.0],
    B = [-1.0, 0.0],
    Sim = bc_vector:cosine_similarity(A, B),
    ?assert(Sim < -0.999).

cosine_empty_test() ->
    ?assertEqual(0.0, bc_vector:cosine_similarity([], [])).

cosine_mismatched_length_test() ->
    ?assertEqual(0.0, bc_vector:cosine_similarity([1.0], [1.0, 2.0])).

cosine_zero_vector_test() ->
    A = [0.0, 0.0, 0.0],
    B = [1.0, 2.0, 3.0],
    ?assertEqual(0.0, bc_vector:cosine_similarity(A, B)).

dot_product_test() ->
    ?assertEqual(14.0, bc_vector:dot_product([1.0, 2.0, 3.0], [1.0, 2.0, 3.0])).

dot_product_zero_test() ->
    ?assertEqual(0.0, bc_vector:dot_product([1.0, 0.0], [0.0, 1.0])).

normalize_test() ->
    V = [3.0, 4.0],
    Norm = bc_vector:l2_normalize(V),
    ?assert(abs(bc_vector:magnitude(Norm) - 1.0) < 0.001).

normalize_zero_test() ->
    V = [0.0, 0.0],
    Norm = bc_vector:l2_normalize(V),
    ?assertEqual([0.0, 0.0], Norm).

magnitude_test() ->
    ?assert(abs(bc_vector:magnitude([3.0, 4.0]) - 5.0) < 0.001).
