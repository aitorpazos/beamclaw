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

-module(bc_chunker_tests).
-moduledoc "EUnit tests for bc_chunker.".

-include_lib("eunit/include/eunit.hrl").

short_text_single_chunk_test() ->
    Text = <<"Hello world this is a short text.">>,
    Chunks = bc_chunker:chunk(Text, 400, 80),
    ?assertEqual(1, length(Chunks)),
    ?assertEqual(Text, hd(Chunks)).

empty_text_test() ->
    ?assertEqual([], bc_chunker:chunk(<<>>, 400, 80)).

exact_chunk_size_test() ->
    %% 5 words, chunk size 5 → single chunk
    Text = <<"one two three four five">>,
    ?assertEqual(1, length(bc_chunker:chunk(Text, 5, 2))).

overlap_correctness_test() ->
    %% 9 words, chunk size 4, overlap 1 → step = 3
    %% Offsets: 0, 3, 6 → Chunks: [1-4], [4-7], [7-9]
    Text = <<"w1 w2 w3 w4 w5 w6 w7 w8 w9">>,
    Chunks = bc_chunker:chunk(Text, 4, 1),
    ?assertEqual(3, length(Chunks)),
    %% First chunk should start with w1
    ?assert(binary:match(hd(Chunks), <<"w1">>) =/= nomatch),
    %% Second chunk should contain w4 (overlap)
    ?assert(binary:match(lists:nth(2, Chunks), <<"w4">>) =/= nomatch),
    %% Third chunk should contain w9
    ?assert(binary:match(lists:nth(3, Chunks), <<"w9">>) =/= nomatch).

default_chunk_test() ->
    %% Short text with default params should return single chunk
    Text = <<"This is a test sentence.">>,
    ?assertEqual(1, length(bc_chunker:chunk(Text))).

word_boundaries_test() ->
    %% Ensure chunks split on word boundaries, not mid-word
    Text = <<"The quick brown fox jumped over the lazy sleeping dog">>,
    Chunks = bc_chunker:chunk(Text, 4, 1),
    lists:foreach(fun(Chunk) ->
        %% No chunk should start or end with a space (after trim)
        Trimmed = string:trim(Chunk),
        ?assertEqual(Trimmed, Chunk)
    end, Chunks).
