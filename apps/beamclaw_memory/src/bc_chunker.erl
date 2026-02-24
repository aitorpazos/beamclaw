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

-module(bc_chunker).
-moduledoc """
Split text into overlapping word-boundary chunks for embedding.
""".

-export([chunk/1, chunk/3]).

-define(DEFAULT_CHUNK_SIZE, 400).
-define(DEFAULT_OVERLAP, 80).

-doc "Chunk text with default settings (400 words, 80 overlap).".
-spec chunk(binary()) -> [binary()].
chunk(Text) ->
    chunk(Text, ?DEFAULT_CHUNK_SIZE, ?DEFAULT_OVERLAP).

-doc "Chunk text into overlapping segments of `Size` words with `Overlap` word overlap.".
-spec chunk(binary(), pos_integer(), non_neg_integer()) -> [binary()].
chunk(Text, Size, Overlap) when is_binary(Text), Size > 0, Overlap >= 0, Overlap < Size ->
    Words = split_words(Text),
    case Words of
        [] -> [];
        _ when length(Words) =< Size ->
            [Text];
        _ ->
            Step = Size - Overlap,
            chunk_loop(Words, Size, Step, 0, [])
    end;
chunk(_, _, _) ->
    [].

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

split_words(Text) ->
    Tokens = re:split(Text, <<"\\s+">>, [{return, binary}]),
    [T || T <- Tokens, T =/= <<>>].

chunk_loop(Words, Size, Step, Offset, Acc) ->
    Total = length(Words),
    case Offset >= Total of
        true ->
            lists:reverse(Acc);
        false ->
            ChunkWords = lists:sublist(Words, Offset + 1, Size),
            Chunk = iolist_to_binary(lists:join(<<" ">>, ChunkWords)),
            NextOffset = Offset + Step,
            case NextOffset >= Total of
                true ->
                    lists:reverse([Chunk | Acc]);
                false ->
                    chunk_loop(Words, Size, Step, NextOffset, [Chunk | Acc])
            end
    end.
