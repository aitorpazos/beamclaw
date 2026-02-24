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

-module(bc_memory_ets).
-moduledoc """
ETS-backed memory backend. In-memory, per-session, lost on restart.

State: #{tab => ets:tid(), session_id => binary()}

Each session gets its own private ETS table. The table is lost when the
owning process (bc_session) terminates. Use bc_memory_sqlite for persistence.
""".
-behaviour(bc_memory).

-export([init/2, store/4, recall/4, get/2, forget/2, search/4]).

%% ---------------------------------------------------------------------------
%% bc_memory callbacks
%% ---------------------------------------------------------------------------

init(_Config, SessionId) ->
    %% No 'named_table' option — the atom is a label only, not a global name.
    %% This avoids creating a new atom per session (atom leak).
    Tab = ets:new(bc_memory_ets, [set, private]),
    {ok, #{tab => Tab, session_id => SessionId}}.

store(Key, Value, Category, #{tab := Tab} = State) ->
    Now = erlang:system_time(second),
    %% Preserve created_at when updating an existing entry.
    CreatedAt = case ets:lookup(Tab, Key) of
        [{Key, Existing}] -> maps:get(created_at, Existing);
        []                -> Now
    end,
    Entry = #{key        => Key,
              value      => Value,
              category   => Category,
              created_at => CreatedAt,
              updated_at => Now},
    ets:insert(Tab, {Key, Entry}),
    {ok, State}.

recall(Query, Limit, Category, #{tab := Tab} = State) ->
    All = ets:tab2list(Tab),
    CatFiltered = [E || {_, E} <- All,
                        category_matches(maps:get(category, E), Category)],
    case is_meaningful_query(Query) of
        false ->
            %% No query: return most-recently-updated entries.
            Sorted = lists:sort(fun(A, B) ->
                maps:get(updated_at, A) >= maps:get(updated_at, B)
            end, CatFiltered),
            {ok, lists:sublist(Sorted, Limit), State};
        true ->
            %% BM25 keyword search.
            Docs = [{maps:get(key, E), to_searchable_text(E)} || E <- CatFiltered],
            Ranked = bc_bm25:rank(Query, Docs),
            ByKey = maps:from_list([{maps:get(key, E), E} || E <- CatFiltered]),
            Results = [maps:get(K, ByKey) || {K, _Score} <- lists:sublist(Ranked, Limit)],
            {ok, Results, State}
    end.

get(Key, #{tab := Tab}) ->
    case ets:lookup(Tab, Key) of
        [{Key, Entry}] -> {ok, Entry};
        []             -> {error, not_found}
    end.

forget(Key, #{tab := Tab} = State) ->
    ets:delete(Tab, Key),
    {ok, State}.

search(Query, Limit, _Options, #{tab := Tab} = State) ->
    All = ets:tab2list(Tab),
    Docs = [{maps:get(key, E), to_searchable_text(E)} || {_, E} <- All],
    Ranked = bc_bm25:rank(Query, Docs),
    ByKey = maps:from_list([{maps:get(key, E), E} || {_, E} <- All]),
    Results = [{Score, maps:get(K, ByKey)}
               || {K, Score} <- lists:sublist(Ranked, Limit)],
    {ok, Results, State}.

%% ---------------------------------------------------------------------------
%% Internal helpers
%% ---------------------------------------------------------------------------

category_matches(_C, all) -> true;
category_matches(C,  C)   -> true;
category_matches(_C, _)   -> false.

is_meaningful_query(<<>>)      -> false;
is_meaningful_query(undefined)  -> false;
is_meaningful_query(all)        -> false;
is_meaningful_query(B) when is_binary(B), byte_size(B) > 0 -> true;
is_meaningful_query(_)          -> false.

to_searchable_text(Entry) ->
    Key = maps:get(key, Entry, <<>>),
    Value = maps:get(value, Entry, <<>>),
    KeyBin = if is_binary(Key) -> Key; true -> iolist_to_binary(io_lib:format("~p", [Key])) end,
    ValBin = if is_binary(Value) -> Value; true -> iolist_to_binary(io_lib:format("~p", [Value])) end,
    <<KeyBin/binary, " ", ValBin/binary>>.
