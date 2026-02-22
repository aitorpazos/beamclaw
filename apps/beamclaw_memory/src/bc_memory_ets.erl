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

-export([init/2, store/4, recall/4, get/2, forget/2]).

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
    Filtered = [E || {_, E} <- All,
                     category_matches(maps:get(category, E), Category),
                     query_matches(Query, E)],
    %% Return most-recently-updated entries first.
    Sorted = lists:sort(fun(A, B) ->
        maps:get(updated_at, A) >= maps:get(updated_at, B)
    end, Filtered),
    {ok, lists:sublist(Sorted, Limit), State}.

get(Key, #{tab := Tab}) ->
    case ets:lookup(Tab, Key) of
        [{Key, Entry}] -> {ok, Entry};
        []             -> {error, not_found}
    end.

forget(Key, #{tab := Tab} = State) ->
    ets:delete(Tab, Key),
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Internal helpers
%% ---------------------------------------------------------------------------

category_matches(_C, all) -> true;
category_matches(C,  C)   -> true;
category_matches(_C, _)   -> false.

%% Full-text search is not implemented for the ETS backend; all entries pass.
query_matches(_, _) -> true.
