%% @doc ETS-backed memory backend. In-memory, per-session, lost on restart.
%%
%% State: #{tab => ets:tid(), session_id => binary()}
%%
%% Each session gets its own private ETS table. The table is lost when the
%% owning process (bc_session) terminates. Use bc_memory_sqlite for persistence.
-module(bc_memory_ets).
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
