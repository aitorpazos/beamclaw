%% @doc Mnesia-backed memory backend. Persists across restarts via disc_copies
%% (or ram_copies when no disc schema is present, e.g. in dev/test).
%%
%% A single global table `bc_memory_entries` is shared across all sessions.
%% Per-session isolation is achieved via a composite {SessionId, UserKey}
%% primary key, avoiding per-session table creation (dynamic atoms / schema
%% changes are undesirable).
%%
%% State: #{session_id => binary()}
%%
%% All reads and writes use dirty operations for speed, matching the trade-off
%% of the ETS backend. Upgrade to transactions if strict consistency is needed.
-module(bc_memory_mnesia).
-behaviour(bc_memory).

-include("bc_memory_mnesia.hrl").

-export([init/2, store/4, recall/4, get/2, forget/2]).

%% Mnesia dirty_match_object uses record fields set to the atom '_' as wildcards.
%% This is a Mnesia runtime convention that Dialyzer's type system cannot model,
%% so we suppress the resulting spurious warnings for the affected functions.
-dialyzer({nowarn_function, [recall/4, query_matches/2]}).

%% ---------------------------------------------------------------------------
%% bc_memory callbacks
%% ---------------------------------------------------------------------------

init(_Config, SessionId) ->
    {ok, #{session_id => SessionId}}.

store(Key, Value, Category, #{session_id := SessionId} = State) ->
    CompositeKey = {SessionId, Key},
    Now = erlang:system_time(second),
    Entry = case mnesia:dirty_read(bc_memory_entries, CompositeKey) of
        [Existing] ->
            Existing#bc_memory_entry_stored{
                value      = Value,
                category   = Category,
                updated_at = Now
            };
        [] ->
            #bc_memory_entry_stored{
                key        = CompositeKey,
                value      = Value,
                category   = Category,
                created_at = Now,
                updated_at = Now
            }
    end,
    ok = mnesia:dirty_write(bc_memory_entries, Entry),
    {ok, State}.

recall(Query, Limit, Category, #{session_id := SessionId} = State) ->
    CatWild = case Category of all -> '_'; _ -> Category end,
    Pattern = #bc_memory_entry_stored{
        key        = {SessionId, '_'},
        value      = '_',
        category   = CatWild,
        created_at = '_',
        updated_at = '_'
    },
    Matches  = mnesia:dirty_match_object(bc_memory_entries, Pattern),
    Filtered = [to_entry(E) || E <- Matches, query_matches(Query, E)],
    Sorted   = lists:sort(fun(A, B) ->
        maps:get(updated_at, A) >= maps:get(updated_at, B)
    end, Filtered),
    {ok, lists:sublist(Sorted, Limit), State}.

get(Key, #{session_id := SessionId} = _State) ->
    case mnesia:dirty_read(bc_memory_entries, {SessionId, Key}) of
        [E] -> {ok, to_entry(E)};
        []  -> {error, not_found}
    end.

forget(Key, #{session_id := SessionId} = State) ->
    ok = mnesia:dirty_delete(bc_memory_entries, {SessionId, Key}),
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Internal helpers
%% ---------------------------------------------------------------------------

%% Convert stored record to the map format used by all bc_memory backends.
%% The SessionId prefix is stripped from the key so callers see the original key.
to_entry(#bc_memory_entry_stored{
            key        = {_SessionId, Key},
            value      = Value,
            category   = Category,
            created_at = CreatedAt,
            updated_at = UpdatedAt}) ->
    #{key        => Key,
      value      => Value,
      category   => Category,
      created_at => CreatedAt,
      updated_at => UpdatedAt}.

%% Full-text query matching is not implemented for the Mnesia backend;
%% all entries pass. The category filter is applied via the match pattern above.
query_matches(_, _) -> true.
