%% @doc ETS-backed memory backend. In-memory, per-session, lost on restart.
-module(bc_memory_ets).
-behaviour(bc_memory).

-export([init/2, store/4, recall/4, get/2, forget/2]).

init(_Config, SessionId) ->
    Tab = ets:new(list_to_atom("memory_" ++ binary_to_list(SessionId)),
                  [set, private]),
    {ok, #{tab => Tab, session_id => SessionId}}.

store(Key, Value, Category, #{tab := Tab} = State) ->
    Now = erlang:system_time(second),
    Entry = #{key => Key, value => Value, category => Category,
              created_at => Now, updated_at => Now},
    ets:insert(Tab, {Key, Entry}),
    {ok, State}.

recall(Query, Limit, Category, #{tab := Tab} = State) ->
    All = ets:tab2list(Tab),
    Filtered = [E || {_, E} <- All,
                     category_matches(maps:get(category, E), Category),
                     query_matches(Query, E)],
    {ok, lists:sublist(Filtered, Limit), State}.

get(Key, #{tab := Tab} = _State) ->
    case ets:lookup(Tab, Key) of
        [{Key, Entry}] -> {ok, Entry};
        []             -> {error, not_found}
    end.

forget(Key, #{tab := Tab} = State) ->
    ets:delete(Tab, Key),
    {ok, State}.

category_matches(_, all)      -> true;
category_matches(C, C)        -> true;
category_matches(_, _)        -> false.

query_matches(all, _Entry)    -> true;
query_matches(_Query, _Entry) -> true. %% TODO: implement full-text search
