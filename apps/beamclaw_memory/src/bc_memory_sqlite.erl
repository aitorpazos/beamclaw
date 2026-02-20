%% @doc SQLite-backed memory backend stub. Persists across restarts.
%% Requires an Erlang SQLite driver (e.g., esqlite or exqlite) as a dep.
-module(bc_memory_sqlite).
-behaviour(bc_memory).

-export([init/2, store/4, recall/4, get/2, forget/2]).

init(_Config, _SessionId) ->
    %% TODO: open SQLite database, create tables if needed
    {error, not_implemented}.

store(_Key, _Value, _Category, State) ->
    {ok, State}.

recall(_Query, _Limit, _Category, State) ->
    {ok, [], State}.

get(_Key, _State) ->
    {error, not_found}.

forget(_Key, State) ->
    {ok, State}.
