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

-module(bc_embedding_cache).
-moduledoc """
ETS-backed embedding cache with 24h TTL.

Cache key: {AgentId, Source, ContentHash} → embedding vector.
Pruned every 60 minutes via a periodic timer.
""".
-behaviour(gen_server).

-export([start_link/0, get/3, put/4, invalidate_agent/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(TABLE, bc_embedding_cache_tab).
-define(TTL_SECONDS, 86400).       %% 24 hours
-define(PRUNE_INTERVAL, 3600000).  %% 60 minutes

%% ---------------------------------------------------------------------------
%% Public API
%% ---------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-doc "Look up a cached embedding. Returns {ok, [float()]} or miss.".
-spec get(binary(), binary(), binary()) -> {ok, [float()]} | miss.
get(AgentId, Source, ContentHash) ->
    Key = {AgentId, Source, ContentHash},
    case ets:lookup(?TABLE, Key) of
        [{Key, Embedding, ExpiresAt}] ->
            case erlang:system_time(second) < ExpiresAt of
                true  -> {ok, Embedding};
                false ->
                    ets:delete(?TABLE, Key),
                    miss
            end;
        [] ->
            miss
    end.

-doc "Cache an embedding vector.".
-spec put(binary(), binary(), binary(), [float()]) -> ok.
put(AgentId, Source, ContentHash, Embedding) ->
    Key = {AgentId, Source, ContentHash},
    ExpiresAt = erlang:system_time(second) + ?TTL_SECONDS,
    ets:insert(?TABLE, {Key, Embedding, ExpiresAt}),
    ok.

-doc "Invalidate all cached embeddings for an agent.".
-spec invalidate_agent(binary()) -> ok.
invalidate_agent(AgentId) ->
    gen_server:cast(?SERVER, {invalidate_agent, AgentId}).

%% ---------------------------------------------------------------------------
%% gen_server callbacks
%% ---------------------------------------------------------------------------

init([]) ->
    ets:new(?TABLE, [named_table, set, public, {read_concurrency, true}]),
    schedule_prune(),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({invalidate_agent, AgentId}, State) ->
    %% Delete all entries where the first element of the key tuple matches AgentId
    ets:match_delete(?TABLE, {{AgentId, '_', '_'}, '_', '_'}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(prune, State) ->
    Now = erlang:system_time(second),
    %% Delete entries where ExpiresAt < Now
    ets:select_delete(?TABLE, [
        {{'_', '_', '$1'}, [{'<', '$1', Now}], [true]}
    ]),
    schedule_prune(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

schedule_prune() ->
    erlang:send_after(?PRUNE_INTERVAL, self(), prune).
