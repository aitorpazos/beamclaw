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

%% @doc LLM provider behaviour.
%%
%% All provider implementations must export these callbacks.
%% Providers run as gen_servers (bc_provider_openrouter, bc_provider_openai)
%% started transiently within each bc_session_sup.
-module(bc_provider).

-include_lib("beamclaw_core/include/bc_types.hrl").

%% Initialise provider state from config.
-callback init(Config :: map()) -> {ok, State :: term()} | {error, term()}.

%% Synchronous completion — blocks until the LLM responds.
-callback complete(Messages :: [#bc_message{}], Options :: map(), State :: term()) ->
    {ok, #bc_message{}, NewState :: term()} | {error, Reason :: term(), NewState :: term()}.

%% Streaming completion — sends chunks to CallerPid:
%%   {stream_chunk, self(), Chunk :: binary()}
%%   {stream_done,  self(), FullMessage :: #bc_message{}}
%%   {stream_error, self(), Reason :: term()}
-callback stream(Messages :: [#bc_message{}], Options :: map(),
                 CallerPid :: pid(), State :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term(), NewState :: term()}.

%% Return provider capabilities (e.g., supports_streaming, supports_tools).
-callback capabilities(State :: term()) -> map().

-callback terminate(Reason :: term(), State :: term()) -> ok.
