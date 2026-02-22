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

%% @doc Memory behaviour definition.
%%
%% All memory backends must implement these callbacks.
%% See bc_memory_ets and bc_memory_mnesia for reference implementations.
-module(bc_memory).

-type memory_category() :: core | daily | conversation | custom.
-export_type([memory_category/0]).

-callback init(Config :: map(), SessionId :: binary()) ->
    {ok, State :: term()} | {error, term()}.

-callback store(Key :: term(), Value :: term(), Category :: memory_category(), State :: term()) ->
    {ok, NewState :: term()} | {error, term()}.

-callback recall(Query :: term(), Limit :: pos_integer(),
                 Category :: memory_category() | all, State :: term()) ->
    {ok, Entries :: list(), NewState :: term()} | {error, term()}.

-callback get(Key :: term(), State :: term()) ->
    {ok, Entry :: term()} | {error, not_found | term()}.

-callback forget(Key :: term(), State :: term()) ->
    {ok, NewState :: term()} | {error, term()}.
