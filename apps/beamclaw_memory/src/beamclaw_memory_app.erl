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

-module(beamclaw_memory_app).
-behaviour(application).

-include("bc_memory_mnesia.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = init_mnesia(),
    ok = maybe_transform_table(),
    beamclaw_memory_sup:start_link().

stop(_State) ->
    ok.

%% ---------------------------------------------------------------------------
%% Mnesia initialisation
%% ---------------------------------------------------------------------------

%% Ensure Mnesia is running and the bc_memory_entries table exists.
%%
%% Schema creation must happen before mnesia:start/0 — but if Mnesia is already
%% running (e.g. listed in `applications` or started in a shell), we skip that
%% step and go straight to table creation.
%%
%% Storage type is disc_copies when a disc schema exists (production), otherwise
%% ram_copies (dev/test shell without schema setup).
init_mnesia() ->
    case mnesia:system_info(is_running) of
        yes ->
            ensure_tables();
        _ ->
            case mnesia:create_schema([node()]) of
                ok                                -> ok;
                {error, {_, {already_exists, _}}} -> ok
            end,
            ok = mnesia:start(),
            ensure_tables()
    end.

ensure_tables() ->
    StorageType = case mnesia:system_info(use_dir) of
        true  -> disc_copies;
        false -> ram_copies
    end,
    case mnesia:create_table(bc_memory_entries, [
            {attributes, record_info(fields, bc_memory_entry_stored)},
            {StorageType, [node()]},
            {type, set}]) of
        {atomic, ok}                   -> ok;
        {aborted, {already_exists, _}} -> ok
    end.

%% Add the `embedding` column to existing tables created before M22.
%% mnesia:transform_table/3 is idempotent when the attributes already match.
maybe_transform_table() ->
    Expected = record_info(fields, bc_memory_entry_stored),
    Current  = mnesia:table_info(bc_memory_entries, attributes),
    case Current =:= Expected of
        true  -> ok;
        false ->
            Transform = fun({bc_memory_entry_stored, Key, Value, Cat, CreatedAt, UpdatedAt}) ->
                {bc_memory_entry_stored, Key, Value, Cat, CreatedAt, UpdatedAt, undefined}
            end,
            {atomic, ok} = mnesia:transform_table(bc_memory_entries, Transform, Expected),
            ok
    end.
