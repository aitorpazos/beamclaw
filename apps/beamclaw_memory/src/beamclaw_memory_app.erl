-module(beamclaw_memory_app).
-behaviour(application).

-include("bc_memory_mnesia.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = init_mnesia(),
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
