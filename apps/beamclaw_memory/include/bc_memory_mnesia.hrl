%% Mnesia table record for bc_memory_entries.
%% Shared between bc_memory_mnesia (read/write) and beamclaw_memory_app (table creation).
%% The table name is `bc_memory_entries`; the record tag is `bc_memory_entry_stored`.
%% Composite key {SessionId, UserKey} provides per-session isolation without
%% per-session table creation.

-record(bc_memory_entry_stored, {
    key        :: {SessionId :: binary(), Key :: term()},
    value      :: term(),
    category   :: core | daily | conversation | custom,
    created_at :: non_neg_integer(),
    updated_at :: non_neg_integer()
}).
