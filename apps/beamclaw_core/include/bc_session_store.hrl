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

%% Mnesia table record for bc_session_stored.
%% Shared between bc_session_store (read/write) and beamclaw_core_app (table creation).
%%
%% The table name is `bc_session_stored`; the record tag is `bc_session_stored`.
%% Primary key is session_id (binary).
%% History is serialized via term_to_binary with a version tag for forward compat.

-record(bc_session_stored, {
    session_id  :: binary(),          %% primary key
    user_id     :: binary(),
    agent_id    :: binary(),
    autonomy    :: atom(),            %% autonomy_level()
    history     :: term(),            %% binary() on disk; list after deserialization
    created_at  :: non_neg_integer(),
    updated_at  :: non_neg_integer(),
    config      :: map()
}).
