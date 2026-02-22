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

-module(bc_tool).
-moduledoc """
Tool behaviour definition.

All tool implementations must export these callbacks.
Tools are registered in bc_tool_registry on startup.
""".

-type autonomy_level() :: read_only | supervised | full.
-export_type([autonomy_level/0]).

%% Return the tool definition (name, description, JSON schema for parameters).
-callback definition() -> map().

%% Execute the tool with the given arguments.
%% Session is a bc_session_ref record (opaque map in lower apps).
%% Returns {ok, ResultBinary} | {error, ErrorBinary}.
-callback execute(Args :: map(), Session :: term(), Context :: map()) ->
    {ok, binary()} | {error, binary()}.

%% Whether this tool requires user approval before execution.
-callback requires_approval() -> boolean().

%% Minimum autonomy level required to run this tool without approval.
-callback min_autonomy() -> autonomy_level().
