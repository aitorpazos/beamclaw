%% @doc Tool behaviour definition.
%%
%% All tool implementations must export these callbacks.
%% Tools are registered in bc_tool_registry on startup.
-module(bc_tool).

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
