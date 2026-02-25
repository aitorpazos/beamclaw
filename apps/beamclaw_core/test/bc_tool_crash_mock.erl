-module(bc_tool_crash_mock).
-moduledoc "Mock tool that crashes on execute — for testing bc_loop crash resilience.".
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"crash_tool">>,
      description => <<"A tool that always crashes">>,
      parameters  => #{type => object, properties => #{}},
      source      => builtin}.

execute(_Args, _SessionRef, _Context) ->
    error(deliberate_crash).

requires_approval() -> false.

min_autonomy() -> read_only.
