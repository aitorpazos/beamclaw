%% @doc Built-in bash tool — runs a bash script (multi-line supported).
-module(bc_tool_bash).
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"bash">>,
      description => <<"Execute a bash script. Supports multi-line scripts.">>,
      parameters  => #{
          type       => object,
          properties => #{
              script => #{type => string, description => <<"Bash script content">>}
          },
          required   => [<<"script">>]
      },
      source => builtin}.

execute(#{<<"script">> := Script}, _Session, _Context) ->
    %% Write script to temp file, execute with bash, return output
    TmpFile = "/tmp/bc_bash_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:write_file(TmpFile, Script),
    Output = os:cmd("bash " ++ TmpFile ++ " 2>&1"),
    _ = file:delete(TmpFile),
    {ok, iolist_to_binary(Output)}.

requires_approval() -> true.

min_autonomy() -> supervised.
