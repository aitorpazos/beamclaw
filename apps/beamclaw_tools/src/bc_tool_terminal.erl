%% @doc Built-in terminal tool — runs a single shell command.
-module(bc_tool_terminal).
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"terminal">>,
      description => <<"Run a single shell command and return stdout+stderr.">>,
      parameters  => #{
          type       => object,
          properties => #{
              command => #{type => string, description => <<"Shell command to execute">>}
          },
          required   => [<<"command">>]
      },
      source => builtin}.

execute(#{<<"command">> := Cmd}, _Session, _Context) ->
    SafeCmd = binary_to_list(Cmd),
    case catch os:cmd(SafeCmd) of
        {'EXIT', Reason} ->
            {error, iolist_to_binary(io_lib:format("exit: ~p", [Reason]))};
        Output ->
            {ok, iolist_to_binary(Output)}
    end.

requires_approval() -> true.

min_autonomy() -> supervised.
