%% @doc Built-in jq tool — runs jq filter on JSON input.
-module(bc_tool_jq).
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"jq">>,
      description => <<"Apply a jq filter to a JSON string. Requires jq on PATH.">>,
      parameters  => #{
          type       => object,
          properties => #{
              filter => #{type => string, description => <<"jq filter expression">>},
              input  => #{type => string, description => <<"JSON string to process">>}
          },
          required   => [<<"filter">>, <<"input">>]
      },
      source => builtin}.

execute(#{<<"filter">> := Filter, <<"input">> := Input}, _Session, _Context) ->
    TmpIn = "/tmp/bc_jq_in_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:write_file(TmpIn, Input),
    Cmd = io_lib:format("jq ~p ~s 2>&1", [binary_to_list(Filter), TmpIn]),
    Output = os:cmd(lists:flatten(Cmd)),
    file:delete(TmpIn),
    {ok, iolist_to_binary(Output)}.

requires_approval() -> false.

min_autonomy() -> read_only.
