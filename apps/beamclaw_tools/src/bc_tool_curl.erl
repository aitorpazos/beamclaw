%% @doc Built-in curl tool — makes HTTP requests.
-module(bc_tool_curl).
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"curl">>,
      description => <<"Make an HTTP request. Returns status code and body.">>,
      parameters  => #{
          type       => object,
          properties => #{
              url     => #{type => string},
              method  => #{type => string, enum => [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"PATCH">>]},
              headers => #{type => object},
              body    => #{type => string}
          },
          required   => [<<"url">>]
      },
      source => builtin}.

execute(#{<<"url">> := Url} = Args, _Session, _Context) ->
    Method  = maps:get(<<"method">>,  Args, <<"GET">>),
    Headers = maps:get(<<"headers">>, Args, #{}),
    Body    = maps:get(<<"body">>,    Args, <<>>),
    HList   = maps:to_list(Headers),
    Req     = {binary_to_list(Url), HList, "application/json", Body},
    MethodAtom = list_to_atom(string:lowercase(binary_to_list(Method))),
    case httpc:request(MethodAtom, Req, [], []) of
        {ok, {{_, StatusCode, _}, _RespHeaders, RespBody}} ->
            Result = iolist_to_binary(io_lib:format("~p\n~s", [StatusCode, RespBody])),
            {ok, Result};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

requires_approval() -> true.

min_autonomy() -> supervised.
