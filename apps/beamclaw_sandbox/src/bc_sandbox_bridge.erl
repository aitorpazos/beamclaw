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

-module(bc_sandbox_bridge).
-moduledoc """
JSON-RPC 2.0 bridge protocol for sandbox ↔ BeamClaw communication.

The bridge runs over a Unix domain socket mounted into the container.
The Python bridge module sends JSON-RPC requests; this module decodes
them and dispatches to the tool registry or MCP registry.

Protocol:
  - Length-prefixed JSON-RPC 2.0 messages (4-byte big-endian length header)
  - Methods: search_tools, get_tool_schema, call_tool
""".

-export([encode_response/2, encode_error/3, decode_request/1,
         dispatch/3, encode_result/1, decode_length_prefixed/1,
         encode_length_prefixed/1]).

%% ---------------------------------------------------------------------------
%% JSON-RPC encoding
%% ---------------------------------------------------------------------------

-doc "Encode a successful JSON-RPC response.".
-spec encode_response(Id :: integer() | binary(), Result :: term()) -> binary().
encode_response(Id, Result) ->
    jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                 <<"id">>      => Id,
                 <<"result">>  => Result}).

-doc "Encode a JSON-RPC error response.".
-spec encode_error(Id :: integer() | binary() | null,
                   Code :: integer(), Message :: binary()) -> binary().
encode_error(Id, Code, Message) ->
    jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                 <<"id">>      => Id,
                 <<"error">>   => #{<<"code">>    => Code,
                                    <<"message">> => Message}}).

%% ---------------------------------------------------------------------------
%% JSON-RPC decoding
%% ---------------------------------------------------------------------------

-doc "Decode a JSON-RPC request from binary.".
-spec decode_request(Binary :: binary()) ->
    {ok, #{method => binary(), params => map(), id => term()}} |
    {error, term()}.
decode_request(Binary) ->
    try
        Map = jsx:decode(Binary, [return_maps]),
        Method = maps:get(<<"method">>, Map),
        Params = maps:get(<<"params">>, Map, #{}),
        Id = maps:get(<<"id">>, Map, null),
        {ok, #{method => Method, params => Params, id => Id}}
    catch
        _:Reason -> {error, {parse_error, Reason}}
    end.

%% ---------------------------------------------------------------------------
%% Length-prefixed framing
%% ---------------------------------------------------------------------------

-doc "Decode a length-prefixed message. Returns {ok, Payload, Rest} or need_more.".
-spec decode_length_prefixed(Buffer :: binary()) ->
    {ok, binary(), binary()} | need_more.
decode_length_prefixed(<<Len:32/big, Rest/binary>>) when byte_size(Rest) >= Len ->
    <<Payload:Len/binary, Remaining/binary>> = Rest,
    {ok, Payload, Remaining};
decode_length_prefixed(_) ->
    need_more.

-doc "Encode a message with 4-byte big-endian length prefix.".
-spec encode_length_prefixed(Payload :: binary()) -> binary().
encode_length_prefixed(Payload) ->
    Len = byte_size(Payload),
    <<Len:32/big, Payload/binary>>.

%% ---------------------------------------------------------------------------
%% Dispatch
%% ---------------------------------------------------------------------------

-doc """
Dispatch a decoded bridge request to the appropriate handler.

ToolBridgeFn is a callback fun(Name, Args) -> {ok, binary()} | {error, binary()}
injected by bc_loop to avoid dependency cycles.
""".
-spec dispatch(Method :: binary(), Params :: map(),
               ToolBridgeFn :: function()) ->
    {ok, term()} | {error, integer(), binary()}.
dispatch(<<"search_tools">>, Params, _ToolBridgeFn) ->
    Detail = maps:get(<<"detail">>, Params, <<"names">>),
    search_tools(Detail);
dispatch(<<"get_tool_schema">>, Params, _ToolBridgeFn) ->
    Name = maps:get(<<"name">>, Params, <<>>),
    get_tool_schema(Name);
dispatch(<<"call_tool">>, Params, ToolBridgeFn) ->
    Name = maps:get(<<"name">>, Params, <<>>),
    Args = maps:get(<<"args">>, Params, #{}),
    call_tool(Name, Args, ToolBridgeFn);
dispatch(Method, _Params, _ToolBridgeFn) ->
    {error, -32601, <<"Method not found: ", Method/binary>>}.

-doc "Encode a dispatch result into a JSON-RPC response binary.".
-spec encode_result({ok, term()} | {error, integer(), binary()}) -> binary().
encode_result({ok, Result}) ->
    encode_response(null, Result);
encode_result({error, Code, Message}) ->
    encode_error(null, Code, Message).

%% ---------------------------------------------------------------------------
%% Internal handlers
%% ---------------------------------------------------------------------------

search_tools(<<"names">>) ->
    Tools = bc_tool_registry:list(),
    Names = [Name || {Name, _Mod, _Def} <- Tools],
    {ok, Names};
search_tools(<<"descriptions">>) ->
    Tools = bc_tool_registry:list(),
    Descs = maps:from_list([{Name, maps:get(description, Def, <<>>)}
                            || {Name, _Mod, Def} <- Tools]),
    {ok, Descs};
search_tools(<<"schemas">>) ->
    Tools = bc_tool_registry:list(),
    Schemas = maps:from_list([{Name, Def} || {Name, _Mod, Def} <- Tools]),
    {ok, Schemas};
search_tools(_Other) ->
    {error, -32602, <<"Invalid detail level; use: names, descriptions, schemas">>}.

get_tool_schema(<<>>) ->
    {error, -32602, <<"Missing required parameter: name">>};
get_tool_schema(Name) ->
    case bc_tool_registry:lookup(Name) of
        {ok, {_Mod, Def}} -> {ok, Def};
        {error, not_found} -> {error, -32602, <<"Tool not found: ", Name/binary>>}
    end.

call_tool(<<>>, _Args, _Fn) ->
    {error, -32602, <<"Missing required parameter: name">>};
call_tool(Name, Args, ToolBridgeFn) ->
    case ToolBridgeFn(Name, Args) of
        {ok, Result}    -> {ok, Result};
        {error, Reason} -> {error, -32000, Reason}
    end.
