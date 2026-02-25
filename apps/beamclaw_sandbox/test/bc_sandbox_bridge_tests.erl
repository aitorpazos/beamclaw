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

-module(bc_sandbox_bridge_tests).
-include_lib("eunit/include/eunit.hrl").

%% ---------------------------------------------------------------------------
%% JSON-RPC encode/decode tests
%% ---------------------------------------------------------------------------

encode_response_test() ->
    Bin = bc_sandbox_bridge:encode_response(1, <<"hello">>),
    Map = jsx:decode(Bin, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Map)),
    ?assertEqual(1, maps:get(<<"id">>, Map)),
    ?assertEqual(<<"hello">>, maps:get(<<"result">>, Map)).

encode_error_test() ->
    Bin = bc_sandbox_bridge:encode_error(42, -32601, <<"Method not found">>),
    Map = jsx:decode(Bin, [return_maps]),
    ?assertEqual(42, maps:get(<<"id">>, Map)),
    Error = maps:get(<<"error">>, Map),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Method not found">>, maps:get(<<"message">>, Error)).

decode_request_valid_test() ->
    Bin = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
                       <<"method">> => <<"search_tools">>,
                       <<"params">> => #{<<"detail">> => <<"names">>}}),
    {ok, Req} = bc_sandbox_bridge:decode_request(Bin),
    ?assertEqual(<<"search_tools">>, maps:get(method, Req)),
    ?assertEqual(#{<<"detail">> => <<"names">>}, maps:get(params, Req)),
    ?assertEqual(1, maps:get(id, Req)).

decode_request_no_params_test() ->
    Bin = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2,
                       <<"method">> => <<"search_tools">>}),
    {ok, Req} = bc_sandbox_bridge:decode_request(Bin),
    ?assertEqual(#{}, maps:get(params, Req)).

decode_request_invalid_json_test() ->
    {error, _} = bc_sandbox_bridge:decode_request(<<"not json">>).

%% ---------------------------------------------------------------------------
%% Length-prefixed framing tests
%% ---------------------------------------------------------------------------

length_prefixed_roundtrip_test() ->
    Payload = <<"hello world">>,
    Frame = bc_sandbox_bridge:encode_length_prefixed(Payload),
    {ok, Decoded, <<>>} = bc_sandbox_bridge:decode_length_prefixed(Frame),
    ?assertEqual(Payload, Decoded).

length_prefixed_with_remainder_test() ->
    Payload = <<"data">>,
    Extra = <<"extra">>,
    Frame = <<(bc_sandbox_bridge:encode_length_prefixed(Payload))/binary, Extra/binary>>,
    {ok, Decoded, Rest} = bc_sandbox_bridge:decode_length_prefixed(Frame),
    ?assertEqual(Payload, Decoded),
    ?assertEqual(Extra, Rest).

length_prefixed_need_more_test() ->
    %% Only 2 bytes of a 4-byte header
    ?assertEqual(need_more, bc_sandbox_bridge:decode_length_prefixed(<<0, 0>>)).

length_prefixed_partial_payload_test() ->
    %% Header says 100 bytes but only 3 available
    ?assertEqual(need_more, bc_sandbox_bridge:decode_length_prefixed(<<0, 0, 0, 100, "abc">>)).

%% ---------------------------------------------------------------------------
%% Dispatch tests
%% ---------------------------------------------------------------------------

dispatch_unknown_method_test() ->
    Fn = fun(_, _) -> {ok, <<"ok">>} end,
    {error, -32601, _} = bc_sandbox_bridge:dispatch(<<"unknown_method">>, #{}, Fn).

dispatch_call_tool_test() ->
    MockFn = fun(<<"read_file">>, #{<<"path">> := <<"/etc/hostname">>}) ->
        {ok, <<"myhost">>}
    end,
    {ok, <<"myhost">>} = bc_sandbox_bridge:dispatch(
        <<"call_tool">>,
        #{<<"name">> => <<"read_file">>, <<"args">> => #{<<"path">> => <<"/etc/hostname">>}},
        MockFn).

dispatch_call_tool_error_test() ->
    MockFn = fun(_, _) -> {error, <<"Tool failed">>} end,
    {error, -32000, <<"Tool failed">>} = bc_sandbox_bridge:dispatch(
        <<"call_tool">>,
        #{<<"name">> => <<"read_file">>, <<"args">> => #{}},
        MockFn).

dispatch_call_tool_missing_name_test() ->
    Fn = fun(_, _) -> {ok, <<"ok">>} end,
    {error, -32602, _} = bc_sandbox_bridge:dispatch(
        <<"call_tool">>, #{<<"args">> => #{}}, Fn).

dispatch_get_tool_schema_missing_name_test() ->
    Fn = fun(_, _) -> {ok, <<"ok">>} end,
    {error, -32602, _} = bc_sandbox_bridge:dispatch(
        <<"get_tool_schema">>, #{}, Fn).

dispatch_search_tools_invalid_detail_test() ->
    Fn = fun(_, _) -> {ok, <<"ok">>} end,
    {error, -32602, _} = bc_sandbox_bridge:dispatch(
        <<"search_tools">>, #{<<"detail">> => <<"invalid">>}, Fn).

%% ---------------------------------------------------------------------------
%% encode_result tests
%% ---------------------------------------------------------------------------

encode_result_ok_test() ->
    Bin = bc_sandbox_bridge:encode_result({ok, [<<"tool1">>, <<"tool2">>]}),
    Map = jsx:decode(Bin, [return_maps]),
    ?assertEqual([<<"tool1">>, <<"tool2">>], maps:get(<<"result">>, Map)).

encode_result_error_test() ->
    Bin = bc_sandbox_bridge:encode_result({error, -32601, <<"Not found">>}),
    Map = jsx:decode(Bin, [return_maps]),
    Error = maps:get(<<"error">>, Map),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)).
