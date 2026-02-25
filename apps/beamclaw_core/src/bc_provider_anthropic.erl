%%
%% Anthropic Messages API provider for BeamClaw.
%%
%% Maps between BeamClaw's internal bc_message/bc_tool_call records and
%% Anthropic's Messages API format (https://docs.anthropic.com/en/api/messages).
%%

-module(bc_provider_anthropic).
-moduledoc "Anthropic (Claude) LLM provider.".
-behaviour(bc_provider).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/1]).
-export([init/1, complete/3, stream/4, capabilities/1, terminate/2]).

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

init(Config) ->
    ApiKey  = bc_config:resolve(maps:get(api_key,  Config, {env, "ANTHROPIC_API_KEY"})),
    BaseUrl = maps:get(base_url, Config, "https://api.anthropic.com"),
    Model   = maps:get(model,    Config, "claude-sonnet-4-20250514"),
    MaxTok  = maps:get(max_tokens, Config, 8192),
    {ok, #{api_key => ApiKey, base_url => BaseUrl, model => Model,
           max_tokens => MaxTok}}.

complete(Messages, Options, State) ->
    Body = build_request_body(Messages, Options, State),
    case post(State, "/v1/messages", Body) of
        {ok, RespBody} ->
            Msg = parse_response(RespBody),
            {ok, Msg, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

stream(Messages, Options, CallerPid, State) ->
    %% Fall back to non-streaming for now.
    case complete(Messages, Options, State) of
        {ok, Msg, NewState} ->
            CallerPid ! {stream_chunk, self(), Msg#bc_message.content},
            CallerPid ! {stream_done,  self(), Msg},
            {ok, NewState};
        {error, Reason, NewState} ->
            CallerPid ! {stream_error, self(), Reason},
            {error, Reason, NewState}
    end.

capabilities(_State) ->
    #{supports_streaming => true, supports_tools => true}.

terminate(_Reason, _State) ->
    ok.

%% ---- Request building ----

build_request_body(Messages, Options, #{model := Model, max_tokens := MaxTok}) ->
    {SystemText, NonSystemMsgs} = extract_system(Messages),
    AnthropicMsgs = merge_consecutive(lists:map(fun message_to_anthropic/1, NonSystemMsgs)),
    Base = #{
        model      => list_to_binary(Model),
        max_tokens => MaxTok,
        messages   => AnthropicMsgs
    },
    WithSystem = case SystemText of
        <<>> -> Base;
        _    -> Base#{system => SystemText}
    end,
    WithTools = case maps:get(tools, Options, []) of
        [] -> WithSystem;
        ToolDefs ->
            WithSystem#{tools => [tool_def_to_anthropic(T) || T <- ToolDefs]}
    end,
    jsx:encode(WithTools).

%% Extract system messages into a single system prompt.
extract_system(Messages) ->
    {Sys, Rest} = lists:partition(
        fun(#bc_message{role = R}) -> R =:= system end, Messages),
    SysText = iolist_to_binary(lists:join(<<"\n\n">>,
        [C || #bc_message{content = C} <- Sys, C =/= undefined, C =/= <<>>])),
    {SysText, Rest}.

%% Convert a bc_message to Anthropic message format.
message_to_anthropic(#bc_message{role = assistant, content = Content,
                                  tool_calls = ToolCalls})
  when ToolCalls =/= [], ToolCalls =/= undefined ->
    %% Assistant message with tool_use blocks.
    TextBlocks = case Content of
        undefined -> [];
        <<>>      -> [];
        _         -> [#{type => <<"text">>, text => Content}]
    end,
    ToolBlocks = lists:filtermap(fun tool_call_to_block/1, ToolCalls),
    #{role => <<"assistant">>, content => TextBlocks ++ ToolBlocks};

message_to_anthropic(#bc_message{role = tool, content = Content,
                                  tool_call_id = ToolCallId}) ->
    IsError = false,  %% TODO: propagate from tool_calls metadata
    #{role => <<"user">>,
      content => [#{type         => <<"tool_result">>,
                    tool_use_id  => ensure_binary(ToolCallId),
                    content      => ensure_binary(Content),
                    is_error     => IsError}]};

message_to_anthropic(#bc_message{role = Role, content = Content}) ->
    AnthropicRole = case Role of
        user      -> <<"user">>;
        assistant -> <<"assistant">>;
        _         -> <<"user">>
    end,
    #{role => AnthropicRole, content => ensure_binary(Content)}.

%% Convert native OpenAI-format tool_calls to Anthropic tool_use blocks.
%% These come from the tool_calls field on bc_message (parsed by bc_tool_parser).
tool_call_to_block(#{<<"id">> := Id, <<"function">> := #{<<"name">> := Name,
                                                          <<"arguments">> := ArgsJson}}) ->
    Args = case ArgsJson of
        B when is_binary(B) ->
            try jsx:decode(B, [return_maps]) catch _:_ -> #{} end;
        M when is_map(M) -> M;
        _ -> #{}
    end,
    {true, #{type => <<"tool_use">>, id => Id, name => Name, input => Args}};
tool_call_to_block(#bc_tool_call{id = Id, name = Name, args = Args}) ->
    {true, #{type => <<"tool_use">>, id => Id, name => Name, input => Args}};
tool_call_to_block(_) ->
    false.

%% Anthropic requires alternating user/assistant. Merge consecutive same-role messages.
merge_consecutive([]) -> [];
merge_consecutive([M]) -> [M];
merge_consecutive([#{role := R, content := C1}, #{role := R, content := C2} | Rest]) ->
    Merged = merge_content(C1, C2),
    merge_consecutive([#{role => R, content => Merged} | Rest]);
merge_consecutive([M | Rest]) ->
    [M | merge_consecutive(Rest)].

merge_content(C1, C2) when is_binary(C1), is_binary(C2) ->
    <<C1/binary, "\n\n", C2/binary>>;
merge_content(C1, C2) when is_list(C1), is_list(C2) ->
    C1 ++ C2;
merge_content(C1, C2) when is_binary(C1), is_list(C2) ->
    [#{type => <<"text">>, text => C1} | C2];
merge_content(C1, C2) when is_list(C1), is_binary(C2) ->
    C1 ++ [#{type => <<"text">>, text => C2}];
merge_content(_, C2) -> C2.

%% Tool definition → Anthropic format.
tool_def_to_anthropic(#{name := N, description := D, parameters := P}) ->
    #{name         => N,
      description  => D,
      input_schema => encode_params(P)}.

encode_params(M) when is_map(M) ->
    maps:fold(fun(K, V, Acc) ->
        BK = if is_atom(K) -> atom_to_binary(K, utf8); true -> K end,
        Acc#{BK => encode_params(V)}
    end, #{}, M);
encode_params(L) when is_list(L) ->
    [encode_params(E) || E <- L];
encode_params(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
encode_params(V) ->
    V.

%% ---- HTTP ----

post(#{api_key := Key, base_url := Base}, Path, Body) ->
    Url     = Base ++ Path,
    Headers = [{"x-api-key",         Key},
               {"anthropic-version", "2023-06-01"},
               {"content-type",      "application/json"}],
    case httpc:request(post, {Url, Headers, "application/json", Body},
                       [{timeout, 120000}, {connect_timeout, 10000}], []) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            {ok, iolist_to_binary(RespBody)};
        {ok, {{_, Status, _}, _, RespBody}} ->
            logger:warning("[anthropic] HTTP ~p: ~s", [Status, RespBody]),
            {error, {Status, RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% ---- Response parsing ----

parse_response(Body) ->
    Decoded = jsx:decode(Body, [return_maps]),
    ContentBlocks = maps:get(<<"content">>, Decoded, []),
    {TextParts, ToolUseParts} = lists:partition(
        fun(B) -> maps:get(<<"type">>, B, <<"text">>) =:= <<"text">> end,
        ContentBlocks),
    Text = iolist_to_binary(lists:join(<<>>,
        [maps:get(<<"text">>, B, <<>>) || B <- TextParts])),
    ToolCalls = [tool_use_to_call(B) || B <- ToolUseParts],
    #bc_message{
        id         = maps:get(<<"id">>, Decoded, <<>>),
        role       = assistant,
        content    = Text,
        tool_calls = ToolCalls,
        ts         = erlang:system_time(millisecond)
    }.

%% Convert Anthropic tool_use block to the format bc_tool_parser expects.
tool_use_to_call(#{<<"id">> := Id, <<"name">> := Name, <<"input">> := Input}) ->
    #bc_tool_call{id = Id, name = Name, args = Input, source = native}.

%% ---- Helpers ----

ensure_binary(undefined) -> <<>>;
ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) -> list_to_binary(L);
ensure_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
ensure_binary(Other) -> iolist_to_binary(io_lib:format("~p", [Other])).
