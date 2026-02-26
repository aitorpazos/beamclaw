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

-define(MAX_INPUT_TOKENS, 190000).
-define(MAX_RETRIES, 3).

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
    complete_with_retry(Messages, Options, State, ?MAX_RETRIES).

complete_with_retry(_Messages, _Options, State, 0) ->
    {error, {retries_exhausted, "Failed after truncation retries"}, State};
complete_with_retry(Messages, Options, State, RetriesLeft) ->
    Body = build_request_body(Messages, Options, State),
    case post(State, "/v1/messages", Body) of
        {ok, RespBody} ->
            Msg = parse_response(RespBody),
            {ok, Msg, State};
        {error, {400, ErrorBody}} ->
            case is_prompt_too_long(ErrorBody) of
                true ->
                    logger:warning("[anthropic] prompt too long, truncating history (retries left: ~B)", [RetriesLeft - 1]),
                    {_SystemMsgs, NonSystem} = lists:partition(
                        fun(#bc_message{role = R}) -> R =:= system end, Messages),
                    SystemMsgs = [M || #bc_message{role = R} = M <- Messages, R =:= system],
                    %% Aggressively drop first half of non-system messages
                    Len = length(NonSystem),
                    Drop = max(1, Len div 2),
                    Trimmed = lists:nthtail(min(Drop, Len - 1), NonSystem),
                    Cleaned = sanitize_history(Trimmed),
                    complete_with_retry(SystemMsgs ++ Cleaned, Options, State, RetriesLeft - 1);
                false ->
                    case is_orphaned_tool_use(ErrorBody) of
                        true ->
                            logger:warning("[anthropic] orphaned tool_use detected, sanitizing history"),
                            Sanitized = sanitize_history(Messages),
                            complete_with_retry(Sanitized, Options, State, RetriesLeft - 1);
                        false ->
                            {error, {400, ErrorBody}, State}
                    end
            end;
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

%% ---- Error detection ----

is_prompt_too_long(Body) when is_list(Body) ->
    is_prompt_too_long(list_to_binary(Body));
is_prompt_too_long(Body) when is_binary(Body) ->
    binary:match(Body, <<"prompt is too long">>) =/= nomatch;
is_prompt_too_long(_) -> false.

is_orphaned_tool_use(Body) when is_list(Body) ->
    is_orphaned_tool_use(list_to_binary(Body));
is_orphaned_tool_use(Body) when is_binary(Body) ->
    binary:match(Body, <<"tool_use">>) =/= nomatch andalso
    binary:match(Body, <<"tool_result">>) =/= nomatch;
is_orphaned_tool_use(_) -> false.

%% ---- History sanitization ----

%% Remove orphaned tool_use (assistant with tool_calls but no following tool_result)
%% and orphaned tool_result (tool messages without preceding tool_use).
sanitize_history(Messages) ->
    %% Pass 1: drop leading tool results
    M1 = drop_leading_tool_results(Messages),
    %% Pass 2: ensure every assistant tool_use has a matching tool_result
    M2 = fix_orphaned_tool_uses(M1),
    %% Pass 3: ensure no empty messages
    [M || #bc_message{role = R, content = C, tool_calls = TCs} = M <- M2,
          not (R =:= user andalso (C =:= undefined orelse C =:= <<>>)),
          not (R =:= assistant andalso (C =:= undefined orelse C =:= <<>>)
               andalso (TCs =:= [] orelse TCs =:= undefined))].

%% Drop any leading messages that would be invalid at the start of a conversation:
%% - tool results without preceding tool_use
%% - assistant messages with tool_calls (since their tool_results may have been dropped)
drop_leading_tool_results([#bc_message{role = tool} | Rest]) ->
    drop_leading_tool_results(Rest);
drop_leading_tool_results([#bc_message{role = assistant, content = C, tool_calls = TCs} | Rest])
  when TCs =/= [], TCs =/= undefined, (C =:= undefined orelse C =:= <<>>) ->
    %% Pure tool_use assistant message with no text — drop it and its results
    drop_leading_tool_results(Rest);
drop_leading_tool_results(Msgs) -> Msgs.

fix_orphaned_tool_uses([]) -> [];
fix_orphaned_tool_uses([#bc_message{role = assistant, tool_calls = TCs} = M | Rest])
  when TCs =/= [], TCs =/= undefined ->
    %% Collect the tool_call IDs from this assistant message
    ToolIds = [tc_id(TC) || TC <- TCs],
    %% Check if the next messages contain matching tool_results
    {ToolResults, Remaining} = collect_tool_results(Rest, ToolIds),
    case length(ToolResults) =:= length(ToolIds) of
        true ->
            %% All tool_results present — keep the pair
            [M | ToolResults] ++ fix_orphaned_tool_uses(Remaining);
        false ->
            %% Missing tool_results — inject synthetic ones for missing IDs
            ExistingIds = [TR#bc_message.tool_call_id || TR <- ToolResults],
            MissingIds = [Id || Id <- ToolIds, not lists:member(Id, ExistingIds)],
            SyntheticResults = [#bc_message{
                id = <<"synthetic_", Id/binary>>,
                role = tool,
                content = <<"[Tool result unavailable]">>,
                tool_calls = [],
                tool_call_id = Id,
                name = <<"unknown">>,
                ts = erlang:system_time(millisecond)
            } || Id <- MissingIds],
            [M | ToolResults ++ SyntheticResults] ++ fix_orphaned_tool_uses(Remaining)
    end;
fix_orphaned_tool_uses([M | Rest]) ->
    [M | fix_orphaned_tool_uses(Rest)].

collect_tool_results([], _Ids) -> {[], []};
collect_tool_results([#bc_message{role = tool, tool_call_id = TcId} = M | Rest], Ids) ->
    case lists:member(TcId, Ids) of
        true ->
            {More, Remaining} = collect_tool_results(Rest, Ids),
            {[M | More], Remaining};
        false ->
            {[], [M | Rest]}
    end;
collect_tool_results(Rest, _Ids) -> {[], Rest}.

tc_id(#bc_tool_call{id = Id}) -> Id;
tc_id(#{<<"id">> := Id}) -> Id;
tc_id(_) -> <<"unknown">>.

%% ---- Request building ----

build_request_body(Messages, Options, #{model := Model, max_tokens := MaxTok}) ->
    {SystemText, NonSystemMsgs} = extract_system(Messages),
    %% Pre-truncate based on token estimate
    TruncatedMsgs = truncate_to_token_limit(NonSystemMsgs, ?MAX_INPUT_TOKENS),
    %% Sanitize to fix any orphans from truncation
    SanitizedMsgs = sanitize_history(TruncatedMsgs),
    AnthropicMsgs = merge_consecutive(lists:map(fun message_to_anthropic/1, SanitizedMsgs)),
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

%% ---- Token estimation & truncation ----

%% Estimate tokens using ~3.5 chars/token (more conservative than 4).
%% Includes tool_call args in the estimate.
truncate_to_token_limit(Messages, MaxTokens) ->
    EstTokens = estimate_tokens(Messages),
    case EstTokens =< MaxTokens of
        true  -> Messages;
        false ->
            Len = length(Messages),
            case Len =< 2 of
                true -> Messages; %% Can't truncate further
                false ->
                    %% Drop first third of messages
                    Drop = max(1, Len div 3),
                    Trimmed = lists:nthtail(min(Drop, Len - 1), Messages),
                    Cleaned = sanitize_history(Trimmed),
                    logger:warning("[anthropic] truncated history: ~B→~B msgs (~B est tokens > ~B limit)",
                                   [Len, length(Cleaned), EstTokens, MaxTokens]),
                    truncate_to_token_limit(Cleaned, MaxTokens)
            end
    end.

estimate_tokens(Messages) ->
    lists:foldl(fun(#bc_message{content = C, tool_calls = TCs}, Acc) ->
        ContentSize = case C of
            undefined -> 0;
            B when is_binary(B) -> byte_size(B);
            _ -> 0
        end,
        ToolSize = lists:foldl(fun
            (#bc_tool_call{args = Args}, A) when is_map(Args) ->
                A + byte_size(jsx:encode(Args));
            ({bc_tool_call, _, _, Args, _}, A) when is_map(Args) ->
                A + byte_size(jsx:encode(Args));
            (_, A) -> A
        end, 0, if is_list(TCs) -> TCs; true -> [] end),
        %% ~3.5 chars per token, plus overhead per message (~10 tokens)
        Acc + ((ContentSize + ToolSize) * 10 div 35) + 10
    end, 0, Messages).

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
