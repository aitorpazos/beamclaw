-module(bc_provider_toolcall_mock).
-moduledoc """
Mock LLM provider that returns a tool call on first stream, then a normal
assistant response on second stream. Used to test bc_loop tool execution paths.
""".
-behaviour(bc_provider).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([init/1, complete/3, stream/4, capabilities/1, terminate/2]).

init(_Config) ->
    {ok, #{call_count => 0}}.

complete(_Messages, _Options, State) ->
    Msg = #bc_message{id = <<"tc-mock-complete">>, role = assistant,
                      content = <<"done">>, ts = 0},
    {ok, Msg, State}.

stream(_Messages, _Options, CallerPid, #{call_count := N} = State) ->
    case N of
        0 ->
            %% First call: return a message with a tool call
            %% Use OpenAI wire format (maps) — that's what real providers return
            ToolCall = #{<<"id">> => <<"tc-1">>,
                         <<"name">> => <<"crash_tool">>,
                         <<"args">> => #{}},
            Msg = #bc_message{
                id         = <<"tc-mock-1">>,
                role       = assistant,
                content    = <<"Calling crash_tool...">>,
                tool_calls = [ToolCall],
                ts         = 0
            },
            CallerPid ! {stream_chunk, self(), <<"Calling crash_tool...">>},
            CallerPid ! {stream_done, self(), Msg},
            {ok, State#{call_count := 1}};
        _ ->
            %% Subsequent calls: return a plain response (after tool result)
            Msg = #bc_message{
                id      = <<"tc-mock-2">>,
                role    = assistant,
                content = <<"The tool crashed but I survived.">>,
                ts      = 0
            },
            CallerPid ! {stream_chunk, self(), <<"The tool crashed">>},
            CallerPid ! {stream_done, self(), Msg},
            {ok, State#{call_count := N + 1}}
    end.

capabilities(_State) ->
    #{supports_streaming => true, supports_tools => true}.

terminate(_Reason, _State) ->
    ok.
