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

-module(bc_smoke_tests).
-moduledoc """
End-to-end smoke test: TUI channel — M7 final task.

Exercises the full agentic loop without HTTP:
  session creation → message dispatch → bc_loop streaming → history update.

bc_provider_smoke_mock:stream/4 sends canned messages directly to the
caller PID before returning, so receive_stream/2 finds them in the mailbox
immediately — no timing dependency on a real HTTP stream.

beamclaw_gateway is NOT started, avoiding the TUI stdin reader.

Startup race: dispatch_run may arrive before bc_loop announces its PID via
set_loop_pid. bc_session queues it and drains on set_loop_pid — safe.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% ---- Fixtures ----

setup() ->
    %% Disable session persistence so Mnesia table absence doesn't crash tests
    application:set_env(beamclaw_core, session_persistence, false),
    {ok, Started} = application:ensure_all_started(beamclaw_core),
    Started.

teardown(Started) ->
    lists:foreach(fun application:stop/1, lists:reverse(Started)).

%% ---- Test suite ----

smoke_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun smoke_roundtrip/1,
        fun tool_crash_resilience/1
    ]}.

%% Dispatch a user message; assert the assistant reply appears in history.
smoke_roundtrip(_Started) ->
    ?_test(do_roundtrip()).

do_roundtrip() ->
    SessionId = <<"smoke-session-1">>,
    Config = #{
        session_id   => SessionId,
        provider_mod => bc_provider_smoke_mock,
        autonomy     => full
    },
    {ok, _SupPid} = bc_sessions_sup:start_session(Config),
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),
    Msg = #bc_channel_message{
        session_id = SessionId,
        user_id    = <<"test">>,
        channel    = tui,
        content    = <<"hello">>,
        raw        = <<"hello">>,
        ts         = 0
    },
    bc_session:dispatch_run(SessionPid, Msg),
    timer:sleep(500),
    History = bc_session:get_history(SessionPid),
    ?assert(length(History) >= 1),
    AssistantMsgs = [M || M <- History, M#bc_message.role =:= assistant],
    ?assert(length(AssistantMsgs) >= 1).

%% Tool crash: register a crashing tool, dispatch a message that triggers it,
%% verify the loop survives and produces both a tool error result and a
%% follow-up assistant response.
tool_crash_resilience(_Started) ->
    ?_test(do_tool_crash()).

do_tool_crash() ->
    %% Register the crashing mock tool
    bc_tool_registry:register(bc_tool_crash_mock, bc_tool_crash_mock:definition()),

    SessionId = <<"crash-test-session">>,
    Config = #{
        session_id   => SessionId,
        provider_mod => bc_provider_toolcall_mock,
        autonomy     => full
    },
    {ok, _SupPid} = bc_sessions_sup:start_session(Config),
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),

    Msg = #bc_channel_message{
        session_id = SessionId,
        user_id    = <<"test">>,
        channel    = tui,
        content    = <<"use the crash tool">>,
        raw        = <<"use the crash tool">>,
        ts         = 0
    },
    bc_session:dispatch_run(SessionPid, Msg),
    %% Wait for: stream(tool_call) → execute(crash) → stream(response)
    timer:sleep(1000),

    History = bc_session:get_history(SessionPid),
    %% Should have: user msg, assistant(tool_call), tool(error), assistant(response)
    ?assert(length(History) >= 3),

    %% The tool result should contain the crash error
    ToolMsgs = [M || M <- History, M#bc_message.role =:= tool],
    ?assert(length(ToolMsgs) >= 1),
    [ToolMsg | _] = ToolMsgs,
    ?assertNotEqual(nomatch,
        binary:match(ToolMsg#bc_message.content, <<"Tool crashed">>)),

    %% The loop survived — there should be a follow-up assistant response
    AssistantMsgs = [M || M <- History, M#bc_message.role =:= assistant],
    ?assert(length(AssistantMsgs) >= 2).
