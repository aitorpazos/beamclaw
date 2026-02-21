%% @doc End-to-end smoke test: TUI channel — M7 final task.
%%
%% Exercises the full agentic loop without HTTP:
%%   session creation → message dispatch → bc_loop streaming → history update.
%%
%% bc_provider_smoke_mock:stream/4 sends canned messages directly to the
%% caller PID before returning, so receive_stream/2 finds them in the mailbox
%% immediately — no timing dependency on a real HTTP stream.
%%
%% beamclaw_gateway is NOT started, avoiding the TUI stdin reader.
%%
%% Startup race: dispatch_run may arrive before bc_loop announces its PID via
%% set_loop_pid. bc_session queues it and drains on set_loop_pid — safe.
-module(bc_smoke_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% ---- Fixtures ----

setup() ->
    {ok, Started} = application:ensure_all_started(beamclaw_core),
    Started.

teardown(Started) ->
    lists:foreach(fun application:stop/1, lists:reverse(Started)).

%% ---- Test suite ----

smoke_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun smoke_roundtrip/1
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
