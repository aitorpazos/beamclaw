%% @doc Developer convenience module for interacting with BeamClaw from the
%% Erlang REPL (rebar3 shell).  Not used in production; gateway channels handle
%% production traffic.
%%
%% Usage from the shell:
%%   bc_shell:chat("Hello, what can you do?").
%%   bc_shell:chat(<<"my-session">>, "Continue our conversation.").
-module(bc_shell).
-include_lib("beamclaw_core/include/bc_types.hrl").

-export([chat/1, chat/2]).

-define(SHELL_SESSION, <<"shell">>).
-define(TIMEOUT_MS, 120_000).

-spec chat(binary() | string()) -> ok | {error, timeout}.
chat(Text) ->
    chat(?SHELL_SESSION, Text).

-spec chat(binary(), binary() | string()) -> ok | {error, timeout}.
chat(SessionId, Text) when is_list(Text) ->
    chat(SessionId, unicode:characters_to_binary(Text));
chat(SessionId, Text) when is_binary(Text) ->
    Msg = #bc_channel_message{
        session_id = SessionId,
        user_id    = <<"shell_user">>,
        channel    = shell,
        content    = Text,
        raw        = Text,
        ts         = erlang:system_time(millisecond),
        reply_pid  = self()
    },
    SessionPid = ensure_session(SessionId),
    bc_session:dispatch_run(SessionPid, Msg),
    io:format("~n"),
    recv_loop(SessionId).

%% ----

recv_loop(SessionId) ->
    receive
        {bc_chunk, SessionId, Chunk} ->
            io:format("~s", [Chunk]),
            recv_loop(SessionId);
        {bc_done, SessionId, _Msg} ->
            io:format("~n"),
            ok
    after ?TIMEOUT_MS ->
        io:format("~n[bc_shell] timed out waiting for response~n"),
        {error, timeout}
    end.

ensure_session(SessionId) ->
    case bc_session_registry:lookup(SessionId) of
        {ok, Pid} ->
            Pid;
        {error, not_found} ->
            Config = #{session_id  => SessionId,
                       user_id     => <<"shell_user">>,
                       channel_id  => SessionId,
                       channel_mod => undefined},
            {ok, _} = bc_sessions_sup:start_session(Config),
            {ok, Pid} = bc_session_registry:lookup(SessionId),
            Pid
    end.
