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

-module(bc_shell).
-moduledoc """
Developer convenience module for interacting with BeamClaw from the
Erlang REPL (rebar3 shell).  Not used in production; gateway channels handle
production traffic.

Usage from the shell:
  bc_shell:chat("Hello, what can you do?").
  bc_shell:chat(<<"my-session">>, "Continue our conversation.").
""".
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
