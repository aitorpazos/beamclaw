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

%% @doc WebSocket handler — GET /ws
%%
%% Each WebSocket connection gets a unique session. Messages are dispatched to
%% bc_session with reply_pid = self(), so bc_loop sends {bc_chunk, SId, Chunk}
%% and {bc_done, SId, Msg} directly to this handler process.
-module(bc_ws_h).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([init/2, websocket_init/1, websocket_handle/2,
         websocket_info/2, terminate/3]).

init(Req, _State) ->
    {cowboy_websocket, Req, #{session_id => undefined}, #{idle_timeout => 3600000}}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    case jsx:decode(Msg, [return_maps]) of
        Decoded when is_map(Decoded) ->
            handle_ws_message(Decoded, State);
        _ ->
            {ok, State}
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({bc_chunk, _SId, Chunk}, State) ->
    Frame = jsx:encode(#{type => <<"chunk">>, content => Chunk}),
    {reply, {text, Frame}, State};
websocket_info({bc_done, _SId, #bc_message{content = Content}}, State) ->
    Frame = jsx:encode(#{type => <<"done">>, content => Content}),
    {reply, {text, Frame}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% Internal

handle_ws_message(#{<<"type">> := <<"message">>, <<"content">> := Content} = Msg, State) ->
    AgentId   = maps:get(<<"agent_id">>, Msg, <<"default">>),
    UserId = case bc_config:canonical_user_id() of
        undefined ->
            RawUserId = maps:get(<<"user_id">>, Msg, <<"anonymous">>),
            <<"ws:", RawUserId/binary>>;
        Canonical -> Canonical
    end,
    SessionId = bc_session_registry:derive_session_id(UserId, AgentId, websocket),
    ChannelMsg = #bc_channel_message{
        session_id = SessionId,
        user_id    = UserId,
        agent_id   = AgentId,
        channel    = websocket,
        content    = Content,
        raw        = Content,
        ts         = erlang:system_time(millisecond),
        reply_pid  = self()
    },
    SessionPid = get_or_create_session(SessionId, UserId, AgentId),
    bc_session:dispatch_run(SessionPid, ChannelMsg),
    {ok, State#{session_id => SessionId}};
handle_ws_message(_Msg, State) ->
    {ok, State}.

get_or_create_session(SessionId, UserId, AgentId) ->
    case bc_session_registry:lookup(SessionId) of
        {ok, Pid} ->
            Pid;
        {error, not_found} ->
            Config = #{session_id  => SessionId,
                       user_id     => UserId,
                       channel_id  => SessionId,
                       channel_mod => undefined,
                       agent_id    => AgentId},
            {ok, _} = bc_sessions_sup:start_session(Config),
            {ok, Pid} = bc_session_registry:lookup(SessionId),
            Pid
    end.

