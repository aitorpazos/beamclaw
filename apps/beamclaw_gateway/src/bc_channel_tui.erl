%% @doc TUI (terminal UI) channel — reads from stdin, writes to stdout.
-module(bc_channel_tui).
-behaviour(gen_server).
%% Implements bc_channel callbacks.

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/1]).
-export([init/1, listen/1, send/3, send_typing/2,
         update_draft/4, finalize_draft/3, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
    Enabled = maps:get(enabled, Config, false),
    State   = #{enabled => Enabled, session_id => <<"tui">>},
    case Enabled of
        true  -> self() ! start_io;
        false -> ok
    end,
    {ok, State}.

listen(State) ->
    {ok, State}.

send(_, #bc_message{content = Content}, State) when is_binary(Content) ->
    io:format("~n[assistant] ~s~n> ", [Content]),
    {ok, State};
send(_, _, State) ->
    {ok, State}.

send_typing(_, _State) ->
    io:format("[...thinking...]~n"),
    ok.

update_draft(_, _, Content, State) ->
    io:format("\r~s", [Content]),
    {ok, State}.

finalize_draft(_, _, State) ->
    io:format("~n"),
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_info(start_io, State) ->
    io:format("BeamClaw TUI — type a message and press Enter.~n> "),
    self() ! read_line,
    {noreply, State};
handle_info(read_line, #{session_id := SessionId} = State) ->
    case io:get_line("") of
        eof -> {stop, normal, State};
        {error, _} -> {stop, io_error, State};
        Line ->
            Text = string:trim(unicode:characters_to_binary(Line)),
            case Text of
                <<>> -> ok;
                _ ->
                    ChannelMsg = #bc_channel_message{
                        session_id = SessionId,
                        user_id    = <<"tui_user">>,
                        channel    = tui,
                        content    = Text,
                        raw        = Line,
                        ts         = erlang:system_time(millisecond)
                    },
                    ensure_session_and_dispatch(SessionId, ChannelMsg)
            end,
            self() ! read_line,
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) -> {reply, {error, unknown}, State}.
handle_cast(_Msg, State)        -> {noreply, State}.
code_change(_OldVsn, State, _)  -> {ok, State}.

ensure_session_and_dispatch(SessionId, Msg) ->
    case bc_session_registry:lookup(SessionId) of
        {ok, Pid} ->
            bc_session:dispatch_run(Pid, Msg);
        {error, not_found} ->
            Config = #{session_id  => SessionId,
                       user_id     => <<"tui_user">>,
                       channel_id  => SessionId,
                       channel_mod => bc_channel_tui},
            bc_sessions_sup:start_session(Config),
            timer:sleep(100),
            case bc_session_registry:lookup(SessionId) of
                {ok, Pid} -> bc_session:dispatch_run(Pid, Msg);
                _          -> ok
            end
    end.
