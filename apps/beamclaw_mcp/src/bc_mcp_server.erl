%% @doc MCP server connection manager.
%%
%% Owns an erlang:open_port for stdio-based MCP servers.
%% Implements JSON-RPC 2.0 protocol for MCP.
%% On start: initialize → initialized notification → tools/list → register in bc_mcp_registry.
%% On port crash: supervisor restarts; rediscovers tools on each restart.
-module(bc_mcp_server).
-behaviour(gen_server).

-export([start_link/1, call_tool/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    config       :: map(),
    name         :: binary(),
    port         :: port() | undefined,
    pending      :: #{integer() => from()},  %% rpc_id → gen_server From
    next_id      :: integer(),
    buf          :: binary()
}).

-type from() :: {pid(), term()}.

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc Call a tool on this MCP server.
-spec call_tool(ServerPid :: pid(), ToolName :: binary(), Args :: map()) ->
    {ok, binary()} | {error, term()}.
call_tool(ServerPid, ToolName, Args) ->
    gen_server:call(ServerPid, {call_tool, ToolName, Args}, 30000).

init(Config) ->
    Name = maps:get(name, Config, <<"unnamed">>),
    State = #state{config  = Config,
                   name    = Name,
                   port    = undefined,
                   pending = #{},
                   next_id = 1,
                   buf     = <<>>},
    self() ! connect,
    {ok, State}.

handle_info(connect, State) ->
    case open_port(State#state.config) of
        {ok, Port} ->
            NewState = State#state{port = Port},
            {noreply, handshake(NewState)};
        {error, Reason} ->
            logger:error("[mcp] failed to connect: ~p", [Reason]),
            {stop, {connect_failed, Reason}, State}
    end;
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    NewBuf = <<(State#state.buf)/binary, Data/binary>>,
    {NewState, Responses} = parse_responses(State#state{buf = NewBuf}),
    lists:foreach(fun(Resp) -> dispatch_response(Resp, NewState) end, Responses),
    {noreply, NewState};
handle_info({Port, closed}, #state{port = Port} = State) ->
    logger:warning("[mcp] port ~p closed", [State#state.name]),
    {stop, port_closed, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call({call_tool, ToolName, Args}, From, State) ->
    Params = #{name => ToolName, arguments => Args},
    {NewState, _Id} = send_rpc(<<"tools/call">>, Params, From, State),
    {noreply, NewState};
handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port}) when Port =/= undefined ->
    catch port_close(Port),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

open_port(Config) ->
    Cmd = maps:get(command, Config, ""),
    Args = maps:get(args, Config, []),
    FullCmd = string:join([Cmd | Args], " "),
    try
        Port = erlang:open_port({spawn, FullCmd},
                                [binary, {packet, line}, exit_status]),
        {ok, Port}
    catch
        _:Reason -> {error, Reason}
    end.

handshake(State) ->
    Params = #{protocolVersion => <<"2024-11-05">>,
               capabilities    => #{},
               clientInfo      => #{name => <<"beamclaw">>, version => <<"0.1.0">>}},
    {NewState, _} = send_rpc(<<"initialize">>, Params, undefined, State),
    NewState.

send_rpc(Method, Params, From, #state{next_id = Id, pending = P, port = Port} = State) ->
    Msg = jsx:encode(#{jsonrpc => <<"2.0">>, id => Id,
                       method  => Method, params => Params}),
    port_command(Port, [Msg, "\n"]),
    NewPending = case From of
        undefined -> P;
        _         -> P#{Id => From}
    end,
    {State#state{next_id = Id + 1, pending = NewPending}, Id}.

parse_responses(State) ->
    parse_responses(State, []).

parse_responses(#state{buf = Buf} = State, Acc) ->
    case binary:split(Buf, <<"\n">>) of
        [Line, Rest] ->
            try
                Resp = jsx:decode(Line, [return_maps]),
                parse_responses(State#state{buf = Rest}, [Resp | Acc])
            catch _:_ ->
                parse_responses(State#state{buf = Rest}, Acc)
            end;
        _ ->
            {State, lists:reverse(Acc)}
    end.

dispatch_response(#{<<"id">> := Id, <<"result">> := Result},
                  #state{pending = P} = _State) ->
    case maps:get(Id, P, undefined) of
        undefined ->
            handle_result(Id, Result, _State);
        From ->
            gen_server:reply(From, {ok, Result})
    end;
dispatch_response(#{<<"id">> := Id, <<"error">> := Err},
                  #state{pending = P} = _State) ->
    case maps:get(Id, P, undefined) of
        undefined -> ok;
        From      -> gen_server:reply(From, {error, Err})
    end;
dispatch_response(#{<<"method">> := _Notif}, _State) ->
    %% Notifications (no id) — ignore for now
    ok;
dispatch_response(_Other, _State) ->
    ok.

handle_result(_Id, #{<<"tools">> := Tools}, State) ->
    %% Response to tools/list: register in bc_mcp_registry
    bc_mcp_registry:register_tools(self(), State#state.name, Tools);
handle_result(_Id, _Result, _State) ->
    %% After initialize, send initialized notification and tools/list
    Port = _State#state.port,
    Notif = jsx:encode(#{jsonrpc => <<"2.0">>,
                         method  => <<"notifications/initialized">>,
                         params  => #{}}),
    port_command(Port, [Notif, "\n"]),
    send_rpc(<<"tools/list">>, #{}, undefined, _State).
