%% @doc MCP server connection manager.
%%
%% Owns an erlang:open_port for stdio-based MCP servers.
%% Implements JSON-RPC 2.0 protocol for MCP.
%%
%% Startup sequence:
%%   1. open_port → connect to external MCP process via stdio
%%   2. Send initialize RPC (id=1)
%%   3. On initialize response: send notifications/initialized notification
%%      then send tools/list RPC
%%   4. On tools/list response: register tools in bc_mcp_registry
%%
%% On port crash: supervisor restarts the gen_server; handshake repeats,
%% tools are rediscovered and re-registered.
-module(bc_mcp_server).
-behaviour(gen_server).

-export([start_link/1, call_tool/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% open_port/1 builds a command from a runtime Config map; Dialyzer cannot
%% infer that maps:get/3 on an untyped map yields a valid string, so it
%% conservatively concludes that erlang:open_port always raises and that
%% handshake/1 is dead code.  These are false positives.
-dialyzer({nowarn_function, [open_port/1, handle_info/2, handshake/1]}).

-record(state, {
    config   :: map(),
    name     :: binary(),
    port     :: port() | undefined,
    pending  :: #{integer() => {pid(), term()}},  %% rpc_id → gen_server From
    next_id  :: integer(),
    buf      :: binary()
}).

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc Call a tool on this MCP server. Times out after 30 s.
-spec call_tool(ServerPid :: pid(), ToolName :: binary(), Args :: map()) ->
    {ok, term()} | {error, term()}.
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
            NewState = handshake(State#state{port = Port}),
            {noreply, NewState};
        {error, Reason} ->
            logger:error("[mcp] ~s: failed to connect: ~p",
                         [State#state.name, Reason]),
            {stop, {connect_failed, Reason}, State}
    end;
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    NewBuf = <<(State#state.buf)/binary, Data/binary>>,
    {ParsedState, Responses} = parse_responses(State#state{buf = NewBuf}),
    %% Fold state through each response so next_id and pending stay consistent.
    FinalState = lists:foldl(fun dispatch_response/2, ParsedState, Responses),
    {noreply, FinalState};
handle_info({Port, closed}, #state{port = Port} = State) ->
    logger:warning("[mcp] ~s: port closed", [State#state.name]),
    {stop, port_closed, State};
handle_info({Port, {exit_status, Code}}, #state{port = Port} = State) ->
    logger:warning("[mcp] ~s: port exited with status ~p",
                   [State#state.name, Code]),
    {stop, {port_exit, Code}, State};
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

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

open_port(Config) ->
    Cmd  = maps:get(command, Config, ""),
    Args = maps:get(args, Config, []),
    case Cmd of
        "" ->
            {error, no_command_configured};
        _ ->
            FullCmd = string:join([Cmd | Args], " "),
            try
                Port = erlang:open_port({spawn, FullCmd},
                                        [binary, {packet, line}, exit_status]),
                {ok, Port}
            catch
                _:Reason -> {error, Reason}
            end
    end.

handshake(State) ->
    Params = #{protocolVersion => <<"2024-11-05">>,
               capabilities    => #{},
               clientInfo      => #{name => <<"beamclaw">>, version => <<"0.1.0">>}},
    {NewState, _Id} = send_rpc(<<"initialize">>, Params, undefined, State),
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

%% dispatch_response/2 is used as a fold function: takes a response and the
%% current State, returns the updated State. This ensures next_id and pending
%% are properly threaded through all responses in a single port data message.
-spec dispatch_response(map(), #state{}) -> #state{}.
dispatch_response(#{<<"id">> := Id, <<"result">> := Result},
                  #state{pending = P} = State) ->
    case maps:get(Id, P, undefined) of
        undefined ->
            %% Unsolicited result (our handshake RPCs use undefined From).
            handle_result(Id, Result, State);
        From ->
            gen_server:reply(From, {ok, Result}),
            State#state{pending = maps:remove(Id, P)}
    end;
dispatch_response(#{<<"id">> := Id, <<"error">> := Err},
                  #state{pending = P} = State) ->
    case maps:get(Id, P, undefined) of
        undefined -> State;
        From ->
            gen_server:reply(From, {error, Err}),
            State#state{pending = maps:remove(Id, P)}
    end;
dispatch_response(#{<<"method">> := _Notif}, State) ->
    %% Notification from server (no id field) — ignore.
    State;
dispatch_response(_Other, State) ->
    State.

%% handle_result/3 processes responses to our internal handshake RPCs.
%% Returns the updated State (next_id incremented when a new RPC is sent).
-spec handle_result(integer(), map(), #state{}) -> #state{}.
handle_result(_Id, #{<<"tools">> := Tools}, State) ->
    %% Response to tools/list: register tools with the registry.
    bc_mcp_registry:register_tools(self(), State#state.name, Tools),
    State;
handle_result(_Id, _Result, State) ->
    %% Response to initialize: send initialized notification, then tools/list.
    Port = State#state.port,
    Notif = jsx:encode(#{jsonrpc => <<"2.0">>,
                         method  => <<"notifications/initialized">>,
                         params  => #{}}),
    port_command(Port, [Notif, "\n"]),
    {NewState, _ToolsId} = send_rpc(<<"tools/list">>, #{}, undefined, State),
    NewState.
