%% @doc Agentic loop — gen_statem.
%%
%% States:
%%   idle → compacting (optional) → streaming → awaiting_approval (optional)
%%        → executing_tools → streaming (loop) → finalizing → idle
%%
%% bc_loop is transient. A crash is restarted by bc_session_sup.
%% bc_session (permanent) retains history across restarts.
-module(bc_loop).
-behaviour(gen_statem).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/1]).
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([idle/3, compacting/3, streaming/3,
         awaiting_approval/3, executing_tools/3, finalizing/3]).

-record(loop_data, {
    session_pid  :: pid(),
    session_id   :: binary(),
    provider_mod :: module(),
    config       :: map(),
    current_run  :: term() | undefined,
    tool_calls   :: [#bc_tool_call{}],
    iteration    :: non_neg_integer()
}).

start_link(Config) ->
    gen_statem:start_link(?MODULE, Config, []).

callback_mode() -> state_functions.

init(Config) ->
    SessionPid  = maps:get(session_pid,  Config),
    SessionId   = maps:get(session_id,   Config),
    ProviderMod = maps:get(provider_mod, Config, bc_provider_openrouter),
    bc_session:set_loop_pid(SessionPid, self()),
    bc_obs:emit(agent_start, #{session_id => SessionId}),
    Data = #loop_data{
        session_pid  = SessionPid,
        session_id   = SessionId,
        provider_mod = ProviderMod,
        config       = Config,
        current_run  = undefined,
        tool_calls   = [],
        iteration    = 0
    },
    {ok, idle, Data}.

%% ---- States ----

idle(cast, {run, Message}, Data) ->
    History  = bc_session:get_history(Data#loop_data.session_pid),
    Threshold = maps:get(compaction_threshold,
                    bc_config:get(beamclaw_core, agentic_loop, #{}), 50),
    NextState = case length(History) > Threshold of
        true  -> compacting;
        false -> streaming
    end,
    bc_obs:emit(agent_start, #{session_id => Data#loop_data.session_id}),
    {next_state, NextState, Data#loop_data{current_run = Message, iteration = 0}};
idle(EventType, EventContent, Data) ->
    handle_common(idle, EventType, EventContent, Data).

compacting(enter, _OldState, Data) ->
    %% Trigger compaction asynchronously via cast to self
    gen_statem:cast(self(), do_compact),
    {keep_state, Data};
compacting(cast, do_compact, Data) ->
    bc_compactor:compact(Data#loop_data.session_pid),
    {next_state, streaming, Data};
compacting(EventType, EventContent, Data) ->
    handle_common(compacting, EventType, EventContent, Data).

streaming(enter, _OldState, Data) ->
    gen_statem:cast(self(), do_stream),
    {keep_state, Data};
streaming(cast, do_stream, Data) ->
    History     = bc_session:get_history(Data#loop_data.session_pid),
    SessionRef  = make_session_ref(Data),
    LoopCfg     = bc_config:get(beamclaw_core, agentic_loop, #{}),
    ChunkSize   = maps:get(stream_chunk_size, LoopCfg, 80),
    T0 = erlang:monotonic_time(millisecond),
    bc_obs:emit(llm_request, #{session_id => Data#loop_data.session_id,
                                message_count => length(History)}),
    case (Data#loop_data.provider_mod):stream(History, #{chunk_size => ChunkSize},
                                              self(), SessionRef) of
        {ok, _ProvState} ->
            receive_stream(Data, T0);
        {error, Reason, _ProvState} ->
            bc_obs:emit(llm_response, #{session_id => Data#loop_data.session_id,
                                        success => false, error => Reason}),
            {next_state, finalizing, Data}
    end;
streaming(EventType, EventContent, Data) ->
    handle_common(streaming, EventType, EventContent, Data).

awaiting_approval(enter, _OldState, Data) ->
    %% bc_approval sends {approval_result, ToolCallId, Decision} back
    {keep_state, Data};
awaiting_approval(info, {approval_result, _ToolCallId, approved}, Data) ->
    {next_state, executing_tools, Data};
awaiting_approval(info, {approval_result, _ToolCallId, denied}, Data) ->
    {next_state, finalizing, Data};
awaiting_approval(EventType, EventContent, Data) ->
    handle_common(awaiting_approval, EventType, EventContent, Data).

executing_tools(enter, _OldState, Data) ->
    gen_statem:cast(self(), do_execute),
    {keep_state, Data};
executing_tools(cast, do_execute, Data) ->
    MaxIter = maps:get(max_tool_iterations,
                  bc_config:get(beamclaw_core, agentic_loop, #{}), 10),
    case Data#loop_data.iteration >= MaxIter of
        true ->
            logger:warning("[loop] max tool iterations (~p) reached", [MaxIter]),
            {next_state, finalizing, Data};
        false ->
            execute_tool_calls(Data)
    end;
executing_tools(EventType, EventContent, Data) ->
    handle_common(executing_tools, EventType, EventContent, Data).

finalizing(enter, _OldState, Data) ->
    gen_statem:cast(self(), do_finalize),
    {keep_state, Data};
finalizing(cast, do_finalize, Data) ->
    bc_session:turn_complete(Data#loop_data.session_pid, ok),
    bc_obs:emit(turn_complete, #{session_id => Data#loop_data.session_id}),
    {next_state, idle, Data#loop_data{current_run = undefined, tool_calls = []}};
finalizing(EventType, EventContent, Data) ->
    handle_common(finalizing, EventType, EventContent, Data).

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%% ---- Internal ----

handle_common(_State, cast, {run, Message}, Data) ->
    %% Enqueue to session while we're busy; bc_session handles the queue
    bc_session:dispatch_run(Data#loop_data.session_pid, Message),
    keep_state_and_data;
handle_common(_State, _Type, _Content, _Data) ->
    keep_state_and_data.

make_session_ref(Data) ->
    #bc_session_ref{
        session_id  = Data#loop_data.session_id,
        user_id     = <<"">>,
        session_pid = Data#loop_data.session_pid,
        autonomy    = supervised
    }.

receive_stream(Data, T0) ->
    receive
        {stream_chunk, _Pid, _Chunk} ->
            %% TODO: forward chunk to channel via bc_channel:update_draft
            receive_stream(Data, T0);
        {stream_done, _Pid, FullMsg} ->
            Duration = erlang:monotonic_time(millisecond) - T0,
            bc_obs:emit(llm_response, #{session_id => Data#loop_data.session_id,
                                        duration_ms => Duration, success => true}),
            ScrubbedMsg = bc_scrubber:scrub_message(FullMsg),
            bc_session:append_message(Data#loop_data.session_pid, ScrubbedMsg),
            ToolCalls = bc_tool_parser:parse(ScrubbedMsg),
            NewData = Data#loop_data{tool_calls = ToolCalls},
            case ToolCalls of
                [] -> {next_state, finalizing, NewData};
                _  -> maybe_await_approval(NewData)
            end;
        {stream_error, _Pid, Reason} ->
            logger:error("[loop] stream error: ~p", [Reason]),
            {next_state, finalizing, Data}
    after 60000 ->
        logger:error("[loop] stream timeout"),
        {next_state, finalizing, Data}
    end.

maybe_await_approval(#loop_data{tool_calls = Calls} = Data) ->
    NeedsApproval = lists:any(fun(TC) ->
        case bc_tool_registry:lookup(TC#bc_tool_call.name) of
            {ok, {Mod, _}} -> Mod:requires_approval();
            _               -> false
        end
    end, Calls),
    case NeedsApproval of
        true  -> {next_state, awaiting_approval, Data};
        false -> {next_state, executing_tools, Data}
    end.

execute_tool_calls(Data) ->
    Results = lists:map(fun(TC) ->
        bc_obs:emit(tool_call_start, #{tool_name  => TC#bc_tool_call.name,
                                       args       => TC#bc_tool_call.args,
                                       session_id => Data#loop_data.session_id}),
        T0 = erlang:monotonic_time(millisecond),
        Result = run_tool(TC, Data),
        Duration = erlang:monotonic_time(millisecond) - T0,
        bc_obs:emit(tool_call_result, #{tool_name  => TC#bc_tool_call.name,
                                        duration_ms => Duration,
                                        success    => element(1, Result) =:= ok,
                                        session_id => Data#loop_data.session_id}),
        #bc_tool_result{
            tool_call_id = TC#bc_tool_call.id,
            name         = TC#bc_tool_call.name,
            content      = element(2, Result),
            is_error     = element(1, Result) =:= error
        }
    end, Data#loop_data.tool_calls),
    %% Scrub credentials from tool results before appending to history
    Scrubbed = [bc_scrubber:scrub_result(R) || R <- Results],
    ToolMsgs = [result_to_message(R) || R <- Scrubbed],
    lists:foreach(fun(M) -> bc_session:append_message(Data#loop_data.session_pid, M) end,
                  ToolMsgs),
    NewData = Data#loop_data{
        tool_calls = [],
        iteration  = Data#loop_data.iteration + 1
    },
    {next_state, streaming, NewData}.

run_tool(#bc_tool_call{name = Name, args = Args}, Data) ->
    SessionRef = make_session_ref(Data),
    case bc_tool_registry:lookup(Name) of
        {ok, {Mod, _Def}} ->
            Mod:execute(Args, SessionRef, #{});
        {error, not_found} ->
            %% Try MCP
            case bc_mcp_registry:lookup(Name) of
                {ok, {ServerPid, _}} ->
                    bc_mcp_server:call_tool(ServerPid, Name, Args);
                {error, not_found} ->
                    {error, <<"tool not found">>}
            end
    end.

result_to_message(#bc_tool_result{tool_call_id = Id, name = Name,
                                   content = Content, is_error = IsErr}) ->
    #bc_message{
        id           = generate_id(),
        role         = tool,
        content      = Content,
        tool_call_id = Id,
        name         = Name,
        ts           = erlang:system_time(millisecond),
        tool_calls   = [{is_error, IsErr}]
    }.

generate_id() ->
    <<N:128>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("~32.16.0b", [N])).
