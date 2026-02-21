%% @doc EUnit tests for bc_compactor context compaction.
%%
%% These tests cover the no-op path (history shorter than or equal to target).
%% The LLM-failure fallback path requires a live HTTP endpoint; it is covered
%% by integration/smoke tests rather than unit tests.
-module(bc_compactor_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% ---- Mock session process ----
%%
%% Responds to the gen_server wire protocol used by bc_session:get_history/1
%% (gen_server:call) and bc_session:set_history/2 (gen_server:cast).

mock_session(History) ->
    spawn(fun() -> mock_session_loop(History) end).

mock_session_loop(History) ->
    receive
        {'$gen_call', From, get_history} ->
            gen_server:reply(From, History),
            mock_session_loop(History);
        {'$gen_cast', {set_history, NewHistory}} ->
            mock_session_loop(NewHistory);
        _ ->
            mock_session_loop(History)
    end.

make_msg(N) ->
    #bc_message{
        id      = integer_to_binary(N),
        role    = user,
        content = iolist_to_binary(["msg ", integer_to_list(N)]),
        ts      = N
    }.

%% ---- Tests ----

%% History shorter than compaction_target → compact is a no-op.
compact_short_history_test() ->
    application:set_env(beamclaw_core, agentic_loop, #{compaction_target => 20}),
    History = [make_msg(I) || I <- lists:seq(1, 5)],
    Pid     = mock_session(History),
    Result  = bc_compactor:compact(Pid),
    exit(Pid, kill),
    ?assertEqual(ok, Result).

%% History exactly at compaction_target → also a no-op (=< is inclusive).
compact_at_threshold_test() ->
    application:set_env(beamclaw_core, agentic_loop, #{compaction_target => 10}),
    History = [make_msg(I) || I <- lists:seq(1, 10)],
    Pid     = mock_session(History),
    Result  = bc_compactor:compact(Pid),
    exit(Pid, kill),
    ?assertEqual(ok, Result).

%% With no agentic_loop config, default target (20) is used — still a no-op
%% for a short history.
compact_default_target_test() ->
    application:unset_env(beamclaw_core, agentic_loop),
    History = [make_msg(I) || I <- lists:seq(1, 3)],
    Pid     = mock_session(History),
    Result  = bc_compactor:compact(Pid),
    exit(Pid, kill),
    ?assertEqual(ok, Result).
