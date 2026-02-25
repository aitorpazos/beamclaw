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

-module(bc_sandbox_registry_tests).
-include_lib("eunit/include/eunit.hrl").

%% ---------------------------------------------------------------------------
%% Test fixtures
%% ---------------------------------------------------------------------------

registry_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun lookup_not_found_t/1,
      fun register_and_lookup_t/1,
      fun scope_isolation_t/1,
      fun destroy_t/1,
      fun dead_process_cleanup_t/1,
      fun list_all_t/1]}.

setup() ->
    %% Start the registry gen_server standalone (not full app)
    {ok, Pid} = bc_sandbox_registry:start_link(),
    Pid.

cleanup(Pid) ->
    %% Unlink so test process doesn't die when we stop the gen_server
    unlink(Pid),
    Ref = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive {'DOWN', Ref, process, Pid, _} -> ok
    after 5000 -> error(cleanup_timeout)
    end,
    %% ETS table is owned by the gen_server, so it's gone now
    ok.

%% ---------------------------------------------------------------------------
%% Tests
%% ---------------------------------------------------------------------------

lookup_not_found_t(_Pid) ->
    [fun() ->
        ?assertEqual({error, not_found},
                     bc_sandbox_registry:lookup(<<"unknown">>, session))
    end].

register_and_lookup_t(_Pid) ->
    [fun() ->
        %% Simulate: spawn a process that acts as a "sandbox"
        FakeSandbox = spawn_link(fun() -> receive stop -> ok end end),
        %% Insert directly into ETS for lookup testing
        ets:insert(bc_sandbox_registry, {{<<"sess1">>, session}, FakeSandbox}),
        {ok, Found} = bc_sandbox_registry:lookup(<<"sess1">>, session),
        ?assertEqual(FakeSandbox, Found),
        FakeSandbox ! stop
    end].

scope_isolation_t(_Pid) ->
    [fun() ->
        P1 = spawn_link(fun() -> receive stop -> ok end end),
        P2 = spawn_link(fun() -> receive stop -> ok end end),
        ets:insert(bc_sandbox_registry, {{<<"sess1">>, session}, P1}),
        ets:insert(bc_sandbox_registry, {{<<"sess1">>, agent}, P2}),
        {ok, F1} = bc_sandbox_registry:lookup(<<"sess1">>, session),
        {ok, F2} = bc_sandbox_registry:lookup(<<"sess1">>, agent),
        ?assertEqual(P1, F1),
        ?assertEqual(P2, F2),
        ?assertNotEqual(F1, F2),
        P1 ! stop,
        P2 ! stop
    end].

destroy_t(_Pid) ->
    [fun() ->
        %% Test that destroying a non-existent entry is ok
        ?assertEqual(ok, bc_sandbox_registry:destroy(<<"nonexistent">>, session)),
        %% Test that ETS entry removal works by inserting + manually deleting
        Dummy = spawn_link(fun() -> receive stop -> ok end end),
        ets:insert(bc_sandbox_registry, {{<<"sess1">>, session}, Dummy}),
        ?assertMatch({ok, _}, bc_sandbox_registry:lookup(<<"sess1">>, session)),
        %% Remove entry manually (destroy uses bc_sandbox:stop which needs
        %% a real gen_server — unit test only validates ETS cleanup path)
        ets:delete(bc_sandbox_registry, {<<"sess1">>, session}),
        ?assertEqual({error, not_found},
                     bc_sandbox_registry:lookup(<<"sess1">>, session)),
        Dummy ! stop
    end].

dead_process_cleanup_t(_RegPid) ->
    [fun() ->
        %% Spawn a process, register it, then kill it
        Proc = spawn(fun() -> receive stop -> ok end end),
        ets:insert(bc_sandbox_registry, {{<<"sess-dead">>, session}, Proc}),
        %% Monitor the process from the registry by sending it a message
        %% that triggers monitoring (the actual registry monitors on get_or_create,
        %% so for this unit test we just verify lookup detects dead process)
        exit(Proc, kill),
        timer:sleep(50),
        ?assertEqual({error, not_found},
                     bc_sandbox_registry:lookup(<<"sess-dead">>, session))
    end].

list_all_t(_Pid) ->
    [fun() ->
        P1 = spawn_link(fun() -> receive stop -> ok end end),
        P2 = spawn_link(fun() -> receive stop -> ok end end),
        ets:insert(bc_sandbox_registry, {{<<"s1">>, session}, P1}),
        ets:insert(bc_sandbox_registry, {{<<"s2">>, session}, P2}),
        All = bc_sandbox_registry:list_all(),
        ?assertEqual(2, length(All)),
        P1 ! stop,
        P2 ! stop
    end].
