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

%% @doc EUnit tests for bc_rate_limiter sliding-window rate limiting.
-module(bc_rate_limiter_tests).

-include_lib("eunit/include/eunit.hrl").

%% ---- Fixtures ----
%%
%% Each test in the {foreach} group gets its own fresh gen_server + ETS table.
%% teardown stops the gen_server, which (as the ETS table owner) also deletes
%% the table, so the next setup starts clean.

setup() ->
    {ok, Pid} = bc_rate_limiter:start_link(),
    Pid.

teardown(Pid) ->
    gen_server:stop(Pid).

%% ---- Test suite ----

rate_limiter_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun test_first_request_allowed/1,
        fun test_requests_within_limit/1,
        fun test_exceeds_limit/1,
        fun test_different_clients_independent/1
    ]}.

%% The very first request for a new client is always allowed.
test_first_request_allowed(_Pid) ->
    ?_assertEqual(ok, bc_rate_limiter:check(<<"client_new">>, <<"/">>)).

%% 59 requests are all within the default 60-request limit.
test_requests_within_limit(_Pid) ->
    Client  = <<"client_sub">>,
    Results = [bc_rate_limiter:check(Client, <<"/">>) || _ <- lists:seq(1, 59)],
    ?_assert(lists:all(fun(R) -> R =:= ok end, Results)).

%% After 60 requests the 61st is rate-limited.
test_exceeds_limit(_Pid) ->
    Client = <<"client_over">>,
    [bc_rate_limiter:check(Client, <<"/">>) || _ <- lists:seq(1, 60)],
    ?_assertEqual({error, rate_limited}, bc_rate_limiter:check(Client, <<"/">>)).

%% Different client IDs have independent counters.
test_different_clients_independent(_Pid) ->
    %% Exhaust client_a
    [bc_rate_limiter:check(<<"client_a">>, <<"/">>) || _ <- lists:seq(1, 60)],
    %% client_b should still be allowed
    ?_assertEqual(ok, bc_rate_limiter:check(<<"client_b">>, <<"/">>)).
