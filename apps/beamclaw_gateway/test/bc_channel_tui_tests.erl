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

-module(bc_channel_tui_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Setup / teardown
%% ===================================================================

setup() ->
    %% enabled => false: suppresses start_io/read_line auto-send in init/1,
    %% so we can install the custom group leader before triggering io.
    {ok, Pid} = bc_channel_tui:start_link(#{enabled => false}),
    Pid.

teardown(Pid) ->
    case is_process_alive(Pid) of
        true  -> gen_server:stop(Pid);
        false -> ok
    end.

%% ===================================================================
%% Test suite
%% ===================================================================

tui_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun test_io_error_enters_dormant_mode/1
    ]}.

%% When io:get_line/1 returns {error, _}, the gen_server must stay alive
%% (dormant mode) rather than stopping and triggering a supervisor restart loop.
test_io_error_enters_dormant_mode(Pid) ->
    GL = spawn(fun error_gl_loop/0),
    erlang:group_leader(GL, Pid),
    Pid ! read_line,
    timer:sleep(100),
    exit(GL, kill),
    ?_assert(is_process_alive(Pid)).

%% ===================================================================
%% Helpers
%% ===================================================================

%% Minimal io server: replies {error, enotsup} to get_line requests,
%% acknowledges other io requests harmlessly.
error_gl_loop() ->
    receive
        {io_request, From, ReplyAs, {get_line, _Enc, _Prompt}} ->
            From ! {io_reply, ReplyAs, {error, enotsup}},
            error_gl_loop();
        {io_request, From, ReplyAs, _Other} ->
            From ! {io_reply, ReplyAs, ok},
            error_gl_loop();
        _ ->
            error_gl_loop()
    after 5000 ->
        ok
    end.
