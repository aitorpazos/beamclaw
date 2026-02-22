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

-module(bc_provider_smoke_mock).
-moduledoc """
Mock LLM provider for smoke tests.

stream/4 sends canned responses directly to CallerPid before returning,
so bc_loop:receive_stream/2 finds them in the mailbox immediately.
No HTTP calls, no external dependencies.
""".
-behaviour(bc_provider).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([init/1, complete/3, stream/4, capabilities/1, terminate/2]).

init(_Config) ->
    {ok, #{}}.

complete(_Messages, _Options, State) ->
    Msg = #bc_message{
        id      = <<"mock-complete-1">>,
        role    = assistant,
        content = <<"Hello, world!">>,
        ts      = 0
    },
    {ok, Msg, State}.

stream(_Messages, _Options, CallerPid, State) ->
    CallerPid ! {stream_chunk, self(), <<"Hello">>},
    Msg = #bc_message{
        id      = <<"mock-1">>,
        role    = assistant,
        content = <<"Hello, world!">>,
        ts      = 0
    },
    CallerPid ! {stream_done, self(), Msg},
    {ok, State}.

capabilities(_State) ->
    #{supports_streaming => true, supports_tools => false}.

terminate(_Reason, _State) ->
    ok.
