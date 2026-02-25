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

-module(beamclaw_sandbox_app).
-moduledoc "OTP application callback for beamclaw_sandbox.".
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case beamclaw_sandbox_sup:start_link() of
        {ok, Pid} ->
            %% Register the exec tool when sandbox is enabled
            case application:get_env(beamclaw_sandbox, enabled, false) of
                true ->
                    Def = bc_tool_exec:definition(),
                    bc_tool_registry:register(bc_tool_exec, Def);
                false ->
                    ok
            end,
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.
