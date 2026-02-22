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

-module(bc_sessions_sup).
-moduledoc "Dynamic supervisor for per-session supervisors.".
-behaviour(supervisor).

-export([start_link/0, start_session/1, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_session(Config :: map()) -> {ok, pid()} | {error, term()}.
start_session(Config) ->
    supervisor:start_child(?MODULE, [Config]).

init([]) ->
    SupFlags = #{strategy  => simple_one_for_one,
                 intensity => 10,
                 period    => 60},
    ChildSpec = #{id       => bc_session_sup,
                  start    => {bc_session_sup, start_link, []},
                  restart  => temporary,
                  shutdown => infinity,
                  type     => supervisor,
                  modules  => [bc_session_sup]},
    {ok, {SupFlags, [ChildSpec]}}.
