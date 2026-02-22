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

-module(bc_session_sup).
-moduledoc """
Per-session supervisor.

Children:
  bc_session  — permanent (the "lane"; holds history across loop crashes)
  bc_loop     — transient (gen_statem; can crash and be restarted safely)

Provider and bc_approval are started on demand by bc_session.
""".
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Config) ->
    supervisor:start_link(?MODULE, Config).

init(Config) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 30},
    Children = [
        #{id       => bc_session,
          start    => {bc_session, start_link, [Config]},
          restart  => permanent,
          shutdown => 10000,
          type     => worker,
          modules  => [bc_session]},
        #{id       => bc_loop,
          start    => {bc_loop, start_link, [Config]},
          restart  => transient,
          shutdown => 5000,
          type     => worker,
          modules  => [bc_loop]}
    ],
    {ok, {SupFlags, Children}}.
