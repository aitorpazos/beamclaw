%% @doc Per-session supervisor.
%%
%% Children:
%%   bc_session  — permanent (the "lane"; holds history across loop crashes)
%%   bc_loop     — transient (gen_statem; can crash and be restarted safely)
%%
%% Provider and bc_approval are started on demand by bc_session.
-module(bc_session_sup).
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
