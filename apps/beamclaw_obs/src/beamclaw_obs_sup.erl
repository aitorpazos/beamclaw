-module(beamclaw_obs_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 30},
    Children = [
        child(bc_obs_manager,    worker),
        child(bc_obs_log,        worker),
        child(bc_obs_prometheus, worker)
    ],
    {ok, {SupFlags, Children}}.

child(Mod, Type) ->
    #{id       => Mod,
      start    => {Mod, start_link, []},
      restart  => permanent,
      shutdown => 5000,
      type     => Type,
      modules  => [Mod]}.
