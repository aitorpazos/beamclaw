-module(beamclaw_tools_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 30},
    Children = [
        #{id       => bc_tool_registry,
          start    => {bc_tool_registry, start_link, []},
          restart  => permanent,
          shutdown => 5000,
          type     => worker,
          modules  => [bc_tool_registry]}
    ],
    {ok, {SupFlags, Children}}.
