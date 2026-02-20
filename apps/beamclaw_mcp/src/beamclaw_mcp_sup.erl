-module(beamclaw_mcp_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 30},
    Children = [
        #{id       => bc_mcp_registry,
          start    => {bc_mcp_registry, start_link, []},
          restart  => permanent,
          shutdown => 5000,
          type     => worker,
          modules  => [bc_mcp_registry]},
        #{id       => bc_mcp_servers_sup,
          start    => {bc_mcp_servers_sup, start_link, []},
          restart  => permanent,
          shutdown => infinity,
          type     => supervisor,
          modules  => [bc_mcp_servers_sup]}
    ],
    {ok, {SupFlags, Children}}.
