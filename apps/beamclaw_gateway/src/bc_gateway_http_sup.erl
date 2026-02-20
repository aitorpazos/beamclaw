-module(bc_gateway_http_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 30},
    Children = [
        #{id       => bc_gateway_cowboy,
          start    => {bc_gateway_cowboy, start_link, []},
          restart  => permanent,
          shutdown => 5000,
          type     => worker,
          modules  => [bc_gateway_cowboy]}
    ],
    {ok, {SupFlags, Children}}.
