-module(beamclaw_gateway_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 30},
    Children = [
        #{id       => bc_rate_limiter,
          start    => {bc_rate_limiter, start_link, []},
          restart  => permanent,
          shutdown => 5000,
          type     => worker,
          modules  => [bc_rate_limiter]},
        #{id       => bc_gateway_http_sup,
          start    => {bc_gateway_http_sup, start_link, []},
          restart  => permanent,
          shutdown => infinity,
          type     => supervisor,
          modules  => [bc_gateway_http_sup]},
        #{id       => bc_gateway_channels_sup,
          start    => {bc_gateway_channels_sup, start_link, []},
          restart  => permanent,
          shutdown => infinity,
          type     => supervisor,
          modules  => [bc_gateway_channels_sup]}
    ],
    {ok, {SupFlags, Children}}.
