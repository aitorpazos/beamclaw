-module(bc_gateway_channels_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 30},
    Children = channel_children(),
    {ok, {SupFlags, Children}}.

channel_children() ->
    Channels = bc_config:get(beamclaw_gateway, channels, []),
    lists:filtermap(fun({telegram, Cfg}) ->
        {true, #{id       => bc_channel_telegram,
                 start    => {bc_channel_telegram, start_link, [Cfg]},
                 restart  => permanent,
                 shutdown => 5000,
                 type     => worker,
                 modules  => [bc_channel_telegram]}};
    ({tui, #{enabled := true} = Cfg}) ->
        {true, #{id       => bc_channel_tui,
                 start    => {bc_channel_tui, start_link, [Cfg]},
                 restart  => permanent,
                 shutdown => 5000,
                 type     => worker,
                 modules  => [bc_channel_tui]}};
    (_) ->
        false
    end, Channels).
