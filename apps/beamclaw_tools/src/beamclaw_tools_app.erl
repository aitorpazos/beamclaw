-module(beamclaw_tools_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    beamclaw_tools_sup:start_link().

stop(_State) ->
    ok.
