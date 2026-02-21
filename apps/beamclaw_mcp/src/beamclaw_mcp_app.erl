-module(beamclaw_mcp_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case beamclaw_mcp_sup:start_link() of
        {ok, Pid} ->
            %% Start one bc_mcp_server per configured MCP server.
            %% Done here (not in the supervisor init) because simple_one_for_one
            %% children must be started via start_child after the sup is running.
            Servers = application:get_env(beamclaw_mcp, servers, []),
            lists:foreach(fun(Config) ->
                case bc_mcp_servers_sup:start_server(Config) of
                    {ok, _} -> ok;
                    {error, Reason} ->
                        logger:warning("[mcp] failed to start server ~p: ~p",
                                       [maps:get(name, Config, unknown), Reason])
                end
            end, Servers),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.
