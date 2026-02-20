%% @doc Dynamic supervisor for MCP server connections.
%%
%% Starts one bc_mcp_server per configured MCP server.
%% Supervisor strategy: one_for_one with MaxR=5, MaxT=30 for exponential-backoff-like behaviour.
-module(bc_mcp_servers_sup).
-behaviour(supervisor).

-export([start_link/0, start_server/1, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_server(Config :: map()) -> {ok, pid()} | {error, term()}.
start_server(Config) ->
    supervisor:start_child(?MODULE, [Config]).

init([]) ->
    SupFlags = #{strategy  => simple_one_for_one,
                 intensity => 5,
                 period    => 30},
    ChildSpec = #{id       => bc_mcp_server,
                  start    => {bc_mcp_server, start_link, []},
                  restart  => transient,
                  shutdown => 5000,
                  type     => worker,
                  modules  => [bc_mcp_server]},
    %% Start configured servers
    Servers = application:get_env(beamclaw_mcp, servers, []),
    {ok, {SupFlags, [ChildSpec]}, Servers}.
