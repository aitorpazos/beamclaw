%% @doc MCP tool registry — maps tool_name → {server_pid, server_name}.
%%
%% When a bc_mcp_server discovers tools via tools/list, it registers them here.
-module(bc_mcp_registry).
-behaviour(gen_server).

-export([start_link/0, register_tools/3, lookup/1, unregister_server/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TAB, bc_mcp_registry).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_tools(ServerPid :: pid(), ServerName :: binary(),
                     Tools :: [map()]) -> ok.
register_tools(ServerPid, ServerName, Tools) ->
    gen_server:cast(?MODULE, {register, ServerPid, ServerName, Tools}).

-spec lookup(ToolName :: binary()) ->
    {ok, {pid(), binary()}} | {error, not_found}.
lookup(ToolName) ->
    case ets:lookup(?TAB, ToolName) of
        [{ToolName, Pid, Name}] -> {ok, {Pid, Name}};
        []                      -> {error, not_found}
    end.

-spec unregister_server(ServerPid :: pid()) -> ok.
unregister_server(ServerPid) ->
    gen_server:cast(?MODULE, {unregister, ServerPid}).

init([]) ->
    ets:new(?TAB, [set, named_table, public, {read_concurrency, true}]),
    {ok, #{}}.

handle_cast({register, Pid, Name, Tools}, State) ->
    lists:foreach(fun(Tool) ->
        ToolName = maps:get(name, Tool, maps:get(<<"name">>, Tool, <<>>)),
        ets:insert(?TAB, {ToolName, Pid, Name})
    end, Tools),
    {noreply, State};
handle_cast({unregister, Pid}, State) ->
    ets:match_delete(?TAB, {'_', Pid, '_'}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
