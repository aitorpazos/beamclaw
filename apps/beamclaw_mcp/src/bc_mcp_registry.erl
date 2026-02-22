%%
%% Copyright Péter Dimitrov 2026, All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

%% @doc MCP tool registry — maps tool_name → {server_pid, server_name}.
%%
%% When a bc_mcp_server discovers tools via tools/list, it calls
%% register_tools/3 here. Each server PID is monitored; when the process
%% dies (e.g. port crash before restart), its ETS entries are cleaned up
%% automatically so stale tool→pid mappings do not accumulate.
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

%% State: #{monitors => #{pid() => reference()}}
init([]) ->
    _ = ets:new(?TAB, [set, named_table, public, {read_concurrency, true}]),
    {ok, #{monitors => #{}}}.

handle_cast({register, Pid, Name, Tools}, #{monitors := Mons} = State) ->
    %% Monitor if not already monitored (handles re-registration after restart
    %% with new PID; old PID's DOWN already cleaned its entries).
    NewMons = case maps:is_key(Pid, Mons) of
        true  -> Mons;
        false ->
            Ref = erlang:monitor(process, Pid),
            Mons#{Pid => Ref}
    end,
    lists:foreach(fun(Tool) ->
        ToolName = maps:get(<<"name">>, Tool, maps:get(name, Tool, <<>>)),
        ets:insert(?TAB, {ToolName, Pid, Name})
    end, Tools),
    {noreply, State#{monitors := NewMons}};
handle_cast({unregister, Pid}, #{monitors := Mons} = State) ->
    ets:match_delete(?TAB, {'_', Pid, '_'}),
    NewMons = case maps:get(Pid, Mons, undefined) of
        undefined -> Mons;
        Ref ->
            erlang:demonitor(Ref, [flush]),
            maps:remove(Pid, Mons)
    end,
    {noreply, State#{monitors := NewMons}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #{monitors := Mons} = State) ->
    %% Server process died — remove all its tool entries from ETS.
    ets:match_delete(?TAB, {'_', Pid, '_'}),
    {noreply, State#{monitors := maps:remove(Pid, Mons)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
