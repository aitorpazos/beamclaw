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

%% @doc Per-session approval workflow.
%%
%% Started transiently within bc_session_sup on first tool requiring approval.
%% Decision logic:
%%   1. Tool in session-scoped allowlist → auto-approve
%%   2. autonomy_level =:= full → auto-approve
%%   3. autonomy_level =:= read_only → deny immediately
%%   4. Otherwise: send prompt to channel; wait for /yes, /no, /always
%%      /always → add to allowlist; approve
-module(bc_approval).
-behaviour(gen_server).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/1, request/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    session_id   :: binary(),
    autonomy     :: autonomy_level(),
    allowlist    :: sets:set(binary()),   %% tool names user approved "always"
    channel_mod  :: module(),
    pending      :: {reference(), gen_server:from(), #bc_tool_call{}} | undefined
}).

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc Request approval for a tool call. Returns approved | denied.
-spec request(Pid :: pid(), ToolCall :: #bc_tool_call{}, Autonomy :: autonomy_level()) ->
    approved | denied.
request(Pid, ToolCall, Autonomy) ->
    gen_server:call(Pid, {request, ToolCall, Autonomy}, 120000).

init(Config) ->
    {ok, #state{
        session_id  = maps:get(session_id,  Config),
        autonomy    = maps:get(autonomy,    Config, supervised),
        allowlist   = sets:new(),
        channel_mod = maps:get(channel_mod, Config, bc_channel_tui),
        pending     = undefined
    }}.

handle_call({request, TC, Autonomy}, _From, State) ->
    Name = TC#bc_tool_call.name,
    Decision = case {Autonomy, sets:is_element(Name, State#state.allowlist)} of
        {_, true}      -> approved;
        {full, _}      -> approved;
        {read_only, _} -> denied;
        _              ->
            %% Need user confirmation
            bc_obs:emit(approval_requested, #{tool_name  => Name,
                                              args       => TC#bc_tool_call.args,
                                              session_id => State#state.session_id}),
            %% TODO: send prompt to channel and wait for reply
            %% For now: auto-approve in supervised mode (to be wired to channel)
            approved
    end,
    bc_obs:emit(approval_resolved, #{tool_name  => Name,
                                     decision   => Decision,
                                     session_id => State#state.session_id}),
    {reply, Decision, State};
handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast({channel_reply, Reply}, #state{pending = {_Ref, From, TC}} = State) ->
    {Decision, NewAllowlist} = case Reply of
        always ->
            {approved, sets:add_element(TC#bc_tool_call.name, State#state.allowlist)};
        yes    ->
            {approved, State#state.allowlist};
        no     ->
            {denied, State#state.allowlist}
    end,
    gen_server:reply(From, Decision),
    {noreply, State#state{pending = undefined, allowlist = NewAllowlist}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
