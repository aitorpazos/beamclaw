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

%% @doc Context compaction.
%%
%% Triggered in the 'compacting' gen_statem state when history exceeds threshold.
%% Algorithm:
%%   1. Keep last N messages verbatim (compaction_target, default 20)
%%   2. Summarize older messages via LLM synchronous complete/3
%%   3. Summary becomes a system message: "[Conversation summary]: ..."
%%   4. Fallback on LLM failure: deterministic trim to last N messages
%%   5. Write new history back to bc_session via set_history/2
-module(bc_compactor).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([compact/1]).

-define(SUMMARY_PREFIX, <<"[Conversation summary]: ">>).

%% @doc Compact the history held in SessionPid. Writes the result back.
-spec compact(SessionPid :: pid()) -> ok.
compact(SessionPid) ->
    History = bc_session:get_history(SessionPid),
    Cfg     = bc_config:get(beamclaw_core, agentic_loop, #{}),
    Target  = maps:get(compaction_target, Cfg, 20),
    Before  = length(History),
    case Before =< Target of
        true -> ok;
        false ->
            {OldMsgs, KeepMsgs} = lists:split(Before - Target, History),
            NewHistory = case summarize(OldMsgs) of
                {ok, Summary} ->
                    SummaryMsg = #bc_message{
                        id      = generate_id(),
                        role    = system,
                        content = <<?SUMMARY_PREFIX/binary, Summary/binary>>,
                        ts      = erlang:system_time(millisecond)
                    },
                    [SummaryMsg | KeepMsgs];
                error ->
                    %% Fallback: deterministic trim
                    KeepMsgs
            end,
            After = length(NewHistory),
            bc_session:set_history(SessionPid, NewHistory),
            bc_obs:emit(compaction_complete,
                        #{before => Before, 'after' => After}),
            ok
    end.

summarize(Messages) ->
    SystemPrompt = #bc_message{
        id      = generate_id(),
        role    = system,
        content = <<"Summarize the following conversation history concisely and factually. "
                    "Focus on key decisions, facts, and context needed to continue. "
                    "Do not include greetings or filler.">>,
        ts = 0
    },
    ConvText = messages_to_text(Messages),
    UserMsg  = #bc_message{
        id      = generate_id(),
        role    = user,
        content = ConvText,
        ts      = erlang:system_time(millisecond)
    },
    ProvMod    = bc_config:get(beamclaw_core, default_provider, openrouter),
    ProvConfig = get_provider_config(ProvMod),
    case ProvMod:init(ProvConfig) of
        {ok, ProvState} ->
            case ProvMod:complete([SystemPrompt, UserMsg], #{}, ProvState) of
                {ok, #bc_message{content = Summary}, _} -> {ok, Summary};
                {error, _, _}                           -> error
            end;
        {error, _} ->
            error
    end.

messages_to_text(Messages) ->
    Parts = lists:map(fun(#bc_message{role = Role, content = Content}) ->
        R = atom_to_binary(Role, utf8),
        C = case Content of undefined -> <<>>; _ -> Content end,
        <<R/binary, ": ", C/binary, "\n">>
    end, Messages),
    iolist_to_binary(Parts).

get_provider_config(ProvMod) ->
    ProviderKey = case ProvMod of
        bc_provider_openrouter -> openrouter;
        bc_provider_openai     -> openai;
        _                      -> openrouter
    end,
    Providers = bc_config:get(beamclaw_core, providers, []),
    ProvMap = proplists:get_value(ProviderKey, Providers, #{}),
    bc_config:resolve(ProvMap).

generate_id() ->
    <<N:64>> = crypto:strong_rand_bytes(8),
    iolist_to_binary(io_lib:format("msg_~16.16.0b", [N])).
