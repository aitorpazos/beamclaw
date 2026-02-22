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

%% @doc OpenAI LLM provider stub.
-module(bc_provider_openai).
-behaviour(bc_provider).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/1]).
-export([init/1, complete/3, stream/4, capabilities/1, terminate/2]).

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

init(Config) ->
    ApiKey  = bc_config:resolve(maps:get(api_key,  Config, {env, "OPENAI_API_KEY"})),
    BaseUrl = maps:get(base_url, Config, "https://api.openai.com/v1"),
    Model   = maps:get(model,    Config, "gpt-4o"),
    {ok, #{api_key => ApiKey, base_url => BaseUrl, model => Model}}.

complete(Messages, Options, State) ->
    bc_provider_openrouter:complete(Messages, Options, State).

stream(Messages, Options, CallerPid, State) ->
    bc_provider_openrouter:stream(Messages, Options, CallerPid, State).

capabilities(_State) ->
    #{supports_streaming => true, supports_tools => true}.

terminate(_Reason, _State) ->
    ok.
