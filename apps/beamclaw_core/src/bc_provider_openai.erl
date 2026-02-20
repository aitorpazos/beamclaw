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
