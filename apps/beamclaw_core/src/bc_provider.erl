%% @doc LLM provider behaviour.
%%
%% All provider implementations must export these callbacks.
%% Providers run as gen_servers (bc_provider_openrouter, bc_provider_openai)
%% started transiently within each bc_session_sup.
-module(bc_provider).

-include_lib("beamclaw_core/include/bc_types.hrl").

%% Initialise provider state from config.
-callback init(Config :: map()) -> {ok, State :: term()} | {error, term()}.

%% Synchronous completion — blocks until the LLM responds.
-callback complete(Messages :: [#bc_message{}], Options :: map(), State :: term()) ->
    {ok, #bc_message{}, NewState :: term()} | {error, Reason :: term(), NewState :: term()}.

%% Streaming completion — sends chunks to CallerPid:
%%   {stream_chunk, self(), Chunk :: binary()}
%%   {stream_done,  self(), FullMessage :: #bc_message{}}
%%   {stream_error, self(), Reason :: term()}
-callback stream(Messages :: [#bc_message{}], Options :: map(),
                 CallerPid :: pid(), State :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term(), NewState :: term()}.

%% Return provider capabilities (e.g., supports_streaming, supports_tools).
-callback capabilities(State :: term()) -> map().

-callback terminate(Reason :: term(), State :: term()) -> ok.
