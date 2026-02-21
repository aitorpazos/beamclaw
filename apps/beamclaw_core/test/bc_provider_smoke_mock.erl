%% @doc Mock LLM provider for smoke tests.
%%
%% stream/4 sends canned responses directly to CallerPid before returning,
%% so bc_loop:receive_stream/2 finds them in the mailbox immediately.
%% No HTTP calls, no external dependencies.
-module(bc_provider_smoke_mock).
-behaviour(bc_provider).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([init/1, complete/3, stream/4, capabilities/1, terminate/2]).

init(_Config) ->
    {ok, #{}}.

complete(_Messages, _Options, State) ->
    Msg = #bc_message{
        id      = <<"mock-complete-1">>,
        role    = assistant,
        content = <<"Hello, world!">>,
        ts      = 0
    },
    {ok, Msg, State}.

stream(_Messages, _Options, CallerPid, State) ->
    CallerPid ! {stream_chunk, self(), <<"Hello">>},
    Msg = #bc_message{
        id      = <<"mock-1">>,
        role    = assistant,
        content = <<"Hello, world!">>,
        ts      = 0
    },
    CallerPid ! {stream_done, self(), Msg},
    {ok, State}.

capabilities(_State) ->
    #{supports_streaming => true, supports_tools => false}.

terminate(_Reason, _State) ->
    ok.
