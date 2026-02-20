%% @doc Credential scrubbing.
%%
%% Applied to every tool result before it enters history or is sent to the LLM.
%% Uses regex patterns to redact known credential formats.
-module(bc_scrubber).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([scrub/1, scrub_message/1, scrub_result/1]).

-define(PATTERNS, [
    %% Generic key=value patterns
    "api[_-]?key\\s*[:=]\\s*\\S+",
    "token\\s*[:=]\\s*\\S+",
    "password\\s*[:=]\\s*\\S+",
    "secret\\s*[:=]\\s*\\S+",
    %% Bearer tokens
    "Bearer\\s+\\S+",
    %% OpenAI keys
    "sk-[A-Za-z0-9_-]{20,}",
    %% GitHub PATs
    "ghp_[A-Za-z0-9]{36}",
    "ghs_[A-Za-z0-9]{36}",
    %% AWS keys (rough pattern)
    "AKIA[0-9A-Z]{16}"
]).

%% @doc Scrub a binary string.
-spec scrub(binary()) -> binary().
scrub(Text) when is_binary(Text) ->
    lists:foldl(fun(Pattern, Acc) ->
        re:replace(Acc, Pattern, <<"[REDACTED]">>,
                   [global, caseless, {return, binary}])
    end, Text, ?PATTERNS);
scrub(Other) ->
    Other.

%% @doc Scrub the content field of a bc_message.
-spec scrub_message(#bc_message{}) -> #bc_message{}.
scrub_message(#bc_message{content = Content} = Msg) when is_binary(Content) ->
    Msg#bc_message{content = scrub(Content)};
scrub_message(Msg) ->
    Msg.

%% @doc Scrub the content field of a bc_tool_result.
-spec scrub_result(#bc_tool_result{}) -> #bc_tool_result{}.
scrub_result(#bc_tool_result{content = Content} = Result) ->
    Result#bc_tool_result{content = scrub(Content)}.
