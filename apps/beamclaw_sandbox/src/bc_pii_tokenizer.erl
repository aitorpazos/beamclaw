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

-module(bc_pii_tokenizer).
-moduledoc """
Bidirectional PII tokenization at the sandbox boundary.

One instance per sandbox. Maintains an ETS mapping between original
sensitive values and opaque tokens (<<PII:tok_NNNN>>). Idempotent:
the same input always maps to the same token within a session.

Data flow:
  1. Script to container: tokenize(Script) — mask secrets in code
  2. Tool call from container: detokenize(Args) — restore real values
  3. Tool result to container: tokenize(Result) — mask secrets in response
  4. Final output to LLM: detokenize(Output) — restore for model context

Patterns matched:
  - API keys: sk-*, ghp_*, ghs_*, AKIA*
  - Bearer tokens
  - Emails
  - Credit card numbers (Luhn-like 13-19 digit sequences)
  - US phone numbers
  - US SSN patterns
  - Custom patterns from config
""".
-behaviour(gen_server).

-export([start_link/0, start_link/1, tokenize/2, detokenize/2, clear/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Pure functions exported for testing
-export([default_patterns/0, find_matches/2]).

-record(state, {
    forward  :: ets:tid(),   %% original -> token
    reverse  :: ets:tid(),   %% token -> original
    counter  :: non_neg_integer(),
    patterns :: [re:mp()]
}).

start_link() ->
    start_link(#{}).

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

-doc "Replace PII in text with tokens. Idempotent per session.".
-spec tokenize(Pid :: pid(), Text :: binary()) -> binary().
tokenize(Pid, Text) ->
    gen_server:call(Pid, {tokenize, Text}).

-doc "Restore original values from tokens in text.".
-spec detokenize(Pid :: pid(), Text :: binary()) -> binary().
detokenize(Pid, Text) ->
    gen_server:call(Pid, {detokenize, Text}).

-doc "Clear all token mappings.".
-spec clear(Pid :: pid()) -> ok.
clear(Pid) ->
    gen_server:call(Pid, clear).

-doc "Stop the tokenizer.".
-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

init(Config) ->
    Forward = ets:new(pii_forward, [set, private]),
    Reverse = ets:new(pii_reverse, [set, private]),
    CustomPatterns = maps:get(patterns, Config, []),
    Compiled = compile_patterns(default_patterns() ++ CustomPatterns),
    {ok, #state{forward = Forward, reverse = Reverse,
                counter = 0, patterns = Compiled}}.

handle_call({tokenize, Text}, _From, State) ->
    {Result, NewState} = do_tokenize(Text, State),
    {reply, Result, NewState};
handle_call({detokenize, Text}, _From, State) ->
    {reply, do_detokenize(Text, State), State};
handle_call(clear, _From, #state{forward = F, reverse = R} = State) ->
    ets:delete_all_objects(F),
    ets:delete_all_objects(R),
    {reply, ok, State#state{counter = 0}};
handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{forward = F, reverse = R}) ->
    ets:delete(F),
    ets:delete(R),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Default patterns
%% ---------------------------------------------------------------------------

-doc "Default PII patterns as regex strings.".
-spec default_patterns() -> [string()].
default_patterns() ->
    [
     %% OpenAI API keys
     "sk-[A-Za-z0-9]{20,}",
     %% GitHub PATs
     "gh[ps]_[A-Za-z0-9]{36,}",
     %% AWS access keys
     "AKIA[A-Z0-9]{16}",
     %% Bearer tokens
     "Bearer\\s+[A-Za-z0-9\\-._~+/]+=*",
     %% Email addresses
     "[a-zA-Z0-9._%+\\-]+@[a-zA-Z0-9.\\-]+\\.[a-zA-Z]{2,}",
     %% Credit card numbers (13-19 digits, optionally space/dash separated)
     "\\b(?:\\d[ \\-]?){12,18}\\d\\b",
     %% US SSN
     "\\b\\d{3}-\\d{2}-\\d{4}\\b",
     %% US phone numbers
     "\\b(?:\\+1[\\- ]?)?\\(?\\d{3}\\)?[\\- ]?\\d{3}[\\- ]?\\d{4}\\b"
    ].

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

compile_patterns(PatternStrs) ->
    lists:filtermap(fun(P) ->
        case re:compile(P, [unicode]) of
            {ok, MP} -> {true, MP};
            {error, _} -> false
        end
    end, PatternStrs).

do_tokenize(Text, State) ->
    Matches = find_matches(Text, State#state.patterns),
    %% Sort by position descending so replacements don't shift offsets
    Sorted = lists:sort(fun({PosA, _}, {PosB, _}) -> PosA > PosB end, Matches),
    {Result, NewState} = lists:foldl(fun({Pos, Len}, {Txt, St}) ->
        Original = binary:part(Txt, Pos, Len),
        {Token, St2} = get_or_create_token(Original, St),
        Before = binary:part(Txt, 0, Pos),
        After = binary:part(Txt, Pos + Len, byte_size(Txt) - Pos - Len),
        {<<Before/binary, Token/binary, After/binary>>, St2}
    end, {Text, State}, Sorted),
    {Result, NewState}.

do_detokenize(Text, #state{reverse = Reverse}) ->
    %% Find all <<PII:tok_NNNN>> tokens and replace with originals
    case re:run(Text, <<"<<PII:tok_\\d+>>">>, [global, {capture, all, index}]) of
        {match, Matches} ->
            Sorted = lists:sort(fun([{PosA, _}], [{PosB, _}]) ->
                PosA > PosB
            end, Matches),
            lists:foldl(fun([{Pos, Len}], Txt) ->
                Token = binary:part(Txt, Pos, Len),
                case ets:lookup(Reverse, Token) of
                    [{Token, Original}] ->
                        Before = binary:part(Txt, 0, Pos),
                        After = binary:part(Txt, Pos + Len, byte_size(Txt) - Pos - Len),
                        <<Before/binary, Original/binary, After/binary>>;
                    [] ->
                        Txt
                end
            end, Text, Sorted);
        nomatch ->
            Text
    end.

find_matches(Text, Patterns) ->
    AllMatches = lists:flatmap(fun(Pattern) ->
        case re:run(Text, Pattern, [global, {capture, all, index}]) of
            {match, Matches} -> [hd(M) || M <- Matches];
            nomatch -> []
        end
    end, Patterns),
    %% Deduplicate overlapping matches — keep the longest at each position
    deduplicate_matches(AllMatches).

deduplicate_matches(Matches) ->
    %% Sort by position, then by length descending
    Sorted = lists:sort(fun({PA, LA}, {PB, LB}) ->
        case PA =:= PB of
            true  -> LA > LB;
            false -> PA < PB
        end
    end, Matches),
    remove_overlaps(Sorted, []).

remove_overlaps([], Acc) ->
    lists:reverse(Acc);
remove_overlaps([{Pos, Len} | Rest], []) ->
    remove_overlaps(Rest, [{Pos, Len}]);
remove_overlaps([{Pos, _Len} | Rest], [{PrevPos, PrevLen} | _] = Acc)
  when Pos < PrevPos + PrevLen ->
    %% Overlaps with previous — skip
    remove_overlaps(Rest, Acc);
remove_overlaps([{Pos, Len} | Rest], Acc) ->
    remove_overlaps(Rest, [{Pos, Len} | Acc]).

get_or_create_token(Original, #state{forward = F, reverse = R,
                                      counter = C} = State) ->
    case ets:lookup(F, Original) of
        [{Original, ExistingToken}] ->
            {ExistingToken, State};
        [] ->
            NewC = C + 1,
            Token = iolist_to_binary(
                io_lib:format("<<PII:tok_~4..0B>>", [NewC])),
            ets:insert(F, {Original, Token}),
            ets:insert(R, {Token, Original}),
            {Token, State#state{counter = NewC}}
    end.
