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

-module(bc_pii_tokenizer_tests).
-include_lib("eunit/include/eunit.hrl").

%% ---------------------------------------------------------------------------
%% Fixture
%% ---------------------------------------------------------------------------

tokenizer_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun passthrough_no_pii_t/1,
      fun openai_key_roundtrip_t/1,
      fun github_pat_roundtrip_t/1,
      fun aws_key_roundtrip_t/1,
      fun bearer_token_roundtrip_t/1,
      fun email_roundtrip_t/1,
      fun ssn_roundtrip_t/1,
      fun idempotent_t/1,
      fun multiple_secrets_t/1,
      fun clear_t/1,
      fun token_format_t/1,
      fun custom_pattern_t/1,
      fun empty_text_t/1,
      fun credit_card_roundtrip_t/1,
      fun phone_roundtrip_t/1]}.

setup() ->
    {ok, Pid} = bc_pii_tokenizer:start_link(),
    Pid.

cleanup(Pid) ->
    bc_pii_tokenizer:stop(Pid).

%% ---------------------------------------------------------------------------
%% Tests
%% ---------------------------------------------------------------------------

passthrough_no_pii_t(Pid) ->
    [fun() ->
        Input = <<"Hello, this is plain text with no secrets.">>,
        Result = bc_pii_tokenizer:tokenize(Pid, Input),
        ?assertEqual(Input, Result)
    end].

openai_key_roundtrip_t(Pid) ->
    [fun() ->
        Key = <<"sk-abc123def456ghi789jkl012mno345pqr678stu901">>,
        Input = <<"api_key = ", Key/binary, " # my key">>,
        Tokenized = bc_pii_tokenizer:tokenize(Pid, Input),
        ?assert(binary:match(Tokenized, <<"<<PII:tok_">>) =/= nomatch),
        ?assertEqual(nomatch, binary:match(Tokenized, Key)),
        Restored = bc_pii_tokenizer:detokenize(Pid, Tokenized),
        ?assertEqual(Input, Restored)
    end].

github_pat_roundtrip_t(Pid) ->
    [fun() ->
        Pat = <<"ghp_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmn">>,
        Input = <<"token: ", Pat/binary>>,
        Tokenized = bc_pii_tokenizer:tokenize(Pid, Input),
        ?assertEqual(nomatch, binary:match(Tokenized, Pat)),
        Restored = bc_pii_tokenizer:detokenize(Pid, Tokenized),
        ?assertEqual(Input, Restored)
    end].

aws_key_roundtrip_t(Pid) ->
    [fun() ->
        Key = <<"AKIAIOSFODNN7EXAMPLE">>,
        Input = <<"aws_access_key_id = ", Key/binary>>,
        Tokenized = bc_pii_tokenizer:tokenize(Pid, Input),
        ?assertEqual(nomatch, binary:match(Tokenized, Key)),
        Restored = bc_pii_tokenizer:detokenize(Pid, Tokenized),
        ?assertEqual(Input, Restored)
    end].

bearer_token_roundtrip_t(Pid) ->
    [fun() ->
        Input = <<"Authorization: Bearer eyJhbGciOiJIUzI1NiJ9.test.sig">>,
        Tokenized = bc_pii_tokenizer:tokenize(Pid, Input),
        ?assertEqual(nomatch, binary:match(Tokenized, <<"eyJhbGci">>)),
        Restored = bc_pii_tokenizer:detokenize(Pid, Tokenized),
        ?assertEqual(Input, Restored)
    end].

email_roundtrip_t(Pid) ->
    [fun() ->
        Input = <<"Contact: user@example.com for details">>,
        Tokenized = bc_pii_tokenizer:tokenize(Pid, Input),
        ?assertEqual(nomatch, binary:match(Tokenized, <<"user@example.com">>)),
        Restored = bc_pii_tokenizer:detokenize(Pid, Tokenized),
        ?assertEqual(Input, Restored)
    end].

ssn_roundtrip_t(Pid) ->
    [fun() ->
        Input = <<"SSN: 123-45-6789">>,
        Tokenized = bc_pii_tokenizer:tokenize(Pid, Input),
        ?assertEqual(nomatch, binary:match(Tokenized, <<"123-45-6789">>)),
        Restored = bc_pii_tokenizer:detokenize(Pid, Tokenized),
        ?assertEqual(Input, Restored)
    end].

idempotent_t(Pid) ->
    [fun() ->
        Key = <<"sk-samekeyusedtwice12345678901234567890">>,
        Input1 = <<"key1: ", Key/binary>>,
        Input2 = <<"key2: ", Key/binary>>,
        T1 = bc_pii_tokenizer:tokenize(Pid, Input1),
        T2 = bc_pii_tokenizer:tokenize(Pid, Input2),
        %% Same key should get same token
        {match, [{_, Len1}]} = re:run(T1, <<"<<PII:tok_\\d+>>">>, [{capture, first, index}]),
        {match, [{_, Len2}]} = re:run(T2, <<"<<PII:tok_\\d+>>">>, [{capture, first, index}]),
        Token1 = binary:part(T1, byte_size(<<"key1: ">>), Len1),
        Token2 = binary:part(T2, byte_size(<<"key2: ">>), Len2),
        ?assertEqual(Token1, Token2)
    end].

multiple_secrets_t(Pid) ->
    [fun() ->
        Input = <<"key=sk-abc123def456ghi789jkl012mno345pqr678stu901 "
                  "email=user@test.com">>,
        Tokenized = bc_pii_tokenizer:tokenize(Pid, Input),
        ?assertEqual(nomatch, binary:match(Tokenized, <<"sk-abc">>)),
        ?assertEqual(nomatch, binary:match(Tokenized, <<"user@test.com">>)),
        Restored = bc_pii_tokenizer:detokenize(Pid, Tokenized),
        ?assertEqual(Input, Restored)
    end].

clear_t(Pid) ->
    [fun() ->
        Input = <<"sk-cleartest1234567890123456789012345678">>,
        _ = bc_pii_tokenizer:tokenize(Pid, Input),
        ok = bc_pii_tokenizer:clear(Pid),
        %% After clear, detokenize should not find mappings
        Detok = bc_pii_tokenizer:detokenize(Pid, <<"<<PII:tok_0001>>">>),
        ?assertEqual(<<"<<PII:tok_0001>>">>, Detok)
    end].

token_format_t(Pid) ->
    [fun() ->
        Input = <<"sk-tokenformattest123456789012345678901">>,
        Tokenized = bc_pii_tokenizer:tokenize(Pid, Input),
        ?assertMatch({match, _}, re:run(Tokenized, <<"<<PII:tok_\\d{4}>>">>))
    end].

custom_pattern_t(_Pid) ->
    [fun() ->
        %% Start a tokenizer with a custom pattern
        {ok, Pid2} = bc_pii_tokenizer:start_link(
            #{patterns => ["CUSTOM-[A-Z]{8}"]}),
        Input = <<"value=CUSTOM-ABCDEFGH here">>,
        Tokenized = bc_pii_tokenizer:tokenize(Pid2, Input),
        ?assertEqual(nomatch, binary:match(Tokenized, <<"CUSTOM-ABCDEFGH">>)),
        Restored = bc_pii_tokenizer:detokenize(Pid2, Tokenized),
        ?assertEqual(Input, Restored),
        bc_pii_tokenizer:stop(Pid2)
    end].

empty_text_t(Pid) ->
    [fun() ->
        ?assertEqual(<<>>, bc_pii_tokenizer:tokenize(Pid, <<>>)),
        ?assertEqual(<<>>, bc_pii_tokenizer:detokenize(Pid, <<>>))
    end].

credit_card_roundtrip_t(Pid) ->
    [fun() ->
        Input = <<"card: 4111 1111 1111 1111 end">>,
        Tokenized = bc_pii_tokenizer:tokenize(Pid, Input),
        ?assertEqual(nomatch, binary:match(Tokenized, <<"4111 1111 1111 1111">>)),
        Restored = bc_pii_tokenizer:detokenize(Pid, Tokenized),
        ?assertEqual(Input, Restored)
    end].

phone_roundtrip_t(Pid) ->
    [fun() ->
        Input = <<"Call (555) 123-4567 now">>,
        Tokenized = bc_pii_tokenizer:tokenize(Pid, Input),
        ?assertEqual(nomatch, binary:match(Tokenized, <<"(555) 123-4567">>)),
        Restored = bc_pii_tokenizer:detokenize(Pid, Tokenized),
        ?assertEqual(Input, Restored)
    end].
