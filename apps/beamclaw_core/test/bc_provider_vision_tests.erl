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

-module(bc_provider_vision_tests).
-moduledoc "Tests for multimodal message formatting in bc_provider_openrouter.".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% ---- text-only message ----

text_only_message_test() ->
    Msg = #bc_message{role = user, content = <<"hello">>},
    Map = bc_provider_openrouter:message_to_map(Msg),
    ?assertEqual(<<"user">>, maps:get(role, Map)),
    ?assertEqual(<<"hello">>, maps:get(content, Map)).

text_only_no_attachments_test() ->
    Msg = #bc_message{role = user, content = <<"hi">>, attachments = []},
    Map = bc_provider_openrouter:message_to_map(Msg),
    ?assertEqual(<<"hi">>, maps:get(content, Map)).

%% ---- user message with image ----

user_with_image_test() ->
    B64 = base64:encode(<<"fake image">>),
    Msg = #bc_message{role = user, content = <<"describe this">>,
                      attachments = [{<<"image/jpeg">>, B64}]},
    Map = bc_provider_openrouter:message_to_map(Msg),
    ?assertEqual(<<"user">>, maps:get(role, Map)),
    Content = maps:get(content, Map),
    ?assert(is_list(Content)),
    ?assertEqual(2, length(Content)),
    [TextPart, ImagePart] = Content,
    ?assertEqual(<<"text">>, maps:get(<<"type">>, TextPart)),
    ?assertEqual(<<"describe this">>, maps:get(<<"text">>, TextPart)),
    ?assertEqual(<<"image_url">>, maps:get(<<"type">>, ImagePart)),
    ImageUrl = maps:get(<<"image_url">>, ImagePart),
    Url = maps:get(<<"url">>, ImageUrl),
    ExpectedPrefix = <<"data:image/jpeg;base64,">>,
    ?assertEqual(ExpectedPrefix, binary:part(Url, 0, byte_size(ExpectedPrefix))).

%% ---- system message ignores attachments ----

system_ignores_attachments_test() ->
    B64 = base64:encode(<<"fake image">>),
    Msg = #bc_message{role = system, content = <<"you are helpful">>,
                      attachments = [{<<"image/png">>, B64}]},
    Map = bc_provider_openrouter:message_to_map(Msg),
    ?assertEqual(<<"system">>, maps:get(role, Map)),
    %% Should be plain string, not content array
    ?assertEqual(<<"you are helpful">>, maps:get(content, Map)).

%% ---- assistant message ignores attachments ----

assistant_ignores_attachments_test() ->
    B64 = base64:encode(<<"fake">>),
    Msg = #bc_message{role = assistant, content = <<"I see">>,
                      attachments = [{<<"image/jpeg">>, B64}]},
    Map = bc_provider_openrouter:message_to_map(Msg),
    ?assertEqual(<<"I see">>, maps:get(content, Map)).

%% ---- multiple images ----

multiple_images_test() ->
    B64a = base64:encode(<<"img1">>),
    B64b = base64:encode(<<"img2">>),
    Msg = #bc_message{role = user, content = <<"compare these">>,
                      attachments = [{<<"image/jpeg">>, B64a},
                                     {<<"image/png">>, B64b}]},
    Map = bc_provider_openrouter:message_to_map(Msg),
    Content = maps:get(content, Map),
    ?assertEqual(3, length(Content)),
    [TextPart, Img1, Img2] = Content,
    ?assertEqual(<<"text">>, maps:get(<<"type">>, TextPart)),
    ?assertEqual(<<"image_url">>, maps:get(<<"type">>, Img1)),
    ?assertEqual(<<"image_url">>, maps:get(<<"type">>, Img2)),
    %% Verify different MIME types
    Url1 = maps:get(<<"url">>, maps:get(<<"image_url">>, Img1)),
    Url2 = maps:get(<<"url">>, maps:get(<<"image_url">>, Img2)),
    ?assertNotEqual(nomatch, binary:match(Url1, <<"image/jpeg">>)),
    ?assertNotEqual(nomatch, binary:match(Url2, <<"image/png">>)).

%% ---- undefined content with attachment ----

undefined_content_with_attachment_test() ->
    B64 = base64:encode(<<"img">>),
    Msg = #bc_message{role = user, content = undefined,
                      attachments = [{<<"image/jpeg">>, B64}]},
    Map = bc_provider_openrouter:message_to_map(Msg),
    Content = maps:get(content, Map),
    ?assert(is_list(Content)),
    [TextPart | _] = Content,
    ?assertEqual(<<"">>, maps:get(<<"text">>, TextPart)).
