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

-module(bc_telegram_photo_tests).
-moduledoc "Tests for bc_telegram_photo — photo extraction and encoding.".

-include_lib("eunit/include/eunit.hrl").

%% ---- extract_photo ----

extract_photo_present_test() ->
    Msg = #{<<"photo">> => [
        #{<<"file_id">> => <<"small_id">>, <<"width">> => 90},
        #{<<"file_id">> => <<"medium_id">>, <<"width">> => 320},
        #{<<"file_id">> => <<"large_id">>, <<"width">> => 800}
    ]},
    ?assertEqual({ok, <<"large_id">>}, bc_telegram_photo:extract_photo(Msg)).

extract_photo_absent_test() ->
    Msg = #{<<"text">> => <<"hello">>},
    ?assertEqual(no_photo, bc_telegram_photo:extract_photo(Msg)).

extract_photo_empty_array_test() ->
    Msg = #{<<"photo">> => []},
    ?assertEqual(no_photo, bc_telegram_photo:extract_photo(Msg)).

%% ---- extract_caption ----

extract_caption_present_test() ->
    Msg = #{<<"caption">> => <<"Look at this!">>},
    ?assertEqual(<<"Look at this!">>, bc_telegram_photo:extract_caption(Msg)).

extract_caption_absent_test() ->
    Msg = #{<<"text">> => <<"hello">>},
    ?assertEqual(undefined, bc_telegram_photo:extract_caption(Msg)).

extract_caption_empty_test() ->
    Msg = #{<<"caption">> => <<>>},
    ?assertEqual(undefined, bc_telegram_photo:extract_caption(Msg)).

%% ---- validate_size ----

validate_size_ok_test() ->
    Bin = crypto:strong_rand_bytes(1000),
    ?assertEqual(ok, bc_telegram_photo:validate_size(Bin, 5242880)).

validate_size_too_large_test() ->
    %% Create a binary just over the limit
    Bin = binary:copy(<<0>>, 5242881),
    ?assertEqual({error, too_large}, bc_telegram_photo:validate_size(Bin, 5242880)).

validate_size_exact_limit_test() ->
    Bin = binary:copy(<<0>>, 5242880),
    ?assertEqual(ok, bc_telegram_photo:validate_size(Bin, 5242880)).

%% ---- to_attachment ----

to_attachment_test() ->
    ImageBin = <<"fake image data">>,
    {Mime, B64} = bc_telegram_photo:to_attachment(<<"image/jpeg">>, ImageBin),
    ?assertEqual(<<"image/jpeg">>, Mime),
    ?assertEqual(base64:encode(ImageBin), B64).

%% ---- mime_from_path ----

mime_from_path_jpg_test() ->
    ?assertEqual(<<"image/jpeg">>, bc_telegram_photo:mime_from_path(<<"photos/img.jpg">>)).

mime_from_path_jpeg_test() ->
    ?assertEqual(<<"image/jpeg">>, bc_telegram_photo:mime_from_path(<<"photo.jpeg">>)).

mime_from_path_png_test() ->
    ?assertEqual(<<"image/png">>, bc_telegram_photo:mime_from_path(<<"image.png">>)).

mime_from_path_webp_test() ->
    ?assertEqual(<<"image/webp">>, bc_telegram_photo:mime_from_path(<<"sticker.webp">>)).

mime_from_path_gif_test() ->
    ?assertEqual(<<"image/gif">>, bc_telegram_photo:mime_from_path(<<"anim.gif">>)).

mime_from_path_unknown_test() ->
    ?assertEqual(<<"image/jpeg">>, bc_telegram_photo:mime_from_path(<<"file.bmp">>)).
