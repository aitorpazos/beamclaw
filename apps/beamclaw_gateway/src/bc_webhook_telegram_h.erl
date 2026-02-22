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

-module(bc_webhook_telegram_h).
-moduledoc "Telegram webhook handler — POST /webhook/telegram".

-export([init/2]).

init(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Update = jsx:decode(Body, [return_maps]),
    %% Forward to telegram channel for processing
    bc_channel_telegram:handle_webhook(Update),
    Req3 = cowboy_req:reply(200, #{}, <<>>, Req2),
    {ok, Req3, State}.
