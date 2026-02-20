%% @doc Telegram webhook handler — POST /webhook/telegram
-module(bc_webhook_telegram_h).

-export([init/2]).

init(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Update = jsx:decode(Body, [return_maps]),
    %% Forward to telegram channel for processing
    bc_channel_telegram:handle_webhook(Update),
    Req3 = cowboy_req:reply(200, #{}, <<>>, Req2),
    {ok, Req3, State}.
