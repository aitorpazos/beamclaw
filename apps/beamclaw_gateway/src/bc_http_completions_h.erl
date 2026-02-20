%% @doc OpenAI-compatible chat completions handler — POST /v1/chat/completions
%%
%% Accepts OpenAI-format request bodies and dispatches to a BeamClaw session.
%% Supports SSE streaming (stream: true).
-module(bc_http_completions_h).

-export([init/2]).

init(Req, State) ->
    ClientIp = peer_ip(Req),
    case bc_rate_limiter:check(ClientIp, <<"/v1/chat/completions">>) of
        {error, rate_limited} ->
            Req2 = cowboy_req:reply(429,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{error => <<"rate limited">>}), Req),
            {ok, Req2, State};
        ok ->
            handle_completion(Req, State)
    end.

handle_completion(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    _Decoded   = jsx:decode(Body, [return_maps]),
    %% TODO: route to bc_session and stream SSE or return JSON response
    RespBody  = jsx:encode(#{id      => <<"chatcmpl-stub">>,
                              object  => <<"chat.completion">>,
                              choices => [#{message => #{role => <<"assistant">>,
                                                         content => <<"[stub]">>}}]}),
    Req3 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        RespBody, Req2),
    {ok, Req3, State}.

peer_ip(Req) ->
    case cowboy_req:peer(Req) of
        {Ip, _Port} -> Ip;
        _            -> unknown
    end.

