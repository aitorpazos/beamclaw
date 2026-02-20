%% @doc WebSocket handler — GET /ws
-module(bc_ws_h).

-export([init/2, websocket_init/1, websocket_handle/2,
         websocket_info/2, terminate/3]).

init(Req, _State) ->
    ClientIp = cowboy_req:peer(Req),
    {cowboy_websocket, Req, #{client_ip => ClientIp, session_id => undefined}}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    Decoded = jsx:decode(Msg, [return_maps]),
    handle_ws_message(Decoded, State);
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({send, Content}, State) ->
    {reply, {text, jsx:encode(#{type => <<"message">>, content => Content})}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.

handle_ws_message(#{<<"type">> := <<"message">>, <<"content">> := Content} = _Msg, State) ->
    %% TODO: dispatch to session
    Echo = jsx:encode(#{type => <<"message">>, content => Content}),
    {reply, {text, Echo}, State};
handle_ws_message(_Msg, State) ->
    {ok, State}.
