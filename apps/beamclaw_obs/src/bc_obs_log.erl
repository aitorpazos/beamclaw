%% @doc Log observability backend. Implements the bc_observer behaviour.
-module(bc_obs_log).
-behaviour(gen_server).
%% Implements bc_observer backend callbacks (handle_event/2).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% bc_obs callbacks
-export([handle_event/2]).

-define(PG_SCOPE, bc_obs_backends).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    pg:join(?PG_SCOPE, backends, self()),
    Level = application:get_env(beamclaw_obs, log_level, info),
    {ok, #{level => Level}}.

handle_cast({event, Event}, State) ->
    {ok, NewState} = handle_event(Event, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% bc_obs callback
handle_event(#{type := Type, data := Data, ts := Ts} = _Event, State) ->
    logger:info("[obs] ~p ts=~p data=~p", [Type, Ts, Data]),
    {ok, State}.
