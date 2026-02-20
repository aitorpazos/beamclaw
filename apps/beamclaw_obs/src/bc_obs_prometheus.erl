%% @doc Prometheus observability backend stub. Implements the bc_observer behaviour.
-module(bc_obs_prometheus).
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
    {ok, #{}}.

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

%% bc_obs callback — increment counters/histograms per event type
handle_event(#{type := _Type, data := _Data}, State) ->
    %% TODO: instrument with prometheus counters/histograms
    {ok, State}.
