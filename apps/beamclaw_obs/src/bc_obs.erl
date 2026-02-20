%% @doc Observer behaviour and public emit API.
%%
%% Usage: bc_obs:emit(tool_call_start, #{tool_name => Name, session_id => SId}).
%%
%% emit/2 is a non-blocking cast; it never creates backpressure on callers.
-module(bc_obs).

-export([emit/2]).

%% Observer backend behaviour.
-callback init(Config :: map()) -> {ok, State :: term()} | {error, term()}.
-callback handle_event(Event :: term(), State :: term()) -> {ok, State :: term()}.
-callback terminate(Reason :: term(), State :: term()) -> ok.

%% @doc Emit an observability event. Non-blocking.
-spec emit(Type :: atom(), Data :: map()) -> ok.
emit(Type, Data) ->
    Event = #{type => Type, data => Data, ts => erlang:system_time(millisecond)},
    gen_server:cast(bc_obs_manager, {emit, Event}).
