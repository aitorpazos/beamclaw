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
