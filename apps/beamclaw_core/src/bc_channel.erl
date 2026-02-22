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

-module(bc_channel).
-moduledoc """
Channel behaviour — messaging platform abstraction.

Channels receive messages from users and send responses back.
Streaming uses update_draft/finalize_draft for progressive output.
""".

-include_lib("beamclaw_core/include/bc_types.hrl").

-callback init(Config :: map()) -> {ok, State :: term()} | {error, term()}.

%% Begin listening for incoming messages (long-poll, webhook setup, etc.).
-callback listen(State :: term()) -> {ok, NewState :: term()} | {error, term()}.

%% Send a completed message to the user.
-callback send(SessionId :: binary(), Message :: #bc_message{}, State :: term()) ->
    {ok, NewState :: term()} | {error, term()}.

%% Send a typing indicator.
-callback send_typing(SessionId :: binary(), State :: term()) -> ok.

%% Update an in-progress streamed draft (for progressive output, ~80-char chunks).
-callback update_draft(SessionId :: binary(), DraftId :: term(),
                       Content :: binary(), State :: term()) ->
    {ok, NewState :: term()} | {error, term()}.

%% Finalize a streamed draft (turn it into a permanent message).
-callback finalize_draft(SessionId :: binary(), DraftId :: term(),
                         State :: term()) ->
    {ok, NewState :: term()} | {error, term()}.

-callback terminate(Reason :: term(), State :: term()) -> ok.
