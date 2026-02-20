%% @doc Channel behaviour — messaging platform abstraction.
%%
%% Channels receive messages from users and send responses back.
%% Streaming uses update_draft/finalize_draft for progressive output.
-module(bc_channel).

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
