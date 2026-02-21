%% @doc Sliding-window rate limiter — per client IP, ETS-backed, pruned every 60s.
-module(bc_rate_limiter).
-behaviour(gen_server).

-export([start_link/0, check/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TAB,          bc_rate_limiter).
-define(PRUNE_MS,     60000).
-define(DEFAULT_LIMIT, 60).    %% requests per window
-define(WINDOW_MS,    60000).  %% 60-second window

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Check if ClientId is within rate limit. Returns ok | {error, rate_limited}.
-spec check(ClientId :: term(), Path :: binary()) -> ok | {error, rate_limited}.
check(ClientId, _Path) ->
    Now  = erlang:monotonic_time(millisecond),
    Key  = {client, ClientId},
    case ets:lookup(?TAB, Key) of
        [{Key, Timestamps}] ->
            Window = [T || T <- Timestamps, T > Now - ?WINDOW_MS],
            case length(Window) >= ?DEFAULT_LIMIT of
                true  -> {error, rate_limited};
                false ->
                    ets:insert(?TAB, {Key, [Now | Window]}),
                    ok
            end;
        [] ->
            ets:insert(?TAB, {Key, [Now]}),
            ok
    end.

init([]) ->
    _ = ets:new(?TAB, [set, named_table, public, {write_concurrency, true}]),
    erlang:send_after(?PRUNE_MS, self(), prune),
    {ok, #{}}.

handle_info(prune, State) ->
    Now = erlang:monotonic_time(millisecond),
    Cutoff = Now - ?WINDOW_MS,
    ets:foldl(fun({Key, Timestamps}, _) ->
        case [T || T <- Timestamps, T > Cutoff] of
            []      -> ets:delete(?TAB, Key);
            Pruned  -> ets:insert(?TAB, {Key, Pruned})
        end
    end, ok, ?TAB),
    erlang:send_after(?PRUNE_MS, self(), prune),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) -> {reply, {error, unknown}, State}.
handle_cast(_Msg, State)        -> {noreply, State}.
terminate(_Reason, _State)      -> ok.
code_change(_OldVsn, State, _) -> {ok, State}.
