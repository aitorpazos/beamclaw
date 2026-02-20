%% @doc Dynamic supervisor for per-session supervisors.
-module(bc_sessions_sup).
-behaviour(supervisor).

-export([start_link/0, start_session/1, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_session(Config :: map()) -> {ok, pid()} | {error, term()}.
start_session(Config) ->
    supervisor:start_child(?MODULE, [Config]).

init([]) ->
    SupFlags = #{strategy  => simple_one_for_one,
                 intensity => 10,
                 period    => 60},
    ChildSpec = #{id       => bc_session_sup,
                  start    => {bc_session_sup, start_link, []},
                  restart  => temporary,
                  shutdown => infinity,
                  type     => supervisor,
                  modules  => [bc_session_sup]},
    {ok, {SupFlags, [ChildSpec]}}.
