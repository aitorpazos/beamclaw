%% @doc BeamClaw CLI entry point (escript).
%%
%% Usage: beamclaw [tui|start|stop|restart|remote_console|doctor|status|version|help]
%%
%% Built with: rebar3 escriptize
%% Output:     _build/default/bin/beamclaw
-module(beamclaw_cli).
-export([main/1]).

-define(VERSION, "0.1.0").
-define(DAEMON_NODE, 'beamclaw@localhost').
-define(GATEWAY_PORT, 8080).

%%--------------------------------------------------------------------
%% Entry point
%%--------------------------------------------------------------------

main([])                   -> cmd_tui();
main(["tui"            |_]) -> cmd_tui();
main(["start"          |_]) -> cmd_start();
main(["stop"           |_]) -> cmd_stop();
main(["restart"        |_]) -> cmd_restart();
main(["remote_console" |_]) -> cmd_remote_console();
main(["doctor"         |_]) -> cmd_doctor();
main(["status"         |_]) -> cmd_status();
main(["version"        |_]) -> cmd_version();
main(["help"           |_]) -> cmd_help();
main(["--help"         |_]) -> cmd_help();
main([Unknown          |_]) ->
    io:format(standard_error, "beamclaw: unknown command '~s'~n", [Unknown]),
    io:format(standard_error, "Run 'beamclaw help' for usage.~n", []),
    halt(1).

%%--------------------------------------------------------------------
%% Commands
%%--------------------------------------------------------------------

%% @doc Start interactive TUI chat. Exclusive stdin; blocks until EOF.
cmd_tui() ->
    apply_tui_config(),
    case application:ensure_all_started(beamclaw_gateway) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            io:format(standard_error,
                      "beamclaw: failed to start gateway: ~p~n", [Reason]),
            halt(1)
    end,
    case whereis(bc_channel_tui) of
        undefined ->
            io:format(standard_error,
                      "beamclaw: bc_channel_tui not registered after startup~n", []),
            halt(1);
        TuiPid ->
            Ref = monitor(process, TuiPid),
            receive
                {'DOWN', Ref, process, _, _} -> halt(0)
            end
    end.

%% @doc Start gateway as a background daemon (Erlang distribution IPC).
cmd_start() ->
    ensure_ctl_node(),
    case net_adm:ping(?DAEMON_NODE) of
        pong ->
            io:format("Gateway already running.~n"),
            halt(1);
        pang ->
            spawn_daemon()
    end.

%% @doc Stop a running daemon via OTP RPC.
cmd_stop() ->
    ensure_ctl_node(),
    case do_stop() of
        ok ->
            io:format("Gateway stopped.~n"),
            halt(0);
        not_running ->
            io:format("Gateway not running.~n"),
            halt(1);
        timeout ->
            io:format(standard_error,
                      "beamclaw: daemon did not stop within 10s~n", []),
            halt(1)
    end.

%% @doc Stop then start the daemon.
cmd_restart() ->
    ensure_ctl_node(),
    case do_stop() of
        ok          -> io:format("Gateway stopped.~n");
        not_running -> io:format("Gateway was not running.~n");
        timeout     ->
            io:format(standard_error,
                      "beamclaw: daemon did not stop within 10s~n", []),
            halt(1)
    end,
    spawn_daemon().

%% @doc Print the erl -remsh command to attach a live shell to the daemon.
cmd_remote_console() ->
    ensure_ctl_node(),
    case net_adm:ping(?DAEMON_NODE) of
        pang ->
            io:format("Gateway not running.~n"),
            halt(1);
        pong ->
            Cookie = atom_to_list(erlang:get_cookie()),
            io:format("Run: erl -remsh ~s -setcookie ~s~n",
                      [atom_to_list(?DAEMON_NODE), Cookie]),
            halt(0)
    end.

%% @doc Check environment and connectivity. Exits 0 if no failures, 1 otherwise.
cmd_doctor() ->
    Checks = [
        check_otp_version(),
        check_openrouter_key(),
        check_openai_key(),
        check_telegram_token(),
        check_epmd()
    ],
    AllChecks = case os:getenv("OPENROUTER_API_KEY") of
        false -> Checks;
        _     -> Checks ++ [check_openrouter_network()]
    end,
    HasFailure = lists:any(fun({Tag, _}) -> Tag =:= fail end, AllChecks),
    case HasFailure of
        true  -> halt(1);
        false -> halt(0)
    end.

%% @doc Ping the running gateway's /health endpoint.
cmd_status() ->
    application:ensure_all_started(inets),
    Port = case os:getenv("BEAMCLAW_PORT") of
        false -> ?GATEWAY_PORT;
        P     -> list_to_integer(P)
    end,
    Url = "http://localhost:" ++ integer_to_list(Port) ++ "/health",
    case httpc:request(get, {Url, []}, [{timeout, 3000}], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            io:format("Gateway running at localhost:~p — ~s~n", [Port, Body]),
            halt(0);
        {ok, {{_, Code, _}, _, Body}} ->
            io:format("Gateway returned HTTP ~p: ~s~n", [Code, Body]),
            halt(1);
        {error, {failed_connect, _}} ->
            io:format("Gateway not running at localhost:~p~n", [Port]),
            halt(1);
        {error, Reason} ->
            io:format(standard_error,
                      "beamclaw: status check failed: ~p~n", [Reason]),
            halt(1)
    end.

cmd_version() ->
    io:format("beamclaw ~s~n", [?VERSION]).

cmd_help() ->
    io:format(
        "Usage: beamclaw <command>~n~n"
        "Commands:~n"
        "  tui              Start interactive TUI chat (default)~n"
        "  start            Start gateway as background daemon~n"
        "  stop             Stop running daemon~n"
        "  restart          Stop then start daemon~n"
        "  remote_console   Print command to attach live Erlang shell~n"
        "  doctor           Check environment and connectivity~n"
        "  status           Ping running gateway HTTP health endpoint~n"
        "  version          Print version~n"
        "  help             Show this help~n~n"
        "Notes:~n"
        "  TUI: use Ctrl+D (EOF) to quit.~n"
        "  Ctrl+C shows the OTP break menu (type 'q' + Enter to exit).~n"
        "  Daemon IPC uses Erlang distribution; epmd must be available.~n~n"
        "Environment:~n"
        "  OPENROUTER_API_KEY  Required for LLM completions~n"
        "  OPENAI_API_KEY      Optional alternative provider~n"
        "  TELEGRAM_BOT_TOKEN  Optional Telegram channel~n"
        "  BEAMCLAW_PORT       Override gateway port (default: 8080)~n"
    ).

%%--------------------------------------------------------------------
%% Internal: TUI embedded config
%%--------------------------------------------------------------------

apply_tui_config() ->
    application:set_env(beamclaw_core, default_provider, openrouter,
                        [{persistent, true}]),
    application:set_env(beamclaw_core, providers, [
        {openrouter, #{api_key  => {env, "OPENROUTER_API_KEY"},
                       base_url => "https://openrouter.ai/api/v1",
                       model    => "moonshotai/kimi-k2.5"}},
        {openai,     #{api_key  => {env, "OPENAI_API_KEY"},
                       base_url => "https://api.openai.com/v1",
                       model    => "gpt-4o"}}
    ], [{persistent, true}]),
    application:set_env(beamclaw_core, agentic_loop,
        #{max_tool_iterations => 10,
          compaction_threshold => 50,
          compaction_target    => 20,
          stream_chunk_size    => 80},
        [{persistent, true}]),
    application:set_env(beamclaw_core, autonomy_level, supervised,
                        [{persistent, true}]),
    application:set_env(beamclaw_core, session_ttl_seconds, 3600,
                        [{persistent, true}]),
    application:set_env(beamclaw_mcp, servers, [], [{persistent, true}]),
    application:set_env(beamclaw_gateway, http, #{port => ?GATEWAY_PORT},
                        [{persistent, true}]),
    TuiChannel = {tui, #{enabled => true}},
    Channels = case os:getenv("TELEGRAM_BOT_TOKEN") of
        false -> [TuiChannel];
        _     -> [{telegram, #{token => {env, "TELEGRAM_BOT_TOKEN"},
                               mode  => long_poll}},
                  TuiChannel]
    end,
    application:set_env(beamclaw_gateway, channels, Channels,
                        [{persistent, true}]),
    application:set_env(beamclaw_memory, backend, ets, [{persistent, true}]),
    application:set_env(beamclaw_obs, backends, [{log, #{level => info}}],
                        [{persistent, true}]).

%%--------------------------------------------------------------------
%% Internal: daemon lifecycle
%%--------------------------------------------------------------------

%% Enable Erlang distribution with a unique ctl node name.
ensure_ctl_node() ->
    Id   = integer_to_list(erlang:unique_integer([positive])),
    Name = list_to_atom("beamclaw_ctl_" ++ Id ++ "@localhost"),
    case net_kernel:start([Name, shortnames]) of
        {ok, _}                       -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} ->
            io:format(standard_error,
                      "beamclaw: could not enable distribution: ~p~n"
                      "  (Is epmd running? Try: epmd -daemon)~n",
                      [Reason]),
            halt(1)
    end.

do_stop() ->
    case net_adm:ping(?DAEMON_NODE) of
        pang -> not_running;
        pong ->
            rpc:call(?DAEMON_NODE, init, stop, []),
            poll_node_down(?DAEMON_NODE, 20, 500)
    end.

spawn_daemon() ->
    ErlBin    = find_erl_bin(),
    EbinPaths = code:get_path(),
    PaArgs    = lists:flatmap(fun(P) -> ["-pa", P] end, EbinPaths),
    ConfigArgs = case filelib:is_file("config/sys.config") of
        true  -> ["-config", "config/sys"];
        false -> []
    end,
    Eval = "application:ensure_all_started(beamclaw_gateway),"
           "receive after infinity -> ok end.",
    DaemonArgs = ["-noshell", "-sname", "beamclaw"]
                 ++ PaArgs
                 ++ ConfigArgs
                 ++ ["-eval", Eval],
    erlang:open_port({'spawn_executable', ErlBin},
                     [{args, DaemonArgs}, detached]),
    case poll_node_up(?DAEMON_NODE, 10, 500) of
        ok ->
            io:format("Gateway started.~n"),
            halt(0);
        timeout ->
            io:format(standard_error,
                      "beamclaw: daemon did not respond within 5s~n"
                      "  (Check OTP logger output for startup errors.)~n",
                      []),
            halt(1)
    end.

find_erl_bin() ->
    Candidate = filename:join([code:root_dir(), "bin", "erl"]),
    case filelib:is_file(Candidate) of
        true  -> Candidate;
        false ->
            case os:find_executable("erl") of
                false ->
                    io:format(standard_error,
                              "beamclaw: cannot find 'erl' binary~n", []),
                    halt(1);
                Path -> Path
            end
    end.

poll_node_up(_Node, 0, _Interval) -> timeout;
poll_node_up(Node, Retries, Interval) ->
    case net_adm:ping(Node) of
        pong -> ok;
        pang ->
            timer:sleep(Interval),
            poll_node_up(Node, Retries - 1, Interval)
    end.

poll_node_down(_Node, 0, _Interval) -> timeout;
poll_node_down(Node, Retries, Interval) ->
    case net_adm:ping(Node) of
        pang -> ok;
        pong ->
            timer:sleep(Interval),
            poll_node_down(Node, Retries - 1, Interval)
    end.

%%--------------------------------------------------------------------
%% Internal: doctor checks
%%--------------------------------------------------------------------

check_otp_version() ->
    Release = erlang:system_info(otp_release),
    Major   = list_to_integer(hd(string:tokens(Release, "."))),
    case Major >= 28 of
        true  -> print_check(ok,   "OTP " ++ Release);
        false -> print_check(fail, "OTP " ++ Release ++ " (need >= 28)")
    end.

check_openrouter_key() ->
    case os:getenv("OPENROUTER_API_KEY") of
        false -> print_check(fail, "OPENROUTER_API_KEY not set (required)");
        _     -> print_check(ok,   "OPENROUTER_API_KEY set")
    end.

check_openai_key() ->
    case os:getenv("OPENAI_API_KEY") of
        false -> print_check(info, "OPENAI_API_KEY not set (optional)");
        _     -> print_check(ok,   "OPENAI_API_KEY set")
    end.

check_telegram_token() ->
    case os:getenv("TELEGRAM_BOT_TOKEN") of
        false -> print_check(info, "TELEGRAM_BOT_TOKEN not set (Telegram disabled)");
        _     -> print_check(ok,   "TELEGRAM_BOT_TOKEN set")
    end.

check_epmd() ->
    case os:find_executable("epmd") of
        false ->
            print_check(warn, "epmd not found on PATH "
                              "(needed for start/stop/remote_console)");
        _ ->
            print_check(ok, "epmd found")
    end.

check_openrouter_network() ->
    application:ensure_all_started(inets),
    Url = "https://openrouter.ai/api/v1/models",
    case httpc:request(get, {Url, []}, [{timeout, 5000}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            print_check(ok,   "OpenRouter API reachable (200 OK)");
        {ok, {{_, Code, _}, _, _}} ->
            print_check(warn, "OpenRouter API returned HTTP "
                              ++ integer_to_list(Code));
        {error, Reason} ->
            print_check(fail, "OpenRouter API unreachable: "
                              ++ format_reason(Reason))
    end.

print_check(ok,   Msg) -> io:format("[ok]   ~s~n", [Msg]), {ok,   Msg};
print_check(warn, Msg) -> io:format("[warn] ~s~n", [Msg]), {warn, Msg};
print_check(fail, Msg) -> io:format("[fail] ~s~n", [Msg]), {fail, Msg};
print_check(info, Msg) -> io:format("[info] ~s~n", [Msg]), {info, Msg}.

format_reason({failed_connect, _}) ->
    "connection refused or DNS failure";
format_reason(Reason) ->
    lists:flatten(io_lib:format("~p", [Reason])).
