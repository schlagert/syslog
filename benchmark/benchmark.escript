#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

%%%=============================================================================
%%% Copyright 2013-2017, Tobias Schlager <schlagert@github.com>
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc
%%% A benchmarking escript to test different logging frameworks. Simulates a
%%% burst of large log statements.
%%%
%%% The test spams messages from a configurable number of processes over a
%%% configurable amount of time. The number of messages sent will be reported
%%% as well as the total duration which includes the time needed to deliver all
%%% messages and the maximum needed processes memory.
%%%
%%% Its probably a good idea to 'nice -19' the benchmark, at least if you're
%%% trying the `sasl_syslog' case.
%%% @end
%%%=============================================================================

-mode(compile).
-compile([export_all]).

-behaviour(gen_event).
-define(TEST_PORT, 31337).

%% ~ 14*60=840bytes (roughly) of delicious garbage
-define(LARGE_FMT,
        "~s~w~p"
        "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
        "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
        "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        "gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg"
        "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
        "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"
        "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj"
        "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
        "llllllllllllllllllllllllllllllllllllllllllllllllllllllllllll"
        "mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm"
        "nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn").
-define(LARGE_ARGS,
        [
         %% ~s adds nothing
         "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
         %% ~w converts to [98,98,...] => 18*3+2+2=58Chars
         "bbbbbbbbbbbbbbbbbbb",
         %% ~p adds two '"'
         "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
        ]).

%% ~ 5*8=40bytes (roughly) of delicious garbage
-define(SMALL_FMT,
        "~s~w~p"
        "dddddddd"
        "eeeeeeee").
-define(SMALL_ARGS,
        [
         %% ~s adds nothing
         "aaaaaaaa",
         %% ~w converts to [98,98] => 1*3+2+2=7Chars
         "bb",
         %% ~p adds two '"'
         "cccccc"
        ]).

-define(BENCHMARK_DAT(T, N), "benchmark-" ++ T ++ "-" ++ N ++ ".dat").

-define(LAGER,       "https://github.com/basho/lager.git").
-define(LOG4ERL,     "https://github.com/ahmednawras/log4erl.git").
-define(SASL_SYSLOG, "https://github.com/travelping/sasl_syslog.git").

%%%=============================================================================
%%% API
%%%=============================================================================

main([]) ->
    io:format(
      "Usage:~n~n"
      "  ~s all|lager|log4erl|sasl_syslog|syslog [MessageType] [Processes] [Millis]~n~n"
      "      MessageType - Use large or small messages (default large)"
      "      Processes   - Number of processes used (default 1)~n"
      "      Millis      - Duration of spamming in millis (default 2000)~n~n",
      [escript:script_name()]);
main([App]) ->
    main([App, "large"]);
main([App, MessageType]) ->
    main([App, MessageType, "1"]);
main([App, MessageType, NumberOfProcesses]) ->
    main([App, MessageType, NumberOfProcesses, "2000"]);
main(["all", MessageType, NumberOfProcesses, MilliSeconds]) ->
    {NumProcs, Millis, Socket} = setup(NumberOfProcesses, MilliSeconds),
    {File, Fmt, Args} = get_fmt_args(MessageType, NumberOfProcesses),
    io:format("  Profile:                     ~s~n", [MessageType]),
    lager(File, NumProcs, Millis, Socket, Fmt, Args),
    log4erl(File, NumProcs, Millis, Socket, Fmt, Args),
    syslog(File, NumProcs, Millis, Socket, Fmt, Args),
    ok = gen_udp:close(Socket);
main([App, MessageType, NumberOfProcesses, MilliSeconds]) ->
    {NumProcs, Millis, Socket} = setup(NumberOfProcesses, MilliSeconds),
    {File, Fmt, Args} = get_fmt_args(MessageType, NumberOfProcesses),
    io:format("  Profile:                     ~s~n", [MessageType]),
    ?MODULE:(list_to_atom(App))(File, NumProcs, Millis, Socket, Fmt, Args),
    ok = gen_udp:close(Socket).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup(NumberOfProcesses, MilliSeconds) ->
    NumProcs = list_to_integer(NumberOfProcesses),
    Millis = list_to_integer(MilliSeconds),
    io:format(
      "Benchmark~n"
      "---------~n"
      "  Process(es):                 ~p~n"
      "  Duration (ms):               ~p~n",
      [NumProcs, Millis]),
    ok = error_logger:tty(false),
    ok = application:load(sasl),
    ok = application:set_env(sasl, sasl_error_logger, false),
    {ok, _} = application:ensure_all_started(sasl),
    {ok, Socket} = gen_udp:open(?TEST_PORT, [binary]),
    {NumProcs, Millis, Socket}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup_app(App, BaseDir, GitURL) ->
    io:format("  Application:                 ~s~n", [App]),
    ProjectDir = filename:join([BaseDir, atom_to_list(App)]),
    if is_list(GitURL) -> ok = retrieve(GitURL, ProjectDir);
       true            -> ok
    end,
    ok = build(ProjectDir),
    Paths = filelib:wildcard(filename:join([ProjectDir, "**", "ebin"])),
    [true = code:add_path(P) || P <- Paths],
    case application:load(App) of
        ok                             -> {ok, Paths};
        {error, {already_loaded, App}} -> {ok, Paths};
        Error                          -> Error
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
teardown_app(StartedApps, AddedPaths) ->
    [application:stop(App) || App <- StartedApps],
    [true = code:del_path(Path) || Path <- AddedPaths],
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
lager(File, NumProcs, Millis, Socket, Fmt, Args) ->
    {ok, Paths} = setup_app(lager, pwd(), ?LAGER),
    ok = application:set_env(lager, error_logger_redirect, true),
    ok = application:set_env(lager, handlers, [{?MODULE, []}]),
    {ok, Started} = application:ensure_all_started(lager),
    timer:sleep(1000),
    LogFun = fun() -> ok = lager:log(info, self(), Fmt, Args) end,
    ok = run_app(File, lager, LogFun, NumProcs, Millis, Socket),
    teardown_app(Started, Paths).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log4erl(File, NumProcs, Millis, Socket, Fmt, Args) ->
    {ok, Paths} = setup_app(log4erl, pwd(), ?LOG4ERL),
    {ok, Started} = application:ensure_all_started(log4erl),
    Appender = {info, daemon, "localhost", ?TEST_PORT, "%b %D %t localhost %l"},
    {ok, _} = log4erl:add_syslog_appender(?MODULE, Appender),
    LogFun = fun() -> ok = log4erl:log(info, Fmt, Args) end,
    ok = run_app(File, log4erl, LogFun, NumProcs, Millis, Socket),
    teardown_app(Started, Paths).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
sasl_syslog(File, NumProcs, Millis, Socket, Fmt, Args) ->
    {ok, Paths} = setup_app(sasl_syslog, pwd(), ?SASL_SYSLOG),
    ok = application:set_env(sasl_syslog, enabled, true),
    ok = application:set_env(sasl_syslog, remote_host, "localhost"),
    ok = application:set_env(sasl_syslog, remote_port, ?TEST_PORT),
    {ok, Started} = application:ensure_all_started(sasl_syslog),
    LogFun = fun() -> ok = error_logger:info_msg(Fmt, Args) end,
    ok = run_app(File, sasl_syslog, LogFun, NumProcs, Millis, Socket),
    teardown_app(Started, Paths).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
syslog(File, NumProcs, Millis, Socket, Fmt, Args) ->
    BaseDir = filename:join([pwd(), "..", ".."]),
    {ok, Paths} = setup_app(syslog, BaseDir, undefined),
    ok = application:set_env(syslog, dest_host, "localhost"),
    ok = application:set_env(syslog, dest_port, ?TEST_PORT),
    {ok, Started} = application:ensure_all_started(syslog),
    LogFun = fun() -> ok = syslog:info_msg(Fmt, Args) end,
    ok = run_app(File, syslog, LogFun, NumProcs, Millis, Socket),
    teardown_app(Started, Paths).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
run_app(File, App, Fun, NumProcs, Millis, Socket) ->
    ok = empty_mailbox(Socket),
    StartMillis = current_millis(),
    generate_messages(Fun, NumProcs, Millis),
    {NumSent, NumReceived, Memory, StopMillis} = process_messages(Socket, NumProcs),
    Duration = StopMillis - StartMillis,
    report_statistics(File, App, NumSent, NumReceived, Memory, Duration, Millis, NumProcs).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
report_statistics(File, App, NumSent, NumReceived, Memory, Duration, Millis, NumProcs) ->
    Throughput = NumReceived * 1000 div Duration,
    io:format(
      "  Total Messages Sent:         ~w~n"
      "  Total Messages Received:     ~w~n"
      "  Total Messages Dropped:      ~w~n"
      "  Total Duration (ms):         ~w~n"
      "  Speed (msgs/s/proc):         ~w~n"
      "  Message Throughput (msgs/s): ~w~n"
      "  Peak Memory Used (MB):       ~w~n",
      [NumSent,
       NumReceived,
       NumSent - NumReceived,
       Duration,
       (NumSent * 1000 div Millis) div NumProcs,
       Throughput,
       Memory / 1048576]),
    file:write_file(
      File,
      io_lib:format(
        "~-20s~-10B~-10B~-10B~-10.3f~n",
        [App, NumSent, Throughput, Duration, Memory / 1048576]),
      [append]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
empty_mailbox(Socket) ->
    receive
        {udp, _, _, _, _} ->
            empty_mailbox(Socket);
        {udp_closed, Socket} ->
            exit({error, udp_closed});
        {udp_closed, _} ->
            empty_mailbox(Socket)
    after 100 ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
generate_messages(Fun, NumProcs, Millis) ->
    [spawn_monitor(
       fun() ->
               generate_loop(Fun, Millis)
       end)
     || _ <- lists:seq(1, NumProcs)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
generate_loop(Fun, Millis) ->
    generate_loop(Fun, current_millis(Millis), 0).
generate_loop(Fun, EndMillis, NumMessages) ->
    case EndMillis - current_millis() of
        LeftMillis when LeftMillis > 0 ->
            {Time, ok} = timer:tc(Fun),
            if Time > 5000000 ->
                    io:format(standard_error,
                              "Sender blocked for ~wms!!!~n",
                              [Time div 1000]);
               true ->
                    ok
            end,
            generate_loop(Fun, EndMillis, NumMessages + 1);
        _ ->
            exit({ok, NumMessages})
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
process_messages(Socket, NumProcs) ->
    process_messages(Socket, NumProcs, {0, 0}, 0, 0).
process_messages(_Sock, 0, {MaxMemory, _}, NumSent, NumSent) ->
    {NumSent, NumSent, MaxMemory, current_millis()};
process_messages(Sock, Left, Memory, NumSent, NumReceived) ->
    NewMemory = {MaxMemory, _} = memory_snapshot(Memory),
    receive
        {'DOWN', _, process, _Pid, {ok, Sent}} ->
            process_messages(Sock, Left - 1, NewMemory, NumSent + Sent, NumReceived);
        {'DOWN', _, process, _, _} ->
            process_messages(Sock, Left - 1, NewMemory, NumSent, NumReceived);
        {udp, Sock, _, _, _} ->
            process_messages(Sock, Left, NewMemory, NumSent, NumReceived + 1);
        {udp_closed, Sock} ->
            exit({error, udp_closed});
        _ ->
            process_messages(Sock, Left, NewMemory, NumSent, NumReceived)
    after 5000 ->
            {NumSent, NumReceived, MaxMemory, current_millis() - 5000}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
memory_snapshot({Max, 0})       -> {erlang:max(erlang:memory(processes), Max), 1};
memory_snapshot({Max, Counter}) -> {Max, (Counter + 1) rem 100}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
retrieve(Url, Dir)        -> retrieve(filelib:is_dir(Dir), Url, Dir).
retrieve(true, _, Dir)    -> os_cmd("git pull", Dir);
retrieve(false, Url, Dir) -> os_cmd("git clone " ++ Url ++ " " ++ Dir, pwd()).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
build(Dir) -> os_cmd("rebar get-deps compile", Dir).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
os_cmd(Cmd, Dir) when is_list(Cmd) ->
    Options = [{cd, Dir}, {line, 1024}, stderr_to_stdout, use_stdio, exit_status],
    wait_for_os_cmd(erlang:open_port({spawn, Cmd}, Options), Cmd).
wait_for_os_cmd(Port, Cmd) ->
    receive
        {Port, {exit_status, 0}} ->
            ok;
        {'EXIT', Port, Status} ->
            Reason = io_lib:format("~s failed with ~p", [Cmd, Status]),
            {error, lists:flatten(Reason)};
        {Port, {exit_status, Status}} ->
            Reason = io_lib:format("~s failed with ~p", [Cmd, Status]),
            {error, lists:flatten(Reason)};
        {Port, {data, {Flag, _}}} when Flag =:= eol orelse Flag =:= noeol->
            wait_for_os_cmd(Port, Cmd);
        _ ->
            wait_for_os_cmd(Port, Cmd)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
pwd() -> {ok, Dir} = file:get_cwd(), Dir.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_fmt_args("large", NumberOfProcesses) ->
    {?BENCHMARK_DAT("large", NumberOfProcesses), ?LARGE_FMT, ?LARGE_ARGS};
get_fmt_args(_, NumberOfProcesses) ->
    {?BENCHMARK_DAT("small", NumberOfProcesses), ?SMALL_FMT, ?SMALL_ARGS}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
current_millis() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs * 1000 + MicroSecs div 1000.
current_millis(OffsetMillis) ->
    current_millis() + OffsetMillis.

%%%=============================================================================
%%% gen_event callbacks
%%%=============================================================================

-record(state, {socket = inet:socket(), log_level :: term()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    {ok, S} = gen_udp:open(0, [binary]),
    L = lager_util:config_to_mask(info),
    {ok, #state{socket = S, log_level = L}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(get_loglevel, State = #state{log_level = L}) -> {ok, L, State};
handle_call(_Request, State)                             -> {ok, ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event({log, Msg}, State = #state{socket = S, log_level = L}) ->
    case lager_util:is_loggable(Msg, L, ?MODULE) of
        true ->
            IOList = lager_default_formatter:format(Msg, []),
            ok = gen_udp:send(S, "localhost", ?TEST_PORT, IOList);
        false ->
            ok
    end,
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(_Info, State) -> {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Arg, #state{socket = Socket}) -> gen_udp:close(Socket).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.
