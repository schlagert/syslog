#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

%%%=============================================================================
%%% Copyright 2013, Tobias Schlager <schlagert@github.com>
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
%%% A benchmarking escript to test different logging frameworks.
%%%
%%% The test spams messages from a configurable number of processes over a
%%% configurable amount of time. The number of messages sent will be reported
%%% as well as the total duration which includes the time needed to deliver all
%%% messages and the maximum needed processes memory.
%%% @end
%%%=============================================================================

-mode(compile).

-define(TEST_PORT, 31337).
-define(FMT, "~p").
-define(ARGS,
	[%% 840bytes of garbage
	 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	 "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
	 "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
	 "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
	 "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
	 "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
	 "gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg"
	 "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
	 "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"
	 "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj"
	 "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
	 "llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll"
	 ]).

-define(URL,
        [
         {"lager",       "https://github.com/basho/lager.git"},
         {"log4erl",     "https://github.com/schlagert/log4erl.git"},
         {"sasl_syslog", "https://github.com/travelping/sasl_syslog.git"},
         {"syslog",      "https://github.com/schlagert/syslog.git"}
        ]).

%%%=============================================================================
%%% API
%%%=============================================================================

main([]) ->
    io:format(
      "Usage:~n~n"
      "  ~s all|lager|log4erl|sasl_syslog|syslog [Processes] [Millis]~n~n"
      "      Processes - Number of processes used (default 1)~n"
      "      Millis    - Duration of spamming in millis (default 2000)~n~n",
      [escript:script_name()]);
main([App]) ->
    main([App, "1"]);
main([App, NumberOfProcesses]) ->
    main([App, NumberOfProcesses, "2000"]);
main(["all", NumberOfProcesses, MilliSeconds]) ->
    main(["lager", NumberOfProcesses, MilliSeconds]),
    main(["log4erl", NumberOfProcesses, MilliSeconds]),
    main(["syslog", NumberOfProcesses, MilliSeconds]),
    %% This is likely to fail or at least take damn long
    main(["sasl_syslog", NumberOfProcesses, MilliSeconds]);
main([App, NumberOfProcesses, MilliSeconds]) ->
    Millis = list_to_integer(MilliSeconds),
    NumProcs = list_to_integer(NumberOfProcesses),
    ProjectDir = filename:join([pwd(), App]),
    ok = error_logger:tty(false),
    ok = load_app(sasl),
    ok = application:set_env(sasl, sasl_error_logger, false),
    ok = start_app(sasl),
    ok = retrieve(url(App), ProjectDir),
    ok = build(ProjectDir),
    true = code:add_path(filename:join([ProjectDir, "ebin"])),
    {ok, LogFun} = start(App, ProjectDir),
    {ok, Socket} = gen_udp:open(?TEST_PORT, [binary, {reuseaddr, true}]),
    try
        io:format(
          "Benchmark~n"
          "---------~n"
          "  Application:         ~s~n"
          "  Process(es):         ~p~n"
          "  Duration:            ~pms~n",
          [App, NumProcs, Millis]),
        ok = run(App, LogFun, NumProcs, Millis, Socket)
    after
        gen_udp:close(Socket)
    end,
    true = code:del_path(filename:join([ProjectDir, "ebin"])),
    ok = stop(App, ProjectDir).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start("lager", ProjectDir) ->
    true = code:add_path(filename:join([ProjectDir, "deps", "goldrush", "ebin"])),
    ok = load_app(lager),
    ok = application:set_env(lager, error_logger_redirect, true),
    ok = application:set_env(lager, handlers, [{lager_console_backend, info}]),
    ok = lager:start(),
    true = park_process(user),
    true = register(user, self()),
    {ok, fun() -> ok = lager:log(info, self(), ?FMT, ?ARGS) end};
start("log4erl", _) ->
    ok = start_app(log4erl),
    Appender = {info, daemon, "localhost", ?TEST_PORT, "%b %D %t localhost %l"},
    {ok, _} = log4erl:add_syslog_appender(?MODULE, Appender),
    {ok, fun() -> ok = log4erl:log(info, ?FMT, ?ARGS) end};
start("sasl_syslog", _) ->
    ok = load_app(sasl_syslog),
    ok = application:set_env(sasl_syslog, enabled, true),
    ok = application:set_env(sasl_syslog, remote_host, "localhost"),
    ok = application:set_env(sasl_syslog, remote_port, ?TEST_PORT),
    ok = start_app(sasl_syslog),
    {ok, fun() -> ok = error_logger:info_msg(?FMT, ?ARGS) end};
start("syslog", _) ->
    ok = application:set_env(syslog, dest_host, "localhost"),
    ok = application:set_env(syslog, dest_port, ?TEST_PORT),
    ok = start_app(syslog),
    {ok, fun() -> ok = syslog:info_msg(?FMT, ?ARGS) end}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
run(App, Fun, NumProcs, Millis, Socket) ->
    ok = empty_socket(Socket),
    StartMillis = current_millis(),
%%    eprof:start(),
%%    Ps = [lager_sup, lager_event, lager_crash_log, lager_handler_watcher_sup],
%%    Ps = [syslog_logger, syslog],
%%    profiling = eprof:start_profiling(Ps),
    generate(Fun, NumProcs, Millis),
    {NumSent, Memory} = finalize(Socket, NumProcs),
%%    eprof:stop_profiling(),
%%    eprof:log("bench.prof"),
%%    eprof:analyze(procs),
    report(App, NumSent, Memory, current_millis() - StartMillis).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
report(App, NumSent, Memory, Duration) ->
    NumSentPerSecond = NumSent * 1000 div Duration,
    io:format(
      "  Total messages sent: ~p~n"
      "  Messages per second: ~p~n"
      "  Total duration:      ~pms~n"
      "  Max. memory used :   ~pMB~n",
      [NumSent, NumSentPerSecond, Duration, Memory / 1048576]),
    file:write_file(
      "bench.dat",
      io_lib:format(
        "~-20s~-10B~-10B~-10B~-10.3f~n",
        [App, NumSent, NumSentPerSecond, Duration, Memory / 1048576]),
      [append]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
stop("lager", ProjectDir) ->
    restore_process(user),
    application:stop(goldrush),
    true = code:del_path(filename:join([ProjectDir, "deps", "goldrush", "ebin"])),
    stop(lager, ProjectDir);
stop(App, ProjectDir) when is_list(App) ->
    stop(list_to_atom(App), ProjectDir);
stop(App, _ProjectDir) when is_atom(App) ->
    application:stop(App),
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
url(App) -> proplists:get_value(App, ?URL).

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
empty_socket(Socket) ->
    receive
        {udp, _, _, _, _} ->
            empty_socket(Socket);
        {udp_closed, Socket} ->
            exit({error, udp_closed});
        {udp_closed, _} ->
            empty_socket(Socket)
    after 100 ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
generate(Fun, NumProcs, Millis) ->
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
            ok = Fun(),
            generate_loop(Fun, EndMillis, NumMessages + 1);
        _ ->
            exit({ok, NumMessages})
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
finalize(Socket, NumProcs) ->
    finalize(Socket, NumProcs, {0, 0}, 0, 0).
finalize(_Socket, 0, {MaxMemory, _}, NumSent, NumSent) ->
    {NumSent, MaxMemory};
finalize(Socket, Left, Memory, NumSent, NumReceived) ->
    NewMemory = memory_snapshot(Memory),
    receive
        {'DOWN', _, process, _, {ok, Sent}} ->
            finalize(Socket, Left - 1, NewMemory, NumSent + Sent, NumReceived);
        {'DOWN', _, process, _, _} ->
            finalize(Socket, Left - 1, NewMemory, NumSent, NumReceived);
        {udp, Socket, _, _, _} ->
            finalize(Socket, Left, NewMemory, NumSent, NumReceived + 1);
        {udp_closed, Socket} ->
            exit({error, udp_closed});
        {io_request, F, A, {put_chars, unicode, M}} ->
            %% everyone else must do the socket IO, console loggers should
            %% at least fake this here
            F ! {io_reply, A, gen_udp:send(Socket, "localhost", ?TEST_PORT, M)},
            finalize(Socket, Left, NewMemory, NumSent, NumReceived);
        _ ->
            finalize(Socket, Left, NewMemory, NumSent, NumReceived)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
memory_snapshot({Max, 0})       -> {erlang:max(erlang:memory(processes), Max), 1};
memory_snapshot({Max, Counter}) -> {Max, (Counter + 1) rem 100}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start_app(App) when is_list(App) ->
    start_app(list_to_atom(App));
start_app(App) when is_atom(App) ->
    case application:start(App) of
        ok                              -> ok;
        {error, {already_started, App}} -> ok;
        Error                           -> Error
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
load_app(App) when is_list(App) ->
    load_app(list_to_atom(App));
load_app(App) when is_atom(App) ->
    case application:load(App) of
        ok                             -> ok;
        {error, {already_loaded, App}} -> ok;
        Error                          -> Error
    end.

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
        {Port, {data, {Flag, Str}}} when Flag =:= eol orelse Flag =:= noeol->
            io:format("~s~n", [Str]),
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
park_process(Name)         -> park_process(Name, whereis(Name)).
park_process(_, undefined) -> true;
park_process(Name, Pid)    -> unregister(Name), register(parked, Pid).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
restore_process(Name) ->
    case whereis(parked) of
        Pid when is_pid(Pid) ->
            unregister(parked),
            catch unregister(Name),
            register(Name, Pid);
        _ ->
            true
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
current_millis() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs * 1000 + MicroSecs div 1000.
current_millis(OffsetMillis) ->
    current_millis() + OffsetMillis.


%% lager() ->
%%     Handlers = [{lager_syslog_backend, ["beam", local0, info]}],
%%     ok = application:set_env(lager, handlers, Handlers),
%%     ok = application:start(lager),
%%     ok = application:start(lager_syslog),
%%     %% do it
%%     lager:log(info, self(), "hello world by lager from pid ~p", [self()]),
%%     application:stop(lager_syslog),
%%     application:stop(lager).
