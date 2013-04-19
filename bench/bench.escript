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
%%% @end
%%%=============================================================================

-mode(compile).

-define(TEST_PORT, 31337).
-define(FMT,       "a message ~p").
-define(ARGS,      ["containing a lengthy string message to print, not funny"]).

-define(URL,
        [
         {"lager",       "https://github.com/basho/lager_syslog.git"},
         {"log4erl",     "https://github.com/ahmednawras/log4erl.git"},
         {"sasl_syslog", "https://github.com/travelping/sasl_syslog.git"},
         {"syslog",      "https://github.com/schlagert/syslog.git"}
        ]).

%%%=============================================================================
%%% API
%%%=============================================================================

main([App]) ->
    main([App, "1"]);
main([App, NumberOfProcesses]) ->
    main([App, NumberOfProcesses, "2000"]);
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
    {ok, LogFun} = start(App),
    {ok, Socket} = gen_udp:open(?TEST_PORT, [binary, {reuseaddr, true}]),
    try
        io:format(
          "Benchmark~n"
          "---------~n"
          "  Application:         ~s~n"
          "  Process(es):         ~p~n"
          "  Duration:            ~pms~n",
          [App, NumProcs, Millis]),
        ok = run(LogFun, NumProcs, Millis, Socket)
    after
        gen_udp:close(Socket)
    end,
    true = code:del_path(filename:join([ProjectDir, "ebin"])),
    ok = stop(App).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start("lager") ->
    {error, not_yet_implemented};
start("log4erl") ->
    ok = start_app(log4erl),
    Appender = {info, daemon, "localhost", ?TEST_PORT, "%b %D %t localhost %l"},
    {ok, _} = log4erl:add_syslog_appender(?MODULE, Appender),
    {ok, fun() -> log4erl:log(info, ?FMT, ?ARGS) end};
start("sasl_syslog") ->
    ok = load_app(sasl_syslog),
    ok = application:set_env(sasl_syslog, enabled, true),
    ok = application:set_env(sasl_syslog, remote_host, "localhost"),
    ok = application:set_env(sasl_syslog, remote_port, ?TEST_PORT),
    ok = start_app(sasl_syslog),
    {ok, fun() -> ok = error_logger:info_msg(?FMT, ?ARGS) end};
start("syslog") ->
    ok = application:set_env(syslog, dest_host, "localhost"),
    ok = application:set_env(syslog, dest_port, ?TEST_PORT),
    ok = start_app(syslog),
    {ok, fun() -> ok = syslog:info_msg(?FMT, ?ARGS) end}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
run(Fun, NumProcs, Millis, Socket) ->
    ok = empty_socket(Socket),
    StartMillis = current_millis(),
    generate(Fun, NumProcs, Millis),
    NumSent = finalize(Socket, NumProcs),
    StopMillis = current_millis(),
    io:format(
      "  Total messages sent: ~p~n"
      "  Messages per second: ~p~n"
      "  Total duration:      ~pms~n",
      [NumSent, NumSent * 1000 div Millis, StopMillis - StartMillis]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
stop(App) when is_list(App) -> stop(list_to_atom(App));
stop(App) when is_atom(App) -> application:stop(App), ok.

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
    after 20 ->
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
    finalize(Socket, NumProcs, 0, 0).
finalize(_Socket, 0, NumSent, NumSent) ->
    NumSent;
finalize(Socket, Left, NumSent, NumReceived) ->
    receive
        {'DOWN', _, process, _, {ok, Sent}} ->
            finalize(Socket, Left - 1, NumSent + Sent, NumReceived);
        {'DOWN', _, process, _, _} ->
            finalize(Socket, Left - 1, NumSent, NumReceived);
        {udp, Socket, _, _, _} ->
            finalize(Socket, Left, NumSent, NumReceived + 1);
        {udp_closed, Socket} ->
            exit({error, udp_closed});
        _ ->
            finalize(Socket, Left, NumSent, NumReceived)
    end.

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
