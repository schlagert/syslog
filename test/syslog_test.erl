%%%=============================================================================
%%% Copyright 2016, Tobias Schlager <schlagert@github.com>
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
%%%=============================================================================

-module(syslog_test).

-include_lib("eunit/include/eunit.hrl").

-include("syslog.hrl").

-define(TEST_PORT, 31337).
-define(TIMEOUT,   100).

-define(RFC3164_DATE, "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) (\\s|\\d)\\d").
-define(RFC3164_TIME, "\\d\\d:\\d\\d:\\d\\d").

-define(RFC5424_DATE, "\\d\\d\\d\\d-\\d\\d-\\d\\d").
-define(RFC5424_TIME, "T\\d\\d:\\d\\d:\\d\\d\\.\\d\\d\\d\\d\\d\\d").
-define(RFC5424_ZONE, "(Z|(\\+|-)\\d\\d:\\d\\d)").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

rfc3164_test() ->
    {ok, Socket} = setup(rfc3164, debug),

    Proc = pid_to_list(self()),
    Date = ?RFC3164_DATE ++ " " ++ ?RFC3164_TIME,

    ?assertEqual(ok, syslog:info_msg("hello world")),
    Re1 = "<30>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Socket, Re1)),

    ?assertEqual(ok, syslog:msg(critical, "hello world", [])),
    Re2 = "<26>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Socket, Re2)),

    ?assertEqual(ok, syslog:error_msg("hello ~s", ["world"])),
    Re3 = "<27>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Socket, Re3)),

    ?assertEqual(ok, error_logger:error_msg("hello ~s", ["world"])),
    ?assertEqual(ok, expect(Socket, Re3)),

    ?assertEqual(ok, syslog:error_msg("~nhello~n~s~n", ["world"])),
    Re4 = "<27>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello",
    ?assertEqual(ok, expect(Socket, Re4)),
    Re5 = "<27>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - world",
    ?assertEqual(ok, expect(Socket, Re5)),

    ?assertEqual(ok, syslog:msg(crash, "hello world", [])),
    Re6 = "<131>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Socket, Re6)),

    teardown(Socket).

rfc5424_test() ->
    {ok, Socket} = setup(rfc5424, debug),

    Proc = pid_to_list(self()),
    Date = ?RFC5424_DATE ++ ?RFC5424_TIME ++ ?RFC5424_ZONE,

    ?assertEqual(ok, syslog:info_msg("hello world")),
    Re1 = "<30>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Socket, Re1)),

    ?assertEqual(ok, syslog:msg(critical, "hello world", [])),
    Re2 = "<26>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Socket, Re2)),

    ?assertEqual(ok, syslog:error_msg("hello ~s", ["world"])),
    Re3 = "<27>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Socket, Re3)),

    ?assertEqual(ok, error_logger:error_msg("hello ~s", ["world"])),
    ?assertEqual(ok, expect(Socket, Re3)),

    ?assertEqual(ok, syslog:error_msg("~nhello~n~s~n", ["world"])),
    Re4 = "<27>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello",
    ?assertEqual(ok, expect(Socket, Re4)),
    Re5 = "<27>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - world",
    ?assertEqual(ok, expect(Socket, Re5)),

    ?assertEqual(ok, syslog:msg(crash, "hello world", [])),
    Re6 = "<131>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Socket, Re6)),

    teardown(Socket).

log_level_test_() ->
    {timeout,
     5,
     fun() ->
             {ok, Socket} = setup(rfc5424, notice),

             Proc = pid_to_list(self()),
             Date = ?RFC5424_DATE ++ ?RFC5424_TIME ++ ?RFC5424_ZONE,

             ?assertEqual(ok, syslog:debug_msg("hello world")),
             ?assertEqual(ok, syslog:info_msg("hello world")),
             ?assertEqual(timeout, read(Socket)),

             ?assertEqual(ok, syslog:set_log_level(debug)),

             ?assertEqual(ok, syslog:debug_msg("hello world")),
             Re1 = "<31>1 " ++ Date ++ " .+ \\w+ \\d+ "
                 ++ Proc ++ " - hello world",
             ?assertMatch({match, _}, re:run(read(Socket), Re1)),

             teardown(Socket)
     end}.

error_logger_test_() ->
    {timeout,
     5,
     fun() ->
             {ok, Socket} = setup(rfc3164, debug, 20),

             %% test message queue limit and drop percentage

             erlang:suspend_process(whereis(error_logger)),

             Send = fun(I) -> error_logger:info_msg("Message ~w", [I]) end,
             ok = lists:foreach(Send, lists:seq(1, 30)),

             erlang:resume_process(whereis(error_logger)),

             Receive = fun(_) -> ?assert(is_list(read(Socket))) end,
             ok = lists:foreach(Receive, lists:seq(1, 18)),
             ?assertEqual(timeout, read(Socket)),

             %% test (extra) crash_report

             Pid = proc_lib:spawn(fun() -> exit(test_reason) end),
             Date = ?RFC3164_DATE ++ " " ++ ?RFC3164_TIME,
             Proc = pid_to_list(Pid),

             Re = "<27>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++
                 Proc ++ " - exited with {exit,test_reason}",
             ?assertEqual(ok, wait_for(Socket, Re)),

             teardown(Socket)
     end}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup(Protocol, LogLevel) -> setup(Protocol, LogLevel, infinity).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup(Protocol, LogLevel, Limit) ->
    ?assertEqual(ok, application:start(sasl)),
    AppFile = filename:join(["..", "src", "syslog.app.src"]),
    {ok, [AppSpec]} = file:consult(AppFile),
    ?assertEqual(ok, load(AppSpec)),
    ?assertEqual(ok, application:set_env(syslog, dest_port, ?TEST_PORT)),
    ?assertEqual(ok, application:set_env(syslog, protocol, Protocol)),
    ?assertEqual(ok, application:set_env(syslog, crash_facility, local0)),
    ?assertEqual(ok, application:set_env(syslog, log_level, LogLevel)),
    ?assertEqual(ok, application:set_env(syslog, msg_queue_limit, Limit)),
    ?assertEqual(ok, application:set_env(syslog, no_progress, true)),
    ?assertEqual(ok, application:start(syslog)),
    ?assertEqual(ok, empty_mailbox()),
    gen_udp:open(?TEST_PORT, [list]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
teardown(Socket) ->
    application:stop(syslog),
    application:stop(sasl),
    application:unset_env(syslog, dest_port),
    application:unset_env(syslog, protocol),
    application:unset_env(syslog, crash_facility),
    application:unset_env(syslog, log_level),
    application:unset_env(syslog, msg_queue_limit),
    application:unset_env(syslog, drop_percentage),
    gen_udp:close(Socket).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
load(App) -> load(App, application:load(App)).
load(_, ok) -> ok;
load(App, {error, {already_loaded, App}}) -> ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
wait_for(Socket, Pattern) ->
    case expect(Socket, Pattern) of
        {nomatch, _, _} -> wait_for(Socket, Pattern);
        Other           -> Other
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
expect(Socket, Pattern) ->
    case read(Socket) of
        L when is_list(L) ->
            case re:run(L, Pattern) of
                {match, _} -> ok;
                _          -> {nomatch, L, Pattern}
            end;
        Other ->
            Other
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read(Socket) ->
    receive
        {udp, Socket, _, _, L} -> L
    after
        500 -> timeout
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
empty_mailbox() -> receive _ -> empty_mailbox() after ?TIMEOUT -> ok end.
