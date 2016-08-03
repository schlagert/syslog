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

-define(RFC3164_DATE, "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) (\\s|\\d)\\d").
-define(RFC3164_TIME, "\\d\\d:\\d\\d:\\d\\d").

-define(RFC5424_DATE, "\\d\\d\\d\\d-\\d\\d-\\d\\d").
-define(RFC5424_TIME, "T\\d\\d:\\d\\d:\\d\\d\\.\\d\\d\\d\\d\\d\\d").
-define(RFC5424_ZONE, "(Z|(\\+|-)\\d\\d:\\d\\d)").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

rfc3164_test_() ->
    {timeout,
     5,
     [
      {"RFC 3164 over UDP", fun() -> rfc3164(udp) end},
      {"RFC 3164 over TCP", fun() -> rfc3164(tcp) end}
     ]}.

rfc3164(Transport) ->
    Sockets = setup(rfc3164, Transport, debug),

    Proc = pid_to_list(self()),
    Date = ?RFC3164_DATE ++ " " ++ ?RFC3164_TIME,

    ?assertEqual(ok, syslog:info_msg("hello world")),
    Re1 = "<30>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Sockets, Re1)),

    ?assertEqual(ok, syslog:msg(critical, "hello world", [])),
    Re2 = "<26>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Sockets, Re2)),

    ?assertEqual(ok, syslog:error_msg("hello ~s", ["world"])),
    Re3 = "<27>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Sockets, Re3)),

    ?assertEqual(ok, error_logger:error_msg("hello ~s", ["world"])),
    ?assertEqual(ok, expect(Sockets, Re3)),

    ?assertEqual(ok, syslog:error_msg("~nhello~n~s~n", ["world"])),
    Re4 = "<27>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello",
    ?assertEqual(ok, expect(Sockets, Re4)),
    Re5 = "<27>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - world",
    ?assertEqual(ok, expect(Sockets, Re5)),

    ?assertEqual(ok, syslog:msg(crash, "hello world", [])),
    Re6 = "<131>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Sockets, Re6)),

    teardown(Sockets).

rfc5424_test_() ->
    {timeout,
     5,
     [
      {"RFC 5424 over UDP", fun() -> rfc5424(udp) end},
      {"RFC 5424 over TCP", fun() -> rfc5424(tcp) end}
     ]}.

rfc5424(Transport) ->
    Sockets = setup(rfc5424, Transport, debug),

    Proc = pid_to_list(self()),
    Date = ?RFC5424_DATE ++ ?RFC5424_TIME ++ ?RFC5424_ZONE,

    ?assertEqual(ok, syslog:info_msg("hello world")),
    Re1 = "<30>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Sockets, Re1)),

    ?assertEqual(ok, syslog:msg(critical, "hello world", [])),
    Re2 = "<26>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Sockets, Re2)),

    ?assertEqual(ok, syslog:error_msg("hello ~s", ["world"])),
    Re3 = "<27>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Sockets, Re3)),

    ?assertEqual(ok, error_logger:error_msg("hello ~s", ["world"])),
    ?assertEqual(ok, expect(Sockets, Re3)),

    ?assertEqual(ok, syslog:error_msg("~nhello~n~s~n", ["world"])),
    Re4 = "<27>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello",
    ?assertEqual(ok, expect(Sockets, Re4)),
    Re5 = "<27>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - world",
    ?assertEqual(ok, expect(Sockets, Re5)),

    ?assertEqual(ok, syslog:msg(crash, "hello world", [])),
    Re6 = "<131>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(Sockets, Re6)),

    teardown(Sockets).

log_level_test_() ->
    {timeout,
     5,
     fun() ->
             Sockets = setup(rfc5424, udp, notice),

             Proc = pid_to_list(self()),
             Date = ?RFC5424_DATE ++ ?RFC5424_TIME ++ ?RFC5424_ZONE,

             ?assertEqual(ok, syslog:debug_msg("hello world")),
             ?assertEqual(ok, syslog:info_msg("hello world")),
             ?assertEqual(timeout, read(Sockets)),

             ?assertEqual(ok, syslog:set_log_level(debug)),

             ?assertEqual(ok, syslog:debug_msg("hello world")),
             Re1 = "<31>1 " ++ Date ++ " .+ \\w+ \\d+ "
                 ++ Proc ++ " - hello world",
             ?assertMatch({match, _}, re:run(read(Sockets), Re1)),

             teardown(Sockets)
     end}.

error_logger_test_() ->
    {timeout,
     5,
     fun() ->
             Sockets = setup(rfc3164, udp, debug, 20),

             %% test message queue limit and drop percentage

             erlang:suspend_process(whereis(error_logger)),

             Send = fun(I) -> error_logger:info_msg("Message ~w", [I]) end,
             ok = lists:foreach(Send, lists:seq(1, 30)),

             erlang:resume_process(whereis(error_logger)),

             Receive = fun(_) -> ?assert(is_list(read(Sockets))) end,
             ok = lists:foreach(Receive, lists:seq(1, 18)),
             ?assertEqual(timeout, read(Sockets)),

             %% test (extra) crash_report

             Pid = proc_lib:spawn(fun() -> exit(test_reason) end),
             Date = ?RFC3164_DATE ++ " " ++ ?RFC3164_TIME,
             Proc = pid_to_list(Pid),

             Re = "<27>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++
                 Proc ++ " - exited with {exit,test_reason}",
             ?assertEqual(ok, wait_for(Sockets, Re)),

             teardown(Sockets)
     end}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup(Protocol, Transport, LogLevel) ->
    setup(Protocol, Transport, LogLevel, infinity).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup(Protocol, udp, LogLevel, Limit) ->
    ok = setup_apps({Protocol, udp}, LogLevel, Limit),
    {ok, Socket} = gen_udp:open(?TEST_PORT, [list]),
    ok = empty_mailbox(),
    [Socket];
setup(Protocol, tcp, LogLevel, Limit) ->
    {ok, Server} = gen_tcp:listen(?TEST_PORT, [list, {reuseaddr, true}]),
    ok = setup_apps({Protocol, tcp}, LogLevel, Limit),
    {ok, Socket} = gen_tcp:accept(Server),
    ok = empty_mailbox(),
    [Socket, Server].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup_apps(Protocol, LogLevel, Limit) ->
    ok = application:start(sasl),
    ok = load(syslog),
    ok = application:set_env(syslog, dest_port, ?TEST_PORT),
    ok = application:set_env(syslog, protocol, Protocol),
    ok = application:set_env(syslog, crash_facility, local0),
    ok = application:set_env(syslog, log_level, LogLevel),
    ok = application:set_env(syslog, msg_queue_limit, Limit),
    ok = application:set_env(syslog, no_progress, true),
    application:start(syslog).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
teardown(Sockets) ->
    application:stop(syslog),
    application:stop(sasl),
    application:unset_env(syslog, dest_port),
    application:unset_env(syslog, protocol),
    application:unset_env(syslog, crash_facility),
    application:unset_env(syslog, log_level),
    application:unset_env(syslog, msg_queue_limit),
    application:unset_env(syslog, drop_percentage),
    [inet:close(Socket) || Socket <- Sockets],
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
load(App) -> load(App, application:load(App)).
load(_, ok) -> ok;
load(App, {error, {already_loaded, App}}) -> ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
wait_for(Sockets, Pattern) ->
    case expect(Sockets, Pattern) of
        {nomatch, _, _} -> wait_for(Sockets, Pattern);
        Other           -> Other
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
expect(Sockets, Pattern) ->
    case read(Sockets) of
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
read(Sockets = [Socket | _]) ->
    Acc = proplists:get_value(acc, get(), []),
    case read_message(Acc) of
        {ok, {Message, Rest}} ->
            put(acc, Rest),
            Message;
        error ->
            receive
                {udp, Socket, _, _, Message} ->
                    Message;
                {tcp, Socket, Data} ->
                    put(acc, Acc ++ Data),
                    read(Sockets)
            after
                500 -> timeout
            end
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read_message(Data) ->
    case get_message_size(Data) of
        {ok, Size, Rest} when length(Rest) >= Size ->
            {ok, lists:split(Size, Rest)};
        _ ->
            error
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_message_size(Data) ->
    get_message_size(Data, []).
get_message_size([], _Acc) ->
    continue;
get_message_size([$\s | Rest], Acc) ->
    {ok, list_to_integer(lists:reverse(Acc)), Rest};
get_message_size([C | Rest], Acc) when C >= $0, C =< $9->
    get_message_size(Rest, [C | Acc]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
empty_mailbox() -> receive _ -> empty_mailbox() after 100 -> ok end.
