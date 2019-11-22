%%%=============================================================================
%%% Copyright 2016-2018, Tobias Schlager <schlagert@github.com>
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

-module(syslog_tests).

-include_lib("eunit/include/eunit.hrl").

-include("syslog.hrl").

-define(TEST_PORT, 31337).

-define(RFC3164_DATE, "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) (\\s|\\d)\\d").
-define(RFC3164_TIME, "\\d\\d:\\d\\d:\\d\\d").

-define(RFC5424_DATE, "\\d\\d\\d\\d-\\d\\d-\\d\\d").
-define(RFC5424_TIME, "T\\d\\d:\\d\\d:\\d\\d\\.\\d\\d\\d\\d\\d\\d").
-define(RFC5424_ZONE, "(Z|(\\+|-)\\d\\d:\\d\\d)").

-record(state, {
          started :: [atom()],
          devices :: [{gen_udp, gen_udp:socket()} |
                      {gen_tcp, gen_tcp:socket()} |
                      {file, file:io_device(), string()}]}).

%%%=============================================================================
%%% TESTS
%%%=============================================================================

rfc3164_test_() ->
    {timeout,
     5,
     [
      {"RFC 3164 over UDP", fun() -> rfc3164(udp) end},
      {"RFC 3164 over TCP", fun() -> rfc3164(tcp) end},
      {"RFC 3164 to tmp.txt", fun() -> rfc3164("tmp.txt") end}
     ]}.

rfc3164(Transport) ->
    State = setup(rfc3164, Transport, debug),

    Proc = pid_to_list(self()),
    Date = ?RFC3164_DATE ++ " " ++ ?RFC3164_TIME,

    StructuredData = [
                      {'myid@1234', [{a, 1}, {b, 2.5}]},
                      {<<"myid@2345">>, [{c, "1"}, {d, <<"2">>}]}
                     ],

    ?assertEqual(ok, syslog:msg(notice, self(), StructuredData, "", [])),

    ?assertEqual(ok, syslog:info_msg("hello world")),
    Re1 = "<30>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(State, Re1, "started application syslog")),

    ?assertEqual(ok, syslog:msg(critical, "hello world", [])),
    Re2 = "<26>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(State, Re2)),

    ?assertEqual(ok, syslog:error_msg("hello ~s", ["world"])),
    Re3 = "<27>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(State, Re3)),

    ?assertEqual(ok, error_logger:error_msg("hello ~s", ["world"])),
    ?assertEqual(ok, expect(State, Re3)),

    ?assertEqual(ok, syslog:error_msg("~nhello~n~s~n", ["world"])),
    Re4 = "<27>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello",
    ?assertEqual(ok, expect(State, Re4)),
    Re5 = "<27>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - world",
    ?assertEqual(ok, expect(State, Re5)),

    ?assertEqual(ok, syslog:msg(crash, "hello world", [])),
    Re6 = "<131>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(State, Re6)),

    teardown(State).

rfc5424_test_() ->
    {timeout,
     5,
     [
      {"RFC 5424 over UDP", fun() -> rfc5424(udp) end},
      {"RFC 5424 over TCP", fun() -> rfc5424(tcp) end},
      {"RFC 5424 to tmp.txt", fun() -> rfc5424("tmp.txt") end}
     ]}.

rfc5424(Transport) ->
    State = setup(rfc5424, Transport, debug),

    Proc = pid_to_list(self()),
    Date = ?RFC5424_DATE ++ ?RFC5424_TIME ++ ?RFC5424_ZONE,

    StructuredData = [
                      {'myid@1234', [{a, 1}, {b, 2.5}]},
                      {<<"myid@2345">>, [{c, "1"}, {d, <<"2">>}]}
                     ],

    ?assertEqual(ok, syslog:info_msg("hello world")),
    Re1 = "<30>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(State, Re1, "started application syslog")),

    ?assertEqual(ok, syslog:msg(critical, "hello world", [])),
    Re2 = "<26>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(State, Re2)),

    ?assertEqual(ok, syslog:error_msg("hello ~s", ["world"])),
    Re3 = "<27>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(State, Re3)),

    ?assertEqual(ok, error_logger:error_msg("hello ~s", ["world"])),
    ?assertEqual(ok, expect(State, Re3)),

    ?assertEqual(ok, syslog:error_msg("~nhello~n~s~n", ["world"])),
    Re4 = "<27>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello",
    ?assertEqual(ok, expect(State, Re4)),
    Re5 = "<27>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - world",
    ?assertEqual(ok, expect(State, Re5)),

    ?assertEqual(ok, syslog:msg(crash, "hello world", [])),
    Re6 = "<131>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " - hello world",
    ?assertEqual(ok, expect(State, Re6)),

    ?assertEqual(ok, syslog:msg(notice, self(), StructuredData, "", [])),
    Re7 = "<29>1 " ++ Date ++ " .+ \\w+ \\d+ " ++ Proc ++ " "
        "\\[myid@1234 a=\"1\" b=\"2.5\"\\]"
        "\\[myid@2345 c=\"1\" d=\"2\"\\]",
    ?assertEqual(ok, expect(State, Re7)),

    ?assertEqual(ok, syslog:msg(notice, self(), StructuredData, "info", [])),
    Re8 = Re7 ++ " info",
    ?assertEqual(ok, expect(State, Re8)),

    teardown(State).

log_level_test_() ->
    {timeout,
     5,
     fun() ->
             State = setup(rfc5424, udp, notice),

             Proc = pid_to_list(self()),
             Date = ?RFC5424_DATE ++ ?RFC5424_TIME ++ ?RFC5424_ZONE,

             ?assertEqual(ok, syslog:debug_msg("hello world")),
             ?assertEqual(ok, syslog:info_msg("hello world")),
             ?assertEqual(timeout, read(State)),

             ?assertEqual(ok, syslog:set_log_level(debug)),

             ?assertEqual(ok, syslog:debug_msg("hello world")),
             Re1 = "<31>1 " ++ Date ++ " .+ \\w+ \\d+ "
                 ++ Proc ++ " - hello world",
             ?assertMatch({match, _}, re:run(read(State), Re1)),

             teardown(State)
     end}.

error_logger_test_() ->
    case syslog_lib:has_error_logger() of
        true ->
            {timeout,
             5,
             fun() ->
                     State = setup(rfc3164, udp, debug, 20, true),

                     %% test message queue limit and drop percentage

                     erlang:suspend_process(whereis(error_logger)),

                     Send = fun(I) -> error_logger:info_msg("Message ~w", [I]) end,
                     ok = lists:foreach(Send, lists:seq(1, 30)),

                     erlang:resume_process(whereis(error_logger)),

                     Receive = fun(_) -> ?assert(is_list(read(State))) end,
                     ok = lists:foreach(Receive, lists:seq(1, 18)),
                     ?assertEqual(timeout, read(State)),

                     teardown(State)
             end};
        false ->
            fun() -> ok end
    end.

unicode_test_() ->
    {timeout,
     5,
     fun() ->
             State = setup(rfc5424, udp, debug),

             Proc = pid_to_list(self()),
             Date = ?RFC5424_DATE ++ ?RFC5424_TIME ++ ?RFC5424_ZONE,

             {ok, Compiled} = re:compile(<<"<31>1 "/utf8,
                                           (list_to_binary(Date))/binary,
                                           " .+ \\w+ \\d+ "/utf8,
                                           (list_to_binary(Proc))/binary,
                                           " - äöü"/utf8>>, []),
             ?assertEqual(ok, syslog:debug_msg("~ts", [<<"äöü"/utf8>>])),
             ?assertMatch({match, _}, re:run(read(State), Compiled)),

             teardown(State)
     end}.

lager_integration_test_() ->
    {timeout,
     5,
     fun() ->
             State = setup(rfc5424, udp, debug, infinity, false),
             case ensure_loaded(lager) of
                 ok ->
                     ok = application:set_env(lager, error_logger_redirect, false),
                     Handler = {syslog_lager_backend,
                                [
                                 debug,
                                 {"sd_id", [pid]},
                                 {lager_default_formatter, [message]},
                                 true
                                ]},
                     ok = application:set_env(lager, handlers, [Handler]),
                     {ok, Started} = application:ensure_all_started(lager),

                     Metadata = [{pid, self()}, {application, syslog}],
                     ok = lager:log(notice, Metadata, "hello ~s", ["world"]),

                     Proc = pid_to_list(self()),
                     Date = ?RFC5424_DATE ++ ?RFC5424_TIME ++ ?RFC5424_ZONE,
                     Re = "<29>1 " ++ Date ++ " .+ syslog \\d+ " ++ Proc
                         ++ " \\[sd_id pid=\"" ++ Proc ++ "\"\\] hello world",
                     {ok, Compiled} = re:compile(Re, []),
                     ?assertMatch({match, _}, re:run(read(State), Compiled)),

                     file:delete("log/crash.log"),
                     file:del_dir("log"),

                     ok = lists:foreach(fun application:stop/1, Started),
                     ok = lists:foreach(fun application:unload/1, Started);
                 _ ->
                     ok %% no lager available, e.g. rebar2 build?
             end,
             teardown(State)
     end}.

extra_crash_report_test_() ->
    {timeout,
     5,
     fun() ->
             State = setup(rfc3164, tcp, debug),

             %% test (extra) crash_report

             Pid = proc_lib:spawn(fun() -> exit(test_reason) end),
             Date = ?RFC3164_DATE ++ " " ++ ?RFC3164_TIME,
             Proc = pid_to_list(Pid),

             Re = "<27>" ++ Date ++ " .+ \\w+\\[\\d+\\] " ++
                 Proc ++ " - exited with {exit,test_reason}",
             ?assertEqual(ok, wait_for(State, Re)),

             teardown(State)
     end}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup(Protocol, Transport, LogLevel) ->
    setup(Protocol, Transport, LogLevel, infinity, true).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup(Protocol, udp, LogLevel, Limit, Integrate) ->
    {ok, Started} = setup_apps({Protocol, udp}, LogLevel, Limit, Integrate),
    {ok, Socket} = gen_udp:open(?TEST_PORT, [list]),
    ok = empty_mailbox(),
    #state{started = Started, devices = [{gen_udp, Socket}]};
setup(Protocol, tcp, LogLevel, Limit, Integrate) ->
    {ok, Server} = gen_tcp:listen(?TEST_PORT, [list, {reuseaddr, true}]),
    {ok, Started} = setup_apps({Protocol, tcp}, LogLevel, Limit, Integrate),
    {ok, Socket} = gen_tcp:accept(Server),
    ok = empty_mailbox(),
    #state{started = Started, devices = [{gen_tcp, Socket}, {gen_tcp, Server}]};
setup(Protocol, File, LogLevel, Limit, Integrate) ->
    {ok, Started} = setup_apps({Protocol, File}, LogLevel, Limit, Integrate),
    ok = file:write_file(File, <<>>),
    {ok, IoDevice} = file:open(File, [read]),
    ok = empty_mailbox(),
    #state{started = Started, devices = [{file, IoDevice, File}]}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup_apps(Protocol, LogLevel, Limit, Integrate) ->
    ok = ensure_loaded(syslog),
    ok = application:set_env(syslog, dest_port, ?TEST_PORT),
    ok = application:set_env(syslog, protocol, Protocol),
    ok = application:set_env(syslog, crash_facility, local0),
    ok = application:set_env(syslog, log_level, LogLevel),
    ok = application:set_env(syslog, msg_queue_limit, Limit),
    ok = application:set_env(syslog, no_progress, true),
    case Integrate of
        false ->
            ok = application:set_env(syslog, logger, []),
            ok = application:set_env(syslog, syslog_error_logger, false);
        _ ->
            ok
    end,
    application:ensure_all_started(syslog).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
teardown(#state{started = Started, devices = Devices}) ->
    lists:foreach(fun application:stop/1, Started),
    application:unload(syslog),
    erase(acc),
    lists:foreach(
      fun({file, IoDevice, File}) ->
              file:close(IoDevice),
              file:delete(File);
         ({_, Socket}) ->
              inet:close(Socket)
      end, Devices).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
ensure_loaded(App) ->
    case application:load(App) of
        ok                             -> ok;
        {error, {already_loaded, App}} -> ok;
        Error                          -> Error
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
wait_for(State, Pattern) ->
    case expect(State, Pattern) of
        {nomatch, L, _} ->
            io:format(standard_error, "Skipping line ~s~n", [L]),
            wait_for(State, Pattern);
        Other ->
            Other
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
expect(State, Pattern) ->
    expect(State, Pattern, "").

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
expect(State, Pattern, Ignore) ->
    case read(State) of
        L when is_list(L) ->
            case re:run(L, Pattern, [{capture, none}]) of
                nomatch when Ignore =:= "" ->
                    {nomatch, L, Pattern};
                nomatch ->
                    case re:run(L, Ignore, [{capture, none}]) of
                        match   -> expect(State, Pattern, "");
                        nomatch -> {nomatch, L, [Pattern, Ignore]}
                    end;
                match ->
                    ok
            end;
        Other ->
            Other
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read(State = #state{devices = [Device | _]}) ->
    Acc = proplists:get_value(acc, get(), []),
    case {read_message(Acc), Device} of
        {{ok, {Message, Rest}}, _} ->
            put(acc, Rest),
            Message;
        {error, {M, Socket}} when M =:= gen_udp; M =:= gen_tcp ->
            receive
                {udp, Socket, _, _, Message} ->
                    Message;
                {tcp, Socket, Data} ->
                    put(acc, Acc ++ Data),
                    read(State)
            after
                1000 -> timeout
            end;
        {error, {file, IoDevice, _}} ->
            case file:read_line(IoDevice) of
                {ok, Data} ->
                    lists:reverse(tl(lists:reverse(Data)));
                eof ->
                    ok = timer:sleep(50),
                    read(State);
                Other ->
                    Other
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
