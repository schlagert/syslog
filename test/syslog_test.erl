%%%=============================================================================
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

%%%=============================================================================
%%% TESTS
%%%=============================================================================

rfc3164_enabled_test() ->
    {ok, Socket} = setup(rfc3164, true),

    Pid = pid_to_list(self()),
    Month = "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)",
    Date = Month ++ " (\\s|\\d)\\d \\d\\d:\\d\\d:\\d\\d",

    ?assertEqual(ok, syslog:info_msg("hello world")),
    Re1 = "<29>" ++ Date ++ " \\w+ \\w+\\[\\d+\\] " ++ Pid ++ " - hello world",
    ?assertMatch({match, _}, re:run(read(Socket), Re1)),

    ?assertEqual(ok, syslog:msg(critical, "hello world", [])),
    Re2 = "<26>" ++ Date ++ " \\w+ \\w+\\[\\d+\\] " ++ Pid ++ " - hello world",
    ?assertMatch({match, _}, re:run(read(Socket), Re2)),

    ?assertEqual(ok, syslog:error_msg("hello ~s", ["world"])),
    Re3 = "<27>" ++ Date ++ " \\w+ \\w+\\[\\d+\\] " ++ Pid ++ " - hello world",
    ?assertMatch({match, _}, re:run(read(Socket), Re3)),

    ?assertEqual(ok, error_logger:error_msg("hello ~s", ["world"])),
    ?assertMatch({match, _}, re:run(read(Socket), Re3)),

    teardown(Socket).

rfc5424_enabled_test() ->
    {ok, Socket} = setup(rfc5424, true),

    Pid = pid_to_list(self()),
    Date = "\\d\\d\\d\\d-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d.\\d\\d\\d\\d\\d\\dZ",

    ?assertEqual(ok, syslog:info_msg("hello world")),
    Re1 = "<29>1 " ++ Date ++ " \\w+ \\w+ \\d+ " ++ Pid ++ " - hello world",
    ?assertMatch({match, _}, re:run(read(Socket), Re1)),

    ?assertEqual(ok, syslog:msg(critical, "hello world", [])),
    Re2 = "<26>1 " ++ Date ++ " \\w+ \\w+ \\d+ " ++ Pid ++ " - hello world",
    ?assertMatch({match, _}, re:run(read(Socket), Re2)),

    ?assertEqual(ok, syslog:error_msg("hello ~s", ["world"])),
    Re3 = "<27>1 " ++ Date ++ " \\w+ \\w+ \\d+ " ++ Pid ++ " - hello world",
    ?assertMatch({match, _}, re:run(read(Socket), Re3)),

    ?assertEqual(ok, error_logger:error_msg("hello ~s", ["world"])),
    ?assertMatch({match, _}, re:run(read(Socket), Re3)),

    teardown(Socket).

rfc5424_enable_disable_test() ->
    {ok, Socket} = setup(rfc5424, false),

    ?assertEqual(ok, syslog:info_msg("hello world")),
    ?assertEqual(ok, syslog:msg(critical, "hello world", [])),
    ?assertEqual(ok, syslog:error_msg("hello ~s", ["world"])),
    assert_socket_empty(Socket),

    ?assertEqual(ok, syslog:enable()),
    empty_mailbox(),

    Pid = pid_to_list(self()),
    Date = "\\d\\d\\d\\d-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d.\\d\\d\\d\\d\\d\\dZ",

    ?assertEqual(ok, syslog:info_msg("hello world")),
    Re1 = "<29>1 " ++ Date ++ " \\w+ \\w+ \\d+ " ++ Pid ++ " - hello world",
    ?assertMatch({match, _}, re:run(read(Socket), Re1)),

    ?assertEqual(ok, syslog:msg(critical, "hello world", [])),
    Re2 = "<26>1 " ++ Date ++ " \\w+ \\w+ \\d+ " ++ Pid ++ " - hello world",
    ?assertMatch({match, _}, re:run(read(Socket), Re2)),

    ?assertEqual(ok, syslog:error_msg("hello ~s", ["world"])),
    Re3 = "<27>1 " ++ Date ++ " \\w+ \\w+ \\d+ " ++ Pid ++ " - hello world",
    ?assertMatch({match, _}, re:run(read(Socket), Re3)),

    ?assertEqual(ok, syslog:disable()),

    ?assertEqual(ok, syslog:info_msg("hello world")),
    ?assertEqual(ok, syslog:msg(critical, "hello world", [])),
    ?assertEqual(ok, syslog:error_msg("hello ~s", ["world"])),
    assert_socket_empty(Socket),

    teardown(Socket).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

setup(Protocol, Enabled) ->
    ?assertEqual(ok, application:start(sasl)),
    AppFile = filename:join(["..", "src", "syslog.app.src"]),
    {ok, [AppSpec]} = file:consult(AppFile),
    ?assertEqual(ok, load(AppSpec)),
    ?assertEqual(ok, application:set_env(syslog, enabled, Enabled)),
    ?assertEqual(ok, application:set_env(syslog, dest_port, ?TEST_PORT)),
    ?assertEqual(ok, application:set_env(syslog, protocol, Protocol)),
    ?assertEqual(ok, application:start(syslog)),
    ?assertEqual(ok, empty_mailbox()),
    gen_udp:open(?TEST_PORT, [binary, {reuseaddr, true}]).

teardown(Socket) ->
    application:stop(syslog),
    application:stop(sasl),
    application:unset_env(syslog, enabled),
    application:unset_env(syslog, dest_port),
    application:unset_env(syslog, protocol),
    gen_udp:close(Socket).

load(App) -> load(App, application:load(App)).
load(_, ok) -> ok;
load(App, {error, {already_loaded, App}}) -> ok.

read(Socket) -> receive {udp, Socket, _, _, Bin} -> binary_to_list(Bin) end.

empty_mailbox() -> receive _ -> empty_mailbox() after ?TIMEOUT -> ok end.

assert_socket_empty(Socket) ->
    receive
        {udp, Socket, _, _, Bin} ->
            throw({test_failed, {unexpected_message, binary_to_list(Bin)}})
    after
        ?TIMEOUT -> ok
    end.
