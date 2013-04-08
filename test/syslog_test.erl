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

%%%=============================================================================
%%% TESTS
%%%=============================================================================

rfc3164_test() ->
    {ok, Socket} = setup(rfc3164),

    %% empty the mailbox
    receive _ -> ok after 100 -> ok end,

    ?assertEqual(ok, syslog:log(notice,   "hello world")),
    ?assertEqual(ok, syslog:log(critical, "hello world")),
    ?assertEqual(ok, syslog:log(error,    "hello ~s", ["world"])),

    Pid = pid_to_list(self()),
    Month = "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)",
    Date = Month ++ " (\\s|\\d)\\d \\d\\d:\\d\\d:\\d\\d",

    Re1 = "<29>" ++ Date ++ " \\w+ \\w+\\[\\d+\\] " ++ Pid ++ " - hello world",
    Msg1 = binary_to_list(assertReceive(Socket)),
    ?assertMatch({match, _}, re:run(Msg1, Re1)),

    Re2 = "<26>" ++ Date ++ " \\w+ \\w+\\[\\d+\\] " ++ Pid ++ " - hello world",
    Msg2 = binary_to_list(assertReceive(Socket)),
    ?assertMatch({match, _}, re:run(Msg2, Re2)),

    Re3 = "<27>" ++ Date ++ " \\w+ \\w+\\[\\d+\\] " ++ Pid ++ " - hello world",
    Msg3 = binary_to_list(assertReceive(Socket)),
    ?assertMatch({match, _}, re:run(Msg3, Re3)),

    teardown(Socket).

rfc5424_test() ->
    {ok, Socket} = setup(rfc5424),

    %% empty the mailbox
    receive _ -> ok after 100 -> ok end,

    ?assertEqual(ok, syslog:log(notice,   "hello world")),
    ?assertEqual(ok, syslog:log(critical, "hello world")),
    ?assertEqual(ok, syslog:log(error,    "hello ~s", ["world"])),

    Pid = pid_to_list(self()),
    Date = "\\d\\d\\d\\d-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d.\\d\\d\\d\\d\\d\\dZ",

    Re1 = "<29>1 " ++ Date ++ " \\w+ \\w+ \\d+ " ++ Pid ++ " - hello world",
    Msg1 = binary_to_list(assertReceive(Socket)),
    ?assertMatch({match, _}, re:run(Msg1, Re1)),

    Re2 = "<26>1 " ++ Date ++ " \\w+ \\w+ \\d+ " ++ Pid ++ " - hello world",
    Msg2 = binary_to_list(assertReceive(Socket)),
    ?assertMatch({match, _}, re:run(Msg2, Re2)),

    Re3 = "<27>1 " ++ Date ++ " \\w+ \\w+ \\d+ " ++ Pid ++ " - hello world",
    Msg3 = binary_to_list(assertReceive(Socket)),
    ?assertMatch({match, _}, re:run(Msg3, Re3)),

    teardown(Socket).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

setup(Protocol) ->
    ?assertEqual(ok, application:start(sasl)),
    {ok, [AppSpec]} = file:consult(filename:join(["..", "ebin", "syslog.app"])),
    ?assertEqual(ok, load(AppSpec)),
    ?assertEqual(ok, application:set_env(syslog, enabled, true)),
    ?assertEqual(ok, application:set_env(syslog, dest_port, ?TEST_PORT)),
    ?assertEqual(ok, application:set_env(syslog, protocol, Protocol)),
    ?assertEqual(ok, application:start(syslog)),
    gen_udp:open(?TEST_PORT, [binary, {reuseaddr, true}]).

teardown(Socket) ->
    application:stop(syslog),
    application:stop(sasl),
    gen_udp:close(Socket).

load(App) -> load(App, application:load(App)).
load(_, ok) -> ok;
load(App, {error, {already_loaded, App}}) -> ok.

assertReceive(Socket) -> receive {udp, Socket, _, _, Bin} -> Bin end.
