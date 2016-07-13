%%%=============================================================================
%%% Copyright 2013-2016, Tobias Schlager <schlagert@github.com>
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
%%% An RFC3164 compliant protocol backend for syslog messages.
%%% @end
%%%=============================================================================
-module(syslog_rfc3164).

-behaviour(syslog_logger).

%% API
-export([hdr/3, msg/2]).

-include("syslog.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Format the HDR part of RFC 3164 excluding the PRI.
%% @end
%%------------------------------------------------------------------------------
-spec hdr(syslog:datetime(), string(), #syslog_cfg{}) -> iodata().
hdr(Datetime, Pid, Cfg = #syslog_cfg{appname = A, beam_pid = B}) ->
    [
     get_date(Datetime), $\s,
     get_hostname(Cfg), $\s,
     A, $[, B, $], $\s,
     Pid, $\s, $-, $\s
    ].

%%------------------------------------------------------------------------------
%% @doc
%% Format the MSG part of RFC 3164. Basically a noop.
%% @end
%%------------------------------------------------------------------------------
-spec msg(binary(), #syslog_cfg{}) -> iodata().
msg(Msg, _Cfg) -> Msg.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_date({UtcDatetime, _MicroSecs}) ->
    format_date(erlang:universaltime_to_localtime(UtcDatetime)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
format_date({{_, Mo, D}, {H, Mi, S}}) ->
    [month(Mo), " ", day(D), " ", digit(H), $:, digit(Mi), $:, digit(S)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_hostname(#syslog_cfg{hostname = H, domain = D}) ->
    get_hostname(H, string:rstr(H, [$. | D])).
get_hostname(Hostname, Occurence) when Occurence > 2 ->
    string:sub_string(Hostname, 1, Occurence - 1);
get_hostname(Hostname, _) ->
    Hostname.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
month(1)  -> "Jan";
month(2)  -> "Feb";
month(3)  -> "Mar";
month(4)  -> "Apr";
month(5)  -> "May";
month(6)  -> "Jun";
month(7)  -> "Jul";
month(8)  -> "Aug";
month(9)  -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
day(1)  -> " 1";
day(2)  -> " 2";
day(3)  -> " 3";
day(4)  -> " 4";
day(5)  -> " 5";
day(6)  -> " 6";
day(7)  -> " 7";
day(8)  -> " 8";
day(9)  -> " 9";
day(N)  -> integer_to_list(N).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
digit(0)  -> "00";
digit(1)  -> "01";
digit(2)  -> "02";
digit(3)  -> "03";
digit(4)  -> "04";
digit(5)  -> "05";
digit(6)  -> "06";
digit(7)  -> "07";
digit(8)  -> "08";
digit(9)  -> "09";
digit(N)  -> integer_to_list(N).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_date_test() ->
    Datetime = {{{2013,4,6},{21,20,56}},908235},
    Rx = "Apr  6 \\d\\d:20:56",
    ?assertMatch({match, _}, re:run(lists:flatten(get_date(Datetime)), Rx)).

get_hostname_test() ->
    R1 = #syslog_cfg{hostname = "host.domain.com", domain = "domain.com"},
    ?assertEqual("host", get_hostname(R1)),
    R2 = #syslog_cfg{hostname = "host", domain = ""},
    ?assertEqual("host", get_hostname(R2)).

hdr_test() ->
    Datetime = {{{2013,4,6},{21,20,56}},908235},
    Pid = "init",
    Cfg = #syslog_cfg{hostname = "host.domain.com",
                      domain = "domain.com",
                      appname = "beam",
                      beam_pid = "12345"},
    Rx = <<"Apr  6 \\d\\d:20:56 host beam\\[12345\\] init - ">>,
    Actual = iolist_to_binary(hdr(Datetime, Pid, Cfg)),
    ?assertMatch({match, _}, re:run(Actual, Rx)).

msg_test() -> ?assertEqual(<<"info">>, msg(<<"info">>, #syslog_cfg{})).

-endif.
