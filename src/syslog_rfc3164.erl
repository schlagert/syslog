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
%%%
%%% @doc
%%% An RFC3164 compliant protocol backend for syslog messages.
%%% @end
%%%=============================================================================
-module(syslog_rfc3164).

-behaviour(syslog_h).

%% API
-export([to_iolist/1]).

-include("syslog.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Returns an `iolist' containing an RFC3164 compliant syslog report.
%% @end
%%------------------------------------------------------------------------------
-spec to_iolist(#syslog_report{}) -> iolist().
to_iolist(Report = #syslog_report{facility = F, severity = S}) ->
    [
     $<,
     integer_to_list((F bsl 3) + S),
     $>,
     get_date(Report),
     $\s,
     get_hostname(Report),
     $\s,
     Report#syslog_report.appname,
     $[,
     Report#syslog_report.beam_pid,
     $],
     $\s,
     Report#syslog_report.pid,
     $\s,
     $\-,
     $\s,
     Report#syslog_report.msg
    ].

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_date(#syslog_report{timestamp = {MegaSecs, Secs, MicroSecs}}) ->
    get_date(calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}));
get_date({{_, Mo, D}, {H, Mi, S}}) ->
    io_lib:format("~s ~2.. b ~2..0b:~2..0b:~2..0b", [month(Mo), D, H, Mi, S]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_hostname(#syslog_report{hostname = H, domain = D}) ->
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

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_date_test() ->
    R = #syslog_report{timestamp = {1365,283256,908235}},
    ?assertEqual("Apr  6 23:20:56", lists:flatten(get_date(R))).

get_hostname_test() ->
    R1 = #syslog_report{hostname = "host.domain.com", domain = "domain.com"},
    ?assertEqual("host", get_hostname(R1)),
    R2 = #syslog_report{hostname = "host", domain = ""},
    ?assertEqual("host", get_hostname(R2)).

to_iolist_test() ->
    R = #syslog_report{severity = 5,
		       facility = 20,
		       timestamp = {1365,283256,908235},
		       hostname = "host.domain.com",
		       domain = "domain.com",
		       appname = "beam",
		       beam_pid = "12345",
		       pid = "init",
		       msg = "info goes here"},
    ?assertEqual(
       "<165>Apr  6 23:20:56 host beam[12345] init - info goes here",
       binary_to_list(iolist_to_binary(to_iolist(R)))).

-endif.
