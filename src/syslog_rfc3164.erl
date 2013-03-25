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
     rfc3164_date(Report),
     $\s,
     rfc3164_hostname(Report),
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
rfc3164_date(#syslog_report{timestamp = {MegaSecs, Secs, MicroSecs}}) ->
    rfc3164_date(calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}));
rfc3164_date({{_, Mo, D}, {H, Mi, S}}) ->
    io_lib:format("~s ~2.. b ~2..0b:~2..0b:~2..0b", [month(Mo), D, H, Mi, S]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
rfc3164_hostname(#syslog_report{hostname = H, domain = D}) ->
    rfc3164_hostname(H, string:rstr(H, [$. | D])).
rfc3164_hostname(Hostname, Occurence) when Occurence > 2 ->
    string:sub_string(Hostname, 1, Occurence - 1);
rfc3164_hostname(Hostname, _) ->
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
