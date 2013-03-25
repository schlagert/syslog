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
%%% An RFC5424 compliant protocol backend for syslog messages.
%%% @end
%%%=============================================================================
-module(syslog_rfc5424).

-behaviour(syslog_h).

%% API
-export([to_iolist/1]).

-include("syslog.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Returns an `iolist' containing an RFC5424 compliant syslog report.
%% @end
%%------------------------------------------------------------------------------
-spec to_iolist(#syslog_report{}) -> iolist().
to_iolist(Report = #syslog_report{facility = F, severity = S}) ->
    [
     $<,
     integer_to_list((F bsl 3) + S),
     $>,
     $1,
     $\s,
     rfc5424_date(Report),
     $\s,
     truncate(255, Report#syslog_report.hostname),
     $\s,
     truncate(48, Report#syslog_report.appname),
     $\s,
     truncate(12, Report#syslog_report.beam_pid),
     $\s,
     truncate(32, Report#syslog_report.pid),
     $\s,
     $-,
     $\s,
     Report#syslog_report.bom,
     unicode:characters_to_binary(Report#syslog_report.msg)
    ].

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
rfc5424_date(#syslog_report{timestamp = {MegaSecs, Secs, MicroSecs}}) ->
    rfc5424_date(calendar:now_to_universal_time({MegaSecs, Secs, 0}), MicroSecs).
rfc5424_date({{Y, Mo, D}, {H, Mi, S}}, Micro) ->
    io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b.~6..0bZ",
                  [Y, Mo, D, H, Mi, S, Micro]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
truncate(L, S) when length(S) =< L -> S;
truncate(L, S)                     -> string:substr(S, 1, L).
