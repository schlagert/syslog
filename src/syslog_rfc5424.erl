%%%=============================================================================
%%% Copyright 2011, Travelping GmbH <info@travelping.com>
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
%%% An RFC5424 compliant protocol backend for syslog messages.
%%% @end
%%%=============================================================================
-module(syslog_rfc5424).

-behaviour(syslog_logger_h).

%% API
-export([to_iolist/1]).

-include("syslog.hrl").

-define(VERSION, $1).

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
     ?VERSION,
     $\s,
     get_date(Report),
     $\s,
     truncate(255, Report#syslog_report.hostname),
     $\s,
     truncate(48, Report#syslog_report.appname),
     $\s,
     truncate(128, Report#syslog_report.beam_pid),
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
get_date(#syslog_report{datetime = {Datetime, MicroSecs}}) ->
    get_date(Datetime, MicroSecs).
get_date({{Y, Mo, D}, {H, Mi, S}}, Micro) ->
    [integer_to_list(Y), "-", digit(Mo), "-", digit(D), "T",
     digit(H), ":", digit(Mi), ":", digit(S), ".", micro(Micro), "Z"].

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

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
micro(M) when M < 10     -> ["00000", integer_to_list(M)];
micro(M) when M < 100    -> ["0000", integer_to_list(M)];
micro(M) when M < 1000   -> ["000", integer_to_list(M)];
micro(M) when M < 10000  -> ["00", integer_to_list(M)];
micro(M) when M < 100000 -> ["0", integer_to_list(M)];
micro(M)                 -> integer_to_list(M).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
truncate(L, S) when length(S) =< L -> S;
truncate(L, S)                     -> string:substr(S, 1, L).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_date_test() ->
    R = #syslog_report{datetime = {{{2013,4,6},{21,20,56}},908235}},
    ?assertEqual("2013-04-06T21:20:56.908235Z", lists:flatten(get_date(R))).

truncate_test() ->
    ?assertEqual("",    truncate(0, "")),
    ?assertEqual("",    truncate(1, "")),
    ?assertEqual("",    truncate(0, "123")),
    ?assertEqual("1",   truncate(1, "123")),
    ?assertEqual("12",  truncate(2, "123")),
    ?assertEqual("123", truncate(3, "123")),
    ?assertEqual("123", truncate(4, "123")).

-endif.
