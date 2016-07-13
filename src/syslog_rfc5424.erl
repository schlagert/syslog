%%%=============================================================================
%%% Copyright 2011, Travelping GmbH <info@travelping.com>
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
%%% An RFC5424 compliant protocol backend for syslog messages.
%%% @end
%%%=============================================================================
-module(syslog_rfc5424).

-behaviour(syslog_logger).

%% API
-export([hdr/3, msg/2]).

-include("syslog.hrl").

-define(VERSION, $1).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Format the HDR part of RFC 5424 excluding the PRI, including structured
%% elements.
%% @end
%%------------------------------------------------------------------------------
-spec hdr(syslog:datetime(), binary(), #syslog_cfg{}) -> iodata().
hdr(Datetime, Pid, #syslog_cfg{hostname = H, appname = A, beam_pid = B}) ->
    [
     ?VERSION, $\s,
     get_date(Datetime), $\s,
     truncate(255, H), $\s,
     truncate(48, A), $\s,
     truncate(128, B), $\s,
     truncate(32, Pid), $\s, $-, $\s
    ].

%%------------------------------------------------------------------------------
%% @doc
%% Format the MSG part of RFC 5424.
%% @end
%%------------------------------------------------------------------------------
-spec msg(binary(), #syslog_cfg{}) -> binary().
msg(Msg, #syslog_cfg{bom = Bom}) ->
    unicode:characters_to_binary([Bom, Msg]).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_date({UtcDatetime, MicroSecs}) ->
    LocaDatetime = erlang:universaltime_to_localtime(UtcDatetime),
    get_date(LocaDatetime, UtcDatetime, MicroSecs).
get_date(Utc = {{Y, Mo, D}, {H, Mi, S}}, Utc, Micro) ->
    [integer_to_list(Y), $-, digit(Mo), $-, digit(D), $T,
     digit(H), $:, digit(Mi), $:, digit(S), $., micro(Micro), $Z];
get_date(Local = {{Y, Mo, D}, {H, Mi, S}}, Utc, Micro) ->
    {Sign, OH, OMi} = syslog_lib:get_utc_offset(Utc, Local),
    [integer_to_list(Y), $-, digit(Mo), $-, digit(D), $T,
     digit(H), $:, digit(Mi), $:, digit(S), $., micro(Micro),
     Sign, digit(OH), $:, digit(OMi)].

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
truncate(L, S) when size(S) =< L   -> S;
truncate(L, S) when is_list(S)     -> string:substr(S, 1, L);
truncate(L, S) when is_binary(S)   -> binary:part(S, 0, L).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_date_test() ->
    Datetime = {{{2013,4,6},{21,20,56}},908235},
    Rx = "2013-04-06T\\d\\d:\\d\\d:56\\.908235(Z|(\\+|-)\\d\\d:\\d\\d)",
    ?assertMatch({match, _}, re:run(lists:flatten(get_date(Datetime)), Rx)).

truncate_test() ->
    ?assertEqual("",        truncate(0, "")),
    ?assertEqual("",        truncate(1, "")),
    ?assertEqual("",        truncate(0, "123")),
    ?assertEqual("1",       truncate(1, "123")),
    ?assertEqual("12",      truncate(2, "123")),
    ?assertEqual("123",     truncate(3, "123")),
    ?assertEqual("123",     truncate(4, "123")),
    ?assertEqual(<<>>,      truncate(0, <<>>)),
    ?assertEqual(<<>>,      truncate(1, <<>>)),
    ?assertEqual(<<>>,      truncate(0, <<"123">>)),
    ?assertEqual(<<"1">>,   truncate(1, <<"123">>)),
    ?assertEqual(<<"12">>,  truncate(2, <<"123">>)),
    ?assertEqual(<<"123">>, truncate(3, <<"123">>)),
    ?assertEqual(<<"123">>, truncate(4, <<"123">>)).

-endif.
