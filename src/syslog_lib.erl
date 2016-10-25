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
%%% A library module providing `syslog' specific utility functions.
%%% @end
%%%=============================================================================
-module(syslog_lib).

%% API
-export([get_hostname/0,
         get_domain/0,
         get_name/0,
         get_property/2,
         get_pid/1,
         get_utc_datetime/1,
         get_utc_offset/2,
         truncate/2,
         format_rfc3164_date/1,
         format_rfc5424_date/1]).

-define(GET_ENV(Property), application:get_env(syslog, Property)).

-include_lib("kernel/include/inet.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Returns the hostname of the running node. This may include the fully
%% qualified domain name. The hostname will usually be the host part of the
%% node name, except for the cases when the node is not alive or some strange
%% host part was set, e.g. something related to the loopback interface. In this
%% case the hostname will be what `inet:gethostname/0` returns.
%% @end
%%------------------------------------------------------------------------------
-spec get_hostname() -> string().
get_hostname() ->
    get_hostname(get_hostpart(node())).
get_hostname(HostPart) ->
    case lists:member(HostPart, ["nohost" | get_loopback_names()]) of
        true  -> element(2, inet:gethostname());
        false -> HostPart
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the domain name of the running node. If the node was started using
%% short names or if an IP address was set as host part of the node name, the
%% returned string may be empty.
%% @end
%%------------------------------------------------------------------------------
-spec get_domain() -> string().
get_domain() ->
    get_domain(get_hostname()).
get_domain(Hostname) ->
    case {is_ip4(Hostname), string:tokens(Hostname, ".")} of
        {true, _}       -> "";
        {_, [_]}        -> "";
        {_, [_ | Rest]} -> string:join(Rest, ".")
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the name reported in the `APP-NAME' field. If no name is configured
%% using the application environment, name part of the running node is returned.
%% If the node is not running in distributed mode (no nodename configured) the
%% string `"beam"' will be returned.
%% @end
%%------------------------------------------------------------------------------
-spec get_name() -> binary().
get_name() ->
    case ?GET_ENV(app_name) of
        {ok, Name} when is_binary(Name) -> Name;
        {ok, Name} when is_list(Name)   -> list_to_binary(Name);
        {ok, Name} when is_atom(Name)   -> atom_to_binary(Name, utf8);
        undefined                       -> get_name_from_node(node())
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the value for a specific key from the application environment. If no
%% value is configured or the application environment can not be read the
%% provided default value is returned.
%% @end
%%------------------------------------------------------------------------------
-spec get_property(atom(), term()) -> term().
get_property(Property, Default) -> get_property_(?GET_ENV(Property), Default).
get_property_({ok, Value}, _)   -> Value;
get_property_(_, Value)         -> Value.

%%------------------------------------------------------------------------------
%% @doc
%% Returns a string representation for a process. This will either be the
%% (locally) registered name of the process or its process id.
%% @end
%%------------------------------------------------------------------------------
-spec get_pid(pid() | atom() | string()) -> binary().
get_pid(N) when is_list(N) ->
    list_to_binary(N);
get_pid(N) when is_atom(N) ->
    atom_to_binary(N, utf8);
get_pid(P) when is_pid(P) ->
    case catch process_info(P, registered_name) of
        {registered_name, N} -> atom_to_binary(N, utf8);
        _                    -> list_to_binary(pid_to_list(P))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns a syslog datetime object (UTC) with microsecond resolution.
%% @end
%%------------------------------------------------------------------------------
-spec get_utc_datetime(erlang:timestamp()) -> syslog:datetime().
get_utc_datetime({MegaSecs, Secs, MicroSecs}) ->
    Datetime = calendar:now_to_universal_time({MegaSecs, Secs, 0}),
    {Datetime, MicroSecs}.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the offset of a local datetime from the given UTC datetime.
%% @end
%%------------------------------------------------------------------------------
-spec get_utc_offset(calendar:datetime(), calendar:datetime()) ->
                            {43 | 45, 0..23, 0..59}.
get_utc_offset(Utc, Local) when Utc < Local ->
    {0, {H, Mi, 0}} = time_difference(Utc, Local),
    {$+, H, Mi};
get_utc_offset(Utc, Local) ->
    {0, {H, Mi, 0}} = time_difference(Local, Utc),
    {$-, H, Mi}.

%%------------------------------------------------------------------------------
%% @doc
%% Returns a truncated string with at most `Len' characters/bytes.
%% @end
%%------------------------------------------------------------------------------
-spec truncate(pos_integer(), string() | binary()) -> string() | binary().
truncate(Len, Str) when length(Str) =< Len -> Str;
truncate(Len, Str) when size(Str) =< Len   -> Str;
truncate(Len, Str) when is_list(Str)       -> string:substr(Str, 1, Len);
truncate(Len, Str) when is_binary(Str)     -> binary:part(Str, 0, Len).

%%------------------------------------------------------------------------------
%% @doc
%% Formats a (UTC) timestamp according to RFC5424. The returned timestamp will
%% be in local time with UTC offset (if available).
%% @end
%%------------------------------------------------------------------------------
-spec format_rfc5424_date(syslog:datetime()) -> iodata().
format_rfc5424_date({UtcDatetime, MicroSecs}) ->
    LocaDatetime = erlang:universaltime_to_localtime(UtcDatetime),
    format_rfc5424_date(LocaDatetime, UtcDatetime, MicroSecs).
format_rfc5424_date(Utc = {{Y, Mo, D}, {H, Mi, S}}, Utc, Micro) ->
    [integer_to_list(Y), $-, digit(Mo), $-, digit(D), $T,
     digit(H), $:, digit(Mi), $:, digit(S), $., micro(Micro), $Z];
format_rfc5424_date(Local = {{Y, Mo, D}, {H, Mi, S}}, Utc, Micro) ->
    {Sign, OH, OMi} = get_utc_offset(Utc, Local),
    [integer_to_list(Y), $-, digit(Mo), $-, digit(D), $T,
     digit(H), $:, digit(Mi), $:, digit(S), $., micro(Micro),
     Sign, digit(OH), $:, digit(OMi)].

%%------------------------------------------------------------------------------
%% @doc
%% Formats a (UTC) timestamp according to RFC3164. The returned timestamp will
%% be in local time.
%% @end
%%------------------------------------------------------------------------------
-spec format_rfc3164_date(syslog:datetime()) -> iodata().
format_rfc3164_date({UtcDatetime, _MicroSecs}) ->
    format_rfc3164_date_(erlang:universaltime_to_localtime(UtcDatetime)).
format_rfc3164_date_({{_, Mo, D}, {H, Mi, S}}) ->
    [month(Mo), " ", day(D), " ", digit(H), $:, digit(Mi), $:, digit(S)].

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_name_from_node(Node) ->
    case atom_to_binary(Node, utf8) of
        <<"nonode@nohost">> -> <<"beam">>;
        N                   -> hd(binary:split(N, <<"@">>))
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_hostpart(Node) ->
    hd(lists:reverse(string:tokens(atom_to_list(Node), "@"))).

%%------------------------------------------------------------------------------
%% @private
%% Return all possible names of the available loopback interfaces, e.g.
%% `["127.0.0.1", "localhost", "localhost.localdomain", ...]'
%%------------------------------------------------------------------------------
get_loopback_names() ->
    Addresses = get_loopback_addresses(),
    lists:append(
      [Name || Addr <- Addresses,
               Name <- [ntoa(Addr)],
               is_list(Name)],
      [Name || Addr <- Addresses,
               {ok, Hostent} <- [inet:gethostbyaddr(Addr)],
               Name <- [Hostent#hostent.h_name | Hostent#hostent.h_aliases],
               is_list(Name)]).

%%------------------------------------------------------------------------------
%% @private
%% Returns the IPv4 addresses associated with the available loopback interfaces.
%%------------------------------------------------------------------------------
get_loopback_addresses() ->
    [Addr || {ok, IfList} <- [inet:getifaddrs()],
             {_, IfProps} <- IfList,
             {addr, Addr = {_, _, _, _}} <- IfProps,
             {flags, IfFlags} <- IfProps,
             lists:member(loopback, IfFlags)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
ntoa(IPv4) -> lists:flatten(io_lib:format("~w.~w.~w.~w", tuple_to_list(IPv4))).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
is_ip4(Str) -> re:run(Str, "\\d+.\\d+.\\d+.\\d+", [{capture, none}]) =:= match.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
time_difference(T1, T2) ->
    calendar:seconds_to_daystime(
      calendar:datetime_to_gregorian_seconds(T2) -
          calendar:datetime_to_gregorian_seconds(T1)).

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

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
micro(M) when M < 10     -> ["00000", integer_to_list(M)];
micro(M) when M < 100    -> ["0000", integer_to_list(M)];
micro(M) when M < 1000   -> ["000", integer_to_list(M)];
micro(M) when M < 10000  -> ["00", integer_to_list(M)];
micro(M) when M < 100000 -> ["0", integer_to_list(M)];
micro(M)                 -> integer_to_list(M).

%%%=============================================================================
%%% TESTS
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_hostname_test() ->
    {ok, InetReturn} = inet:gethostname(),
    ?assertEqual(InetReturn,        get_hostname("nohost")),
    ?assertEqual(InetReturn,        get_hostname("127.0.0.1")),
    ?assertEqual(InetReturn,        get_hostname("localhost")),
    ?assertEqual("hostname",        get_hostname("hostname")),
    ?assertEqual("hostname.domain", get_hostname("hostname.domain")).

get_domain_test() ->
    ?assertEqual("",          get_domain("host")),
    ?assertEqual("",          get_domain("127.0.0.1")),
    ?assertEqual("domain",    get_domain("host.domain")),
    ?assertEqual("domain.de", get_domain("host.domain.de")).

get_name_from_node_test() ->
    ?assertEqual(<<"beam">>,     get_name_from_node('nonode@nohost')),
    ?assertEqual(<<"nodename">>, get_name_from_node('nodename@hostname')),
    ?assertEqual(<<"nodename">>, get_name_from_node('nodename@hostname.dom.ain')).

get_pid_test() ->
    ?assertEqual(<<"init">>, get_pid(init)),
    ?assertEqual(<<"init">>, get_pid(whereis(init))),
    ?assertEqual(list_to_binary(pid_to_list(self())), get_pid(self())).

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

format_rfc3164_date_test() ->
    Datetime = {{{2013,4,6},{21,20,56}},908235},
    Date = format_rfc3164_date(Datetime),
    Rx = "Apr  6 \\d\\d:20:56",
    ?assertMatch({match, _}, re:run(lists:flatten(Date), Rx)).

format_rfc5424_date_test() ->
    Datetime = {{{2013,4,6},{21,20,56}},908235},
    Date = format_rfc5424_date(Datetime),
    Rx = "2013-04-06T\\d\\d:\\d\\d:56\\.908235(Z|(\\+|-)\\d\\d:\\d\\d)",
    ?assertMatch({match, _}, re:run(lists:flatten(Date), Rx)).

-endif. %% TEST
