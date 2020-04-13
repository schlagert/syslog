%%%=============================================================================
%%% Copyright 2011, Travelping GmbH <info@travelping.com>
%%% Copyright 2013-2017, Tobias Schlager <schlagert@github.com>
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
-export([get_hostname/1,
         get_name/0,
         get_name_metdata_key/0,
         get_property/2,
         get_property/3,
         get_pid/1,
         get_utc_datetime/1,
         get_utc_offset/2,
         truncate/2,
         format_rfc3164_date/1,
         format_rfc5424_date/1,
         get_structured_data/3,
         has_error_logger/0,
         to_type/2]).

-define(GET_ENV(Property), application:get_env(syslog, Property)).

-include_lib("kernel/include/inet.hrl").

%% calendar:system_time_to_universal_time/2
-dialyzer({no_missing_calls, get_utc_datetime/1}).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Returns the hostname of the running node. This may include the fully
%% qualified domain name. The hostname will usually be the host part of the
%% node name, except for the cases when the node is not alive or some strange
%% host part was set, e.g. something related to the loopback interface. In this
%% case the hostname will be what `inet:gethostname/0` returns, optionally with
%% the domain removed.
%% @end
%%------------------------------------------------------------------------------
-spec get_hostname(none | short | long) -> string().
get_hostname(Transform) when is_atom(Transform) ->
    get_hostname(Transform, get_hostpart(node())).
get_hostname(none, HostPart) when is_list(HostPart) ->
    clean_hostpart(HostPart);
get_hostname(short, HostPart) when is_list(HostPart) ->
    CleanHostPart = clean_hostpart(HostPart),
    case is_ip4(CleanHostPart) of
        true  -> CleanHostPart;
        false -> hd(string:tokens(CleanHostPart, "."))
    end;
get_hostname(long, HostPart) when is_list(HostPart) ->
    CleanHostPart = clean_hostpart(HostPart),
    case is_ip4(CleanHostPart) of
        true  -> CleanHostPart;
        false ->
            case lists:member($., CleanHostPart) of
                true  -> CleanHostPart;
                false ->
                    case inet:gethostbyname(CleanHostPart) of
                        {ok, #hostent{h_name=Hostname}} -> Hostname;
                        {error, _}                      -> CleanHostPart
                    end
            end
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
        {ok, Name} ->
            to_type(binary, Name);
        undefined ->
            case ?GET_ENV(appname) of
                {ok, Name} -> to_type(binary, Name);
                undefined  -> get_name_from_node(node())
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the key that should be used to lookup a message metadata value to
%% place in the `APP-NAME' field. If a message metadata field contains such a
%% mapping this will have higher precendence over names configured/returned by
%% {@link get_name/0}. `undefined' is used as the invalid/unconfigured value.
%% @end
%%------------------------------------------------------------------------------
-spec get_name_metdata_key() -> atom() | undefined.
get_name_metdata_key() ->
    case ?GET_ENV(appname_from_metadata) of
        {ok, Value} when is_atom(Value) -> Value;
        _                               -> undefined
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the value for a specific key from the application environment. If no
%% value is configured or the application environment can not be read the
%% provided default value is returned.
%% @end
%%------------------------------------------------------------------------------
-spec get_property(atom(), term()) -> term().
get_property(Property, Default) ->
    case {?GET_ENV(Property), Default} of
        {{ok, Value}, _} -> Value;
        {_, Value}       -> Value
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Similar to {@link get_property/2}. Additionally, this function allows to
%% specifiy the desired target type. The configured value will be converted to
%% the desired type. If this is not possible, the function crashes.
%% @end
%%------------------------------------------------------------------------------
-spec get_property(atom(), term(), binary | integer | ip_addr) -> term().
get_property(Property, Default, Type) ->
    to_type(Type, get_property(Property, Default)).

%%------------------------------------------------------------------------------
%% @doc
%% Returns a string representation for a process. This will either be the
%% (locally) registered name of the process or its process id.
%% @end
%%------------------------------------------------------------------------------
-spec get_pid(pid() | atom() | string()) -> binary().
get_pid(P) when is_pid(P) ->
    case catch process_info(P, registered_name) of
        {registered_name, N} -> to_type(binary, N);
        _                    -> to_type(binary, P)
    end;
get_pid(N) ->
    to_type(binary, N).

%%------------------------------------------------------------------------------
%% @doc
%% Returns a syslog datetime object (UTC) with microsecond resolution from
%% either an Erlang timestamp or the system time in microseconds.
%% @end
%%------------------------------------------------------------------------------
-spec get_utc_datetime(erlang:timestamp() | pos_integer()) -> syslog:datetime().
get_utc_datetime(SystemTime) when is_integer(SystemTime), SystemTime > 0 ->
    MilliSecs = SystemTime div 1000,
    MicroSecs = SystemTime rem 1000000,
    Datetime = calendar:system_time_to_universal_time(MilliSecs, millisecond),
    {Datetime, MicroSecs};
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

%%------------------------------------------------------------------------------
%% @doc
%% Returns structured data from a list or map of metadata (e.g. metadata
%% provided in `lager' messages or `logger' events).
%% @end
%%------------------------------------------------------------------------------
-spec get_structured_data(map() | list(), syslog:sd_id(), [atom()]) ->
                                 [syslog:sd_element()].
get_structured_data(Metadata, SDId, MDKeys) when is_map(Metadata) ->
    get_structured_data(maps:to_list(Metadata), SDId, MDKeys);
get_structured_data(Metadata, SDId, MDKeys) when is_list(Metadata) ->
    case [D || D = {K, _} <- Metadata, lists:member(K, MDKeys)] of
        []       -> [];
        SDParams -> [{SDId, SDParams}]
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Determine whether `error_logger' is available (e.g. we are running a pre
%% OTP-21 release) or not.
%% @end
%%------------------------------------------------------------------------------
-spec has_error_logger() -> boolean().
has_error_logger() ->
    case whereis(error_logger) of
        P when is_pid(P) -> true;
        undefined        -> false
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Convert variables from one type to another
%% @end
%%------------------------------------------------------------------------------
to_type(binary, V) when is_binary(V)   -> V;
to_type(binary, V) when is_list(V)     -> list_to_binary(V);
to_type(binary, V) when is_atom(V)     -> atom_to_binary(V, utf8);
to_type(integer, V) when is_integer(V) -> V;
to_type(integer, V) when is_list(V)    -> list_to_integer(V);
to_type(ip_addr, V) when is_tuple(V)   -> V;
to_type(ip_addr, V) when is_list(V)    -> to_ip_addr_type(V);
to_type(Type, V) when is_pid(V)        -> to_type(Type, pid_to_list(V));
to_type(Type, V) when is_integer(V)    -> to_type(Type, integer_to_list(V));
to_type(Type, V) when is_binary(V)     -> to_type(Type, binary_to_list(V)).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_name_from_node(Node) when is_atom(Node) ->
    case atom_to_binary(Node, utf8) of
        <<"nonode@nohost">> -> <<"beam">>;
        N                   -> hd(binary:split(N, <<"@">>))
    end.

%%------------------------------------------------------------------------------
%% @private
%% Return the host part of the node name
%%------------------------------------------------------------------------------
get_hostpart(Node) when is_atom(Node) ->
    lists:last(string:tokens(atom_to_list(Node), "@")).

%%------------------------------------------------------------------------------
%% @private
%% Check for cases when the node is not alive or some strange host part was set,
%% e.g. something related to the loopback interface and replace host part
%% with the result of `inet:gethostname/0`
%%------------------------------------------------------------------------------
clean_hostpart("nohost") ->
    element(2, inet:gethostname());
clean_hostpart(HostPart) when is_list(HostPart) ->
    case lists:member(HostPart, get_loopback_names()) of
        true  -> element(2, inet:gethostname());
        false -> HostPart
    end.

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
is_ip4(Str) ->
  re:run(Str, "\\A\\d+\\.\\d+\\.\\d+\\.\\d+\\Z", [{capture, none}]) =:= match.

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

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
to_ip_addr_type(V) ->
    case inet:parse_address(V) of
        {error, einval} -> try_inet_getaddr(V, [inet, inet6]);
        {ok, IpAddr}    -> IpAddr
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
try_inet_getaddr(V, [AddrFamily | Rest]) ->
    handle_inet_getaddr(inet:getaddr(V, AddrFamily), V, Rest).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_inet_getaddr(_, _, []) ->
    error(invalid_dest_host);
handle_inet_getaddr({error, _}, V, AddrFamilies) ->
    try_inet_getaddr(V, AddrFamilies);
handle_inet_getaddr({ok, IpAddr}, _, _) ->
    IpAddr.

%%%=============================================================================
%%% TESTS
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_hostname_ip_test() ->
    ?assertEqual("192.168.1.1", get_hostname(none,  "192.168.1.1")),
    ?assertEqual("192.168.1.1", get_hostname(short, "192.168.1.1")),
    ?assertEqual("192.168.1.1", get_hostname(long,  "192.168.1.1")).

get_hostname_none_test() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, #hostent{h_name=Localhost}} = inet:gethostbyaddr("127.0.0.1"),
    ?assertEqual(Hostname,          get_hostname(none, "nohost")),
    ?assertEqual(Hostname,          get_hostname(none, "127.0.0.1")),
    ?assertEqual(Hostname,          get_hostname(none, Localhost)),
    ?assertEqual("hostname",        get_hostname(none, "hostname")),
    ?assertEqual("hostname.domain", get_hostname(none, "hostname.domain")).

get_hostname_short_test() ->
    {ok, Hostname} = inet:gethostname(),
    HostnameShort = hd(string:tokens(Hostname, ".")),
    {ok, #hostent{h_name=Localhost}} = inet:gethostbyaddr("127.0.0.1"),
    ?assertEqual(HostnameShort, get_hostname(short, "nohost")),
    ?assertEqual(HostnameShort, get_hostname(short, "127.0.0.1")),
    ?assertEqual(HostnameShort, get_hostname(short, Localhost)),
    ?assertEqual("hostname",    get_hostname(short, "hostname")),
    ?assertEqual("hostname",    get_hostname(short, "hostname.domain")).

get_hostname_long_test() ->
    {ok, Hostname} = inet:gethostname(),
    HostnameLong1 = case inet:gethostbyname(Hostname) of
        {ok, #hostent{h_name=Hname1}} -> Hname1;
        {error, _}                    -> Hostname
    end,
    HostnameLong2 = case inet:gethostbyname("hostname") of
        {ok, #hostent{h_name=Hname2}} -> Hname2;
        {error, _}                    -> "hostname"
    end,
    {ok, #hostent{h_name=Localhost}} = inet:gethostbyaddr("127.0.0.1"),
    ?assertEqual(HostnameLong1,     get_hostname(long, "nohost")),
    ?assertEqual(HostnameLong1,     get_hostname(long, "127.0.0.1")),
    ?assertEqual(HostnameLong1,     get_hostname(long, Localhost)),
    ?assertEqual(HostnameLong2,     get_hostname(long, "hostname")),
    ?assertEqual("hostname.domain", get_hostname(long, "hostname.domain")).

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
    Rx = "Apr  [67] \\d\\d:20:56",
    ?assertMatch({match, _}, re:run(lists:flatten(Date), Rx)).

format_rfc5424_date_test() ->
    Datetime = {{{2013,4,6},{21,20,56}},908235},
    Date = format_rfc5424_date(Datetime),
    Rx = "2013-04-0[67]T\\d\\d:\\d\\d:56\\.908235(Z|(\\+|-)\\d\\d:\\d\\d)",
    ?assertMatch({match, _}, re:run(lists:flatten(Date), Rx)).

to_type_test() ->
    {ok, #hostent{h_name=Localhost}} = inet:gethostbyaddr("127.0.0.1"),
    ?assertEqual(<<"1">>, to_type(binary, <<"1">>)),
    ?assertEqual(<<"1">>, to_type(binary, "1")),
    ?assertEqual(<<"1">>, to_type(binary, 1)),
    ?assert(is_binary(to_type(binary, self()))),
    ?assertEqual(<<"1">>, to_type(binary, '1')),
    ?assertEqual(1, to_type(integer, <<"1">>)),
    ?assertEqual(1, to_type(integer, "1")),
    ?assertEqual(1, to_type(integer, 1)),
    ?assertEqual({127,0,0,1}, to_type(ip_addr, Localhost)),
    ?assertEqual({127,0,0,1}, to_type(ip_addr, list_to_binary(Localhost))),
    ?assertEqual({127,0,0,1}, to_type(ip_addr, <<"127.0.0.1">>)),
    ?assertEqual({127,0,0,1}, to_type(ip_addr, "127.0.0.1")),
    ?assertEqual({0,0,0,0,0,0,0,1}, to_type(ip_addr, "::1")),
    ?assertError(invalid_dest_host, to_type(ip_addr, "5dzFvraZ7lZUAlQu")).

-endif. %% TEST
