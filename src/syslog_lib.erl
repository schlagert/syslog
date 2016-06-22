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
%%% A library module providing `syslog' specific utility functions.
%%% @end
%%%=============================================================================
-module(syslog_lib).

%% API
-export([get_hostname/0,
         get_domain/0,
         get_name/0,
         get_property/2,
         get_pid/1]).

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
%% Returns the name part of the running node. If the node is not running in
%% distributed mode (no nodename configured) the string `"beam"' will be
%% returned.
%% @end
%%------------------------------------------------------------------------------
-spec get_name() -> string().
get_name()                ->
  case syslog_lib:get_property(appname, undefined) of
    undefined -> get_name(atom_to_list(node()));
    Name -> Name
  end.
get_name("nonode@nohost") -> "beam";
get_name(Node)            -> hd(string:tokens(Node, "@")).

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
-spec get_pid(pid() | atom()) -> string().
get_pid(N) when is_atom(N) ->
    atom_to_list(N);
get_pid(P) when is_pid(P) ->
    case catch process_info(P, registered_name) of
        {registered_name, N} -> atom_to_list(N);
        _                    -> pid_to_list(P)
    end.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

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

get_name_test() ->
    ?assertEqual("beam",     get_name("nonode@nohost")),
    ?assertEqual("nodename", get_name("nodename@hostname")),
    ?assertEqual("nodename", get_name("nodename@hostname.dom.ain")).

get_pid_test() ->
    ?assertEqual("init", get_pid(init)),
    ?assertEqual("init", get_pid(whereis(init))),
    ?assertEqual(pid_to_list(self()), get_pid(self())).

-endif. %% TEST
