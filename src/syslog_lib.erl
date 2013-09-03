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

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Returns the hostname of the running node. This may include the fully
%% qualified domain name.
%% @end
%%------------------------------------------------------------------------------
-spec get_hostname() -> string().
get_hostname()                -> get_hostname(atom_to_list(node())).
get_hostname("nonode@nohost") -> {ok, Hostname} = inet:gethostname(), Hostname;
get_hostname(Node)            -> hd(lists:reverse(string:tokens(Node, "@"))).

%%------------------------------------------------------------------------------
%% @doc
%% Returns the domain name of the running node. If the node was started using
%% short names, the returned string may be empty.
%% @end
%%------------------------------------------------------------------------------
-spec get_domain() -> string().
get_domain()           -> get_domain(string:tokens(get_hostname(), ".")).
get_domain([_])        -> "";
get_domain([_ | Rest]) -> string:join(Rest, ".").

%%------------------------------------------------------------------------------
%% @doc
%% Returns the name part of the running node. If the node is not running in
%% distributed mode (no nodename configured) the string `"beam"' will be
%% returned.
%% @end
%%------------------------------------------------------------------------------
-spec get_name() -> string().
get_name()                -> get_name(atom_to_list(node())).
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
%%% TESTS
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_hostname_test() ->
    {ok, InetReturn} = inet:gethostname(),
    ?assertEqual(InetReturn,        get_hostname("nonode@nohost")),
    ?assertEqual("hostname",        get_hostname("nodename@hostname")),
    ?assertEqual("hostname.domain", get_hostname("nodename@hostname.domain")).

get_domain_test() ->
    ?assertEqual("",          get_domain(string:tokens("host", "."))),
    ?assertEqual("domain",    get_domain(string:tokens("host.domain", "."))),
    ?assertEqual("domain.de", get_domain(string:tokens("host.domain.de", "."))).

get_name_test() ->
    ?assertEqual("beam",     get_name("nonode@nohost")),
    ?assertEqual("nodename", get_name("nodename@hostname")),
    ?assertEqual("nodename", get_name("nodename@hostname.dom.ain")).

get_pid_test() ->
    ?assertEqual("init", get_pid(init)),
    ?assertEqual("init", get_pid(whereis(init))),
    ?assertEqual(pid_to_list(self()), get_pid(self())).

-endif. %% TEST
