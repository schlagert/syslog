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
%%% The event handler for to be attached to the `error_logger' event manager.
%%% This module will handle all log message, format them and forward them to the
%%% configured protocol handler. Protocol handler must implement the `syslog_h'
%%% behaviour.
%%% @end
%%%=============================================================================
-module(syslog_h).

-behaviour(gen_event).

%% API
-export([attach/1]).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("syslog.hrl").

%%%=============================================================================
%%% callback definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% This is the behaviour that must be implemented by protocol backends.
%%------------------------------------------------------------------------------

-callback to_iolist(#syslog_report{}) -> iolist().

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Attach this module as event handler for `error_logger' events. The
%% connection between the event manager and the handler will be supervised by
%% the calling process. The handler will automatically be detached when the
%% calling process exits.
%% @end
%%------------------------------------------------------------------------------
-spec attach(gen_udp:socket()) -> ok | term().
attach(Socket) -> gen_event:add_sup_handler(error_logger, ?MODULE, [Socket]).

%%%=============================================================================
%%% gen_event callbacks
%%%=============================================================================

-record(state, {
          socket         :: gen_udp:socket(),
          protocol       :: module(),
          facility       :: syslog:facility(),
          error_facility :: syslog:facility(),
          dest_host      :: inet:ip_address() | inet:hostname(),
          dest_port      :: inet:port_number(),
          hostname       :: string(),
          domain         :: string(),
          appname        :: string(),
          beam_pid       :: string(),
          bom            :: binary()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([Socket]) ->
    Env = application:get_all_env(syslog),
    Protocol = proplists:get_value(protocol, Env, ?PROTOCOL),
    Port = get_port(proplists:get_value(dest_port, Env, ?DEST_PORT), Protocol),
    UseBOM = proplists:get_value(use_rfc5424_bom, Env, ?USE_BOM),
    {ok, #state{
            socket         = Socket,
            protocol       = get_module(Protocol),
            facility       = proplists:get_value(facility, Env, ?FACILITY),
            error_facility = proplists:get_value(error_facility, Env, ?FACILITY),
            dest_host      = proplists:get_value(dest_host, Env, ?DEST_HOST),
            dest_port      = Port,
            hostname       = get_hostname(),
            domain         = get_domain(),
            appname        = get_appname(),
            beam_pid       = os:getpid(),
            bom            = get_bom(UseBOM)}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event({error, _, {Pid, Fmt, Args}}, State) ->
    {ok, send(format_msg(error, Pid, Fmt, Args, State), State)};
handle_event({error_report, _, {Pid, Type, Report}}, State) ->
    {ok, send(format_report(error, Pid, Type, Report, State), State)};
handle_event({warning_msg, _, {Pid, Fmt, Args}}, State) ->
    {ok, send(format_msg(warning, Pid, Fmt, Args, State), State)};
handle_event({warning_report, _, {Pid, Type, Report}}, State) ->
    {ok, send(format_report(warning, Pid, Type, Report, State), State)};
handle_event({info_msg, _, {Pid, Fmt, Args}}, State) ->
    {ok, send(format_msg(notice, Pid, Fmt, Args, State), State)};
handle_event({info_report, _, {Pid, Type, Report}}, State) ->
    {ok, send(format_report(notice, Pid, Type, Report, State), State)};
handle_event(_, State) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(_Request, State) -> {ok, undef, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(_Info, State) -> {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Arg, _State) -> ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
format_msg(Severity, Pid, Fmt, Args, State) ->
    (get_report(State))#syslog_report{
      severity  = get_severity(Severity),
      facility  = get_facility(Severity, State),
      timestamp = os:timestamp(),
      pid       = get_pid(Pid),
      msg       = format(Fmt, Args)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
format_report(_, Pid, crash_report, Report, State) ->
    Timestamp = os:timestamp(),
    Event = {calendar:now_to_local_time(Timestamp),
             {error_report, self(), {Pid, crash_report, Report}}},
    (get_report(State))#syslog_report{
      severity  = get_severity(critical),
      facility  = get_facility(critical, State),
      timestamp = Timestamp,
      pid       = get_pid(Pid),
      msg       = sasl_report:format_report(fd, crash_report, Event)};
format_report(_, Pid, _, [{application, A}, {started_at, N} | _], State) ->
    (get_report(State))#syslog_report{
      severity  = get_severity(informational),
      facility  = get_facility(informational, State),
      timestamp = os:timestamp(),
      pid       = get_pid(Pid),
      msg       = format("started application ~w on node ~w", [A, N])};
format_report(_, Pid, _, [{application, A}, {exited, R} | _], State) ->
    (get_report(State))#syslog_report{
      severity  = get_severity(error),
      facility  = get_facility(error, State),
      timestamp = os:timestamp(),
      pid       = get_pid(Pid),
      msg       = format("application ~w exited with ~768p", [A, R])};
format_report(_, Pid, _, [{started, Details} | _], State) ->
    Child = get_pid(proplists:get_value(pid, Details)),
    Mfargs = proplists:get_value(mfargs, Details),
    (get_report(State))#syslog_report{
      severity  = get_severity(informational),
      facility  = get_facility(informational, State),
      timestamp = os:timestamp(),
      pid       = get_pid(Pid),
      msg       = format("started child ~s using ~768p", [Child, Mfargs])};
format_report(_, Pid, supervisor_report, Report, State) ->
    Timestamp = os:timestamp(),
    Event = {calendar:now_to_local_time(Timestamp),
             {info_report, self(), {Pid, supervisor_report, Report}}},
    (get_report(State))#syslog_report{
      severity  = get_severity(error),
      facility  = get_facility(error, State),
      timestamp = Timestamp,
      pid       = get_pid(Pid),
      msg       = sasl_report:format_report(fd, supervisor_report, Event)};
format_report(_, Pid, syslog, [{args, A}, {fmt, F}, {severity, S} | _], State) ->
    (get_report(State))#syslog_report{
      severity  = get_severity(S),
      facility  = get_facility(S, State),
      timestamp = os:timestamp(),
      pid       = get_pid(Pid),
      msg       = format(F, A)};
format_report(Severity, Pid, _Type, Report, State) ->
    (get_report(State))#syslog_report{
      severity  = get_severity(Severity),
      facility  = get_facility(Severity, State),
      timestamp = os:timestamp(),
      pid       = get_pid(Pid),
      msg       = format(Report)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send(R = #syslog_report{msg = M}, State) ->
    send([R#syslog_report{msg = L} || L <- string:tokens(M, "\n")], State);
send(Rs, State = #state{protocol = Protocol}) when is_list(Rs) ->
    [send_datagram(Protocol:to_iolist(R), State) || R <- Rs],
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_datagram(Data, #state{socket = S, dest_host = H, dest_port = P}) ->
    ok = gen_udp:send(S, H, P, Data).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
format(Fmt, Args) ->
    try lists:flatten(io_lib:format(Fmt, Args)) of
        Msg -> Msg
    catch
        Class:Exception ->
            lists:flatten(
              io_lib:format(
                "io_lib:format(~256p, ~256p) crashed: ~32p:~512p",
                [Fmt, Args, Class, Exception]))
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
format(Report) -> format("~p", [Report]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_report(State) ->
    #syslog_report{
       hostname  = State#state.hostname,
       domain    = State#state.domain,
       appname   = State#state.appname,
       beam_pid  = State#state.beam_pid,
       bom       = State#state.bom}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_hostname()                -> get_hostname(atom_to_list(node())).
get_hostname("nonode@nohost") -> {ok, Hostname} = inet:gethostname(), Hostname;
get_hostname(Node)            -> hd(lists:reverse(string:tokens(Node, "@"))).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_domain()           -> get_domain(string:tokens(get_hostname(), ".")).
get_domain([_])        -> "";
get_domain([_ | Rest]) -> string:join(Rest, ".").

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_appname()                -> get_appname(atom_to_list(node())).
get_appname("nonode@nohost") -> "beam";
get_appname(Node)            -> hd(string:tokens(Node, "@")).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_module(rfc5424) -> syslog_rfc5424;
get_module(rfc3164) -> syslog_rfc3164.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_port(auto, _)                       -> 514;
get_port(Port, _) when is_integer(Port) -> Port.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_bom(false) -> <<>>;
get_bom(true)  -> unicode:encoding_to_bom(utf8).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_pid(N) when is_atom(N) -> atom_to_list(N);
get_pid(P) when is_pid(P)  -> get_pid(process_info(P, registered_name), P).
get_pid({registered_name, N}, _) -> atom_to_list(N);
get_pid(_, P)                    -> pid_to_list(P).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_facility(error,    #state{error_facility = F}) -> map_facility(F);
get_facility(critical, #state{error_facility = F}) -> map_facility(F);
get_facility(_,        #state{facility = F})       -> map_facility(F).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
map_facility(kernel)   -> 0;
map_facility(kern)     -> 0;
map_facility(mail)     -> 2;
map_facility(daemon)   -> 3;
map_facility(auth)     -> 4;
map_facility(syslog)   -> 5;
map_facility(lpr)      -> 6;
map_facility(news)     -> 7;
map_facility(uucp)     -> 8;
map_facility(cron)     -> 9;
map_facility(authpriv) -> 10;
map_facility(ftp)      -> 11;
map_facility(ntp)      -> 12;
map_facility(logaudit) -> 13;
map_facility(logalert) -> 14;
map_facility(clock)    -> 15;
map_facility(local0)   -> 16;
map_facility(local1)   -> 17;
map_facility(local2)   -> 18;
map_facility(local3)   -> 19;
map_facility(local4)   -> 20;
map_facility(local5)   -> 21;
map_facility(local6)   -> 22;
map_facility(local7)   -> 23.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_severity(emergency)     -> 0;
get_severity(alert)         -> 1;
get_severity(critical)      -> 2;
get_severity(error)         -> 3;
get_severity(warning)       -> 4;
get_severity(notice)        -> 5;
get_severity(informational) -> 6;
get_severity(debug)         -> 7.

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

get_appname_test() ->
    ?assertEqual("beam",     get_appname("nonode@nohost")),
    ?assertEqual("nodename", get_appname("nodename@hostname")),
    ?assertEqual("nodename", get_appname("nodename@hostname.dom.ain")).

-endif. %% TEST
