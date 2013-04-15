%%%=============================================================================
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
%%% The event handler to be attached to the `syslog_logger' event manager.
%%% This handler will convert the incoming messages to `syslog_report's which
%%% will then be converted to Syslog packets using the configured protocol
%%% backend and finally send the binaries over the wire.
%%%
%%% Protocol backends must implement the {@link syslog_error_h} behaviour.
%%%
%%% @see syslog_monitor
%%% @see syslog_rfc3164
%%% @see syslog_rfc5424
%%% @end
%%%=============================================================================
-module(syslog_logger_h).

-behaviour(gen_event).

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
%%% gen_event callbacks
%%%=============================================================================

-record(state, {
          socket          :: gen_udp:socket(),
          protocol        :: module(),
          facility        :: syslog:facility(),
          error_facility  :: syslog:facility(),
          dest_host       :: inet:ip_address() | inet:hostname(),
          dest_port       :: inet:port_number(),
          hostname        :: string(),
          domain          :: string(),
          appname         :: string(),
          beam_pid        :: string(),
          bom             :: binary()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Arg) ->
    {ok, Socket} = gen_udp:open(0, [binary, {reuseaddr, true}]),
    {ok, #state{
            socket          = Socket,
            protocol        = get_protocol(),
            facility        = syslog_lib:get_property(facility, ?FACILITY),
            error_facility  = syslog_lib:get_property(error_facility, ?FACILITY),
            dest_host       = syslog_lib:get_property(dest_host, ?DEST_HOST),
            dest_port       = syslog_lib:get_property(dest_port, ?DEST_PORT),
            hostname        = syslog_lib:get_hostname(),
            domain          = syslog_lib:get_domain(),
            appname         = syslog_lib:get_name(),
            beam_pid        = os:getpid(),
            bom             = get_bom()}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event({log, Severity, Pid, Msg}, State) ->
    {ok, send(get_report(Severity, Pid, Msg, State), State)};
handle_event(_, State) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(_Request, State) -> {ok, undef, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({udp_closed, S}, #state{socket = S}) -> {error, {udp_closed, S}};
handle_info(_Info, State)                        -> {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Arg, #state{socket = Socket}) -> gen_udp:close(Socket).

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
get_report(Severity, Pid, Msg, State) ->
    #syslog_report{
       severity  = map_severity(Severity),
       facility  = severity_to_facility(Severity, State),
       timestamp = os:timestamp(),
       pid       = syslog_lib:get_pid(Pid),
       hostname  = State#state.hostname,
       domain    = State#state.domain,
       appname   = State#state.appname,
       beam_pid  = State#state.beam_pid,
       bom       = State#state.bom,
       msg       = Msg}.

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
get_protocol()        -> get_protocol(syslog_lib:get_property(protocol, ?PROTOCOL)).
get_protocol(rfc5424) -> syslog_rfc5424;
get_protocol(rfc3164) -> syslog_rfc3164.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_bom()           -> get_bom(syslog_lib:get_property(use_rfc5424_bom, false)).
get_bom({ok, true}) -> unicode:encoding_to_bom(utf8);
get_bom(_)          -> <<>>.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
severity_to_facility(emergency, #state{error_facility = F}) -> map_facility(F);
severity_to_facility(alert,     #state{error_facility = F}) -> map_facility(F);
severity_to_facility(critical,  #state{error_facility = F}) -> map_facility(F);
severity_to_facility(error,     #state{error_facility = F}) -> map_facility(F);
severity_to_facility(_,         #state{facility = F})       -> map_facility(F).

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
map_severity(emergency)     -> 0;
map_severity(alert)         -> 1;
map_severity(critical)      -> 2;
map_severity(error)         -> 3;
map_severity(warning)       -> 4;
map_severity(notice)        -> 5;
map_severity(informational) -> 6;
map_severity(debug)         -> 7.
