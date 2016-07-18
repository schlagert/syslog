%%%=============================================================================
%%% Copyright (c) 2013-2016 Tobias Schlager <schlagert@github.com>
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
%%% The main (backend) server of the `syslog' application. All logged events
%%% will be directed into this process. The process itself is pretty simple, all
%%% it does is sending incoming `iodata()' to the configured destination.
%%%
%%% This module also performs the formatting of messages into the configured
%%% protocol format. Note that this is completely done in the calling (logging)
%%% process and only the formatted binary message is forwarded to the event
%%% manager and its handlers. While this may slow down a single logging process,
%%% it evenly distributes the formatting load when there are many concurrent
%%% loggings.
%%%
%%% Protocol backends must implement the {@link syslog_error} behaviour.
%%%
%%% @see syslog_rfc3164
%%% @see syslog_rfc5424
%%% @end
%%%=============================================================================
-module(syslog_logger).

-behaviour(gen_server).

%% API
-export([start_link/0,
         maybe_log/4,
         maybe_log/5,
         set_log_level/1,
         set_log_mode/1]).

%% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include("syslog.hrl").

-define(ASYNC, false).
-define(DEST_HOST, {127, 0, 0, 1}).
-define(DEST_PORT, 514).
-define(FACILITY, ?SYSLOG_FACILITY).
-define(PROTOCOL, rfc3164).
-define(TIMEOUT, 1000).

-record(opts, {
          function       :: cast | {call, pos_integer()},
          log_level      :: 0..7,
          protocol       :: module(),
          facility       :: syslog:facility(),
          crash_facility :: syslog:facility(),
          cfg            :: #syslog_cfg{}}).

%%%=============================================================================
%%% Callback Definitions (the behaviour implemented by protocol backends)
%%%=============================================================================

-callback hdr(syslog:datetime(), binary(), #syslog_cfg{}) -> iodata().
%% @doc
%% Format a header this should include everything including structured data but
%% excluding the PRI part.
%% @end

-callback msg(binary(), #syslog_cfg{}) -> binary().
%% @doc
%% Format the message part (if necessary).
%% @end

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts a registered gen_server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    %% table creation will throw when server gets restarted
    catch ets:new(?MODULE, [named_table, public, {read_concurrency, true}]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% @doc
%% Forwards a pre-formatted message.
%%
%% This is mainly used internally (e.g. by `syslog_error_h' or
%% `syslog_lager_backend'). This function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_log(syslog:severity(),
                syslog:proc_name(),
                erlang:timestamp(),
                iodata()) -> ok.
maybe_log(Severity, Pid, Timestamp, Msg) ->
    Opts = #opts{log_level = Level} = get_opts(),
    case map_severity(Severity) of
        SeverityInt when SeverityInt =< Level ->
            log(SeverityInt, Pid, Timestamp, Msg, Opts);
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Forwards a format message. This function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_log(syslog:severity(),
                syslog:proc_name(),
                erlang:timestamp(),
                io:format(),
                [term()]) -> ok.
maybe_log(Severity, Pid, Timestamp, Fmt, Args) ->
    Opts = #opts{log_level = Level} = get_opts(),
    case map_severity(Severity) of
        SeverityInt when SeverityInt =< Level ->
            try
                Msg = io_lib:format(Fmt, Args),
                log(SeverityInt, Pid, Timestamp, Msg, Opts)
            catch
                C:E -> ?ERR("io_lib:format(~p,~p) failed (~p:~p)~n",
                            [Fmt, Args, C, E])
            end;
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Change the log level to the given value.
%% @end
%%------------------------------------------------------------------------------
-spec set_log_level(syslog:severity()) -> ok | {error, term()}.
set_log_level(Level) ->
    case catch map_severity(Level) of
        I when is_integer(I) ->
            case ets:update_element(?MODULE, opts, {#opts.log_level, I}) of
                true ->
                    syslog_lager_backend:set_log_level(Level);
                false ->
                    {error, {not_running, syslog}}
            end;
        _ ->
            {error, {bad_log_level, Level}}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Change the log mode to the given mode.
%% @end
%%------------------------------------------------------------------------------
-spec set_log_mode(async | sync | {sync, pos_integer()}) ->
                          ok | {error, term()}.
set_log_mode(async) ->
    set_log_function(get_function(true));
set_log_mode(sync) ->
    set_log_function(get_function(false));
set_log_mode({sync, Timeout}) when is_integer(Timeout), Timeout > 0 ->
    set_log_function({call, Timeout}).
set_log_function(Function) ->
    case ets:update_element(?MODULE, opts, {#opts.function, Function}) of
        true  -> ok;
        false -> {error, {not_running, syslog}}
    end.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-record(state, {
          socket    :: inet:socket(),
          dest_host :: inet:ip_address() | inet:hostname(),
          dest_port :: inet:port_number()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    Opts = new_opts(syslog_lib:get_property(log_level, ?SYSLOG_LOGLEVEL)),
    {ok, Socket} = gen_udp:open(0, [binary]),
    State = #state{
               socket = Socket,
               dest_host = syslog_lib:get_property(dest_host, ?DEST_HOST),
               dest_port = syslog_lib:get_property(dest_port, ?DEST_PORT)},
    {ok, set_opts(Opts, State)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call({log, Msg}, _From, State) ->
    {reply, ok, send(Msg, State)};
handle_call(_Request, _From, State) ->
    {reply, undef, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({log, Msg}, State) -> {noreply, send(Msg, State)};
handle_cast(_Request, State)   -> {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({udp_closed, S}, State = #state{socket = S}) ->
    {stop, {error, {udp_closed, S}}, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, #state{socket = Socket}) -> gen_udp:close(Socket).

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
send(Data, State = #state{socket = S, dest_host = H, dest_port = P}) ->
    ok = gen_udp:send(S, H, P, Data),
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log(Severity, Pid, Timestamp, Msg, Opts) ->
    PRI = pri(Severity, Opts),
    HDR = hdr(Pid, Timestamp, Opts),
    lists:foreach(
      fun(<<>>) ->
              ok;
         (Message) ->
              MSG = msg(Message, Opts),
              forward(Msg, iolist_to_binary([PRI, HDR, MSG]), Opts)
      end, binary:split(iolist_to_binary(Msg), [<<"\n">>, <<"\r">>], [global])).

%%------------------------------------------------------------------------------
%% @private
%% Perform the PRI calculation. In case of severity `crash' the facility will be
%% the configured `crash_facility' and severity is set to `error'.
%%------------------------------------------------------------------------------
pri(?SYSLOG_CRASH, #opts{crash_facility = Facility}) ->
    [$<, integer_to_list((map_facility(Facility) bsl 3) + ?SYSLOG_ERROR), $>];
pri(Severity, #opts{facility = Facility}) ->
    [$<, integer_to_list((map_facility(Facility) bsl 3) + Severity), $>].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
hdr(Pid, Timestamp, #opts{protocol = Protocol, cfg = Cfg}) ->
    Datetime = syslog_lib:get_utc_datetime(Timestamp),
    Protocol:hdr(Datetime, syslog_lib:get_pid(Pid), Cfg).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
msg(Msg, #opts{protocol = Protocol, cfg = Cfg}) -> Protocol:msg(Msg, Cfg).

%%------------------------------------------------------------------------------
%% @private
%% The actual forwarding of the message to the `gen_server'.
%%------------------------------------------------------------------------------
forward(Msg, Binary, #opts{function = {call, Timeout}}) ->
    try
        gen_server:call(?MODULE, {log, Binary}, Timeout)
    catch
        exit:{noproc, _}  -> ?ERR("~ts~n", [Msg]);
        exit:{timeout, _} -> ok %% message has been placed in mailbox
    end;
forward(_Msg, Binary, #opts{function = cast}) ->
    gen_server:cast(?MODULE, {log, Binary}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
new_opts(Level) ->
    #opts{
       function = get_function(syslog_lib:get_property(async, ?ASYNC)),
       log_level = map_severity(Level),
       protocol = get_protocol(),
       facility = syslog_lib:get_property(facility, ?FACILITY),
       crash_facility = syslog_lib:get_property(crash_facility, ?FACILITY),
       cfg = new_cfg()}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
new_cfg() ->
    #syslog_cfg{
       hostname = syslog_lib:get_hostname(),
       domain = syslog_lib:get_domain(),
       appname = syslog_lib:get_name(),
       beam_pid = list_to_binary(os:getpid()),
       bom = get_bom()}.

%%------------------------------------------------------------------------------
%% @private
%% If, for some reason, the ETS table cannot be read, a default `opts' structure
%% will be built and returned.
%%------------------------------------------------------------------------------
-spec get_opts() -> #opts{}.
get_opts() ->
    try
        hd(ets:lookup(?MODULE, opts))
    catch
        error:badarg -> new_opts(?SYSLOG_LOGLEVEL)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_opts(Opts = #opts{}, State) ->
    true = ets:insert(?MODULE, Opts),
    State.

%%------------------------------------------------------------------------------
%% @private
%% Note that `crash' is a fake severity which will become `error' in the final
%% translation in {@link pri/2}.
%%------------------------------------------------------------------------------
map_severity(crash)         -> ?SYSLOG_CRASH;
map_severity(emergency)     -> ?SYSLOG_EMERGENCY;
map_severity(alert)         -> ?SYSLOG_ALERT;
map_severity(critical)      -> ?SYSLOG_CRITICAL;
map_severity(error)         -> ?SYSLOG_ERROR;
map_severity(warning)       -> ?SYSLOG_WARNING;
map_severity(notice)        -> ?SYSLOG_NOTICE;
map_severity(informational) -> ?SYSLOG_INFO;
map_severity(debug)         -> ?SYSLOG_DEBUG.

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
get_protocol() ->
    case syslog_lib:get_property(protocol, ?PROTOCOL) of
        rfc5424 -> syslog_rfc5424;
        rfc3164 -> syslog_rfc3164
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_function(true)  -> cast;
get_function(false) -> {call, syslog_lib:get_property(timeout, ?TIMEOUT)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_bom()           -> get_bom(syslog_lib:get_property(use_rfc5424_bom, false)).
get_bom({ok, true}) -> unicode:encoding_to_bom(utf8);
get_bom(_)          -> <<>>.
