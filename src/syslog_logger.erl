%%%=============================================================================
%%% Copyright (c) 2013-2018 Tobias Schlager <schlagert@github.com>
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
         log/6,
         log/7,
         async_log/6,
         async_log/7,
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
-define(TRANSFORM, long).
-define(TRANSPORT, udp).
-define(TIMEOUT, 1000).
-define(MULTILINE, false).

-define(SEPARATORS, [<<"\n">>, <<"\r">>]).
-define(TCP_OPTS, [{active, false},
                   {keepalive, true},
                   {reuseaddr, true},
                   {send_timeout, ?TIMEOUT},
                   {send_timeout_close, true}]).

-record(opts, {
          function       :: cast | {call, pos_integer()},
          log_level      :: 0..7,
          protocol       :: module(),
          facility       :: syslog:facility(),
          crash_facility :: syslog:facility(),
          multiline      :: boolean(),
          cfg            :: #syslog_cfg{}}).

%%%=============================================================================
%%% Callback Definitions (the behaviour implemented by protocol backends)
%%%=============================================================================

-callback hostname(string()) -> string().
%% @doc
%% Value of hostname normalised for use by this protocol.
%% @end

-callback hdr(syslog:datetime(), binary(), #syslog_cfg{}) -> iodata().
%% @doc
%% Format a header this should not include the PRI part of the header.
%% @end

-callback msg([syslog:sd_element()], binary(), #syslog_cfg{}) -> binary().
%% @doc
%% Format the message part which consists of structured data and the actual
%% free-form message (if necessary).
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
%% Forwards a message. This function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec log(syslog:severity(),
          syslog:proc_name(),
          erlang:timestamp() | pos_integer(),
          [syslog:sd_element()],
          io:format() | iolist(),
          [term()] | no_format) -> ok.
log(Severity, Pid, Timestamp, SD, Fmt, Args) ->
    maybe_log(Severity, Pid, Timestamp, SD, Fmt, Args, get_opts()).

%%------------------------------------------------------------------------------
%% @doc
%% Forwards a message. This function never fails. Allows overriding of
%% config options.
%% @end
%%------------------------------------------------------------------------------
-spec log(syslog:severity(),
          syslog:proc_name(),
          erlang:timestamp() | pos_integer(),
          [syslog:sd_element()],
          io:format() | iolist(),
          [term()] | no_format,
          proplists:proplist()) -> ok.
log(Severity, Pid, Timestamp, SD, Fmt, Args, Overrides) ->
    Opts = apply_cfg_overrides(get_opts(), Overrides),
    maybe_log(Severity, Pid, Timestamp, SD, Fmt, Args, Opts).

%%------------------------------------------------------------------------------
%% @doc
%% Forwards a message asynchronously. This function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec async_log(syslog:severity(),
                syslog:proc_name(),
                erlang:timestamp() | pos_integer(),
                [syslog:sd_element()],
                io:format() | iolist(),
                [term()] | no_format) -> ok.
async_log(Severity, Pid, Timestamp, SD, Fmt, Args) ->
    AsyncOpts = (get_opts())#opts{function = get_function(true)},
    maybe_log(Severity, Pid, Timestamp, SD, Fmt, Args, AsyncOpts).

%%------------------------------------------------------------------------------
%% @doc
%% Forwards a message asynchronously. This function never fails. Allows
%% overriding of config options.
%% @end
%%------------------------------------------------------------------------------
-spec async_log(syslog:severity(),
                syslog:proc_name(),
                erlang:timestamp() | pos_integer(),
                [syslog:sd_element()],
                io:format() | iolist(),
                [term()] | no_format,
                proplists:proplist()) -> ok.
async_log(Severity, Pid, Timestamp, SD, Fmt, Args, Overrides) ->
    AsyncOpts = (get_opts())#opts{function = get_function(true)},
    NewAsyncOpts = apply_cfg_overrides(AsyncOpts, Overrides),
    maybe_log(Severity, Pid, Timestamp, SD, Fmt, Args, NewAsyncOpts).

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

-type device() :: standard_io |
                  standard_error |
                  {module(), inet:socket() | io:device()}.

-record(state, {
          device             :: device() | undefined,
          timer = make_ref() :: reference(),
          protocol           :: atom() | tuple(),
          dest_host          :: inet:ip_address() | inet:hostname(),
          dest_port          :: inet:port_number()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    %% avoid excessive garbage collection, use apply for dialyzer
    catch apply(erlang, process_flag, [message_queue_data, off_heap]),
    State = #state{
               protocol = syslog_lib:get_property(protocol, ?PROTOCOL),
               dest_host = syslog_lib:get_property(dest_host, ?DEST_HOST, ip_addr),
               dest_port = syslog_lib:get_property(dest_port, ?DEST_PORT, integer)},
    LogLevel = syslog_lib:get_property(log_level, ?SYSLOG_LOGLEVEL),
    {ok, set_opts(LogLevel, init_transport(State))}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call({log, Msg}, _From, State) -> {reply, ok, send(Msg, State)};
handle_call(_Request, _From, State)   -> {reply, undef, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({log, Msg}, State) -> {noreply, send(Msg, State)};
handle_cast(_Request, State)   -> {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({timeout, Ref, open_device}, State = #state{timer = Ref}) ->
    {noreply, init_transport(State)};
handle_info({udp_closed, S}, State = #state{device = {_, S}}) ->
    Error = {error, {udp_closed, S}},
    {noreply, ensure_transport(0, Error, State#state{device = undefined})};
handle_info({tcp_closed, S}, State = #state{device = {_, S}}) ->
    Error = {error, {tcp_closed, S}},
    {noreply, ensure_transport(0, Error, State#state{device = undefined})};
handle_info({tcp_error, S}, State = #state{device = {_, S}}) ->
    Error = {error, {tcp_error, S}},
    {noreply, ensure_transport(0, Error, State)};
handle_info({ssl_closed, S}, State = #state{device = {_, S}}) ->
    Error = {error, {ssl_closed, S}},
    {noreply, ensure_transport(0, Error, State#state{device = undefined})};
handle_info({ssl_error, S}, State = #state{device = {_, S}}) ->
    Error = {error, {ssl_error, S}},
    {noreply, ensure_transport(0, Error, State)};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, State) -> close_transport(State).

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
init_transport(State = #state{protocol = Protocol}) ->
    open_device(get_transport(Protocol), State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
close_transport(#state{device = {Module, Device}}) -> Module:close(Device);
close_transport(#state{})                          -> ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
ensure_transport(_N, ok, State) ->
    State;
ensure_transport(N, Error, State) ->
    ?ERR("~s - Message transport failed with ~w~n", [?MODULE, Error]),
    close_transport(State),
    ?ERR("~s - DROPPED ~w messages~n", [?MODULE, flush_msg_queue(N)]),
    init_transport(State#state{device = undefined}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
open_device({udp, Opts}, State) ->
    open_device_impl(gen_udp, open, [0, Opts], State);
open_device({tcp, Opts}, State = #state{dest_host = H, dest_port = P}) ->
    open_device_impl(gen_tcp, connect, [H, P, Opts, ?TIMEOUT], State);
open_device({tls, Opts}, State = #state{dest_host = H, dest_port = P}) ->
    open_device_impl(ssl, connect, [H, P, Opts, ?TIMEOUT], State);
open_device({File, Opts}, State) when is_list(File); is_binary(File) ->
    open_device_impl(file, open, [File, [append | Opts]], State);
open_device({IoDevice, []}, State)
  when IoDevice =:= standard_io; IoDevice =:= standard_error ->
    State#state{device = IoDevice}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
open_device_impl(Module, Fun, Args, State) ->
    case erlang:apply(Module, Fun, Args) of
        {ok, Device} ->
            State#state{device = {Module, Device}};
        Error ->
            ?ERR("~s - Failed to open transport ~w~n", [?MODULE, Error]),
            Time = syslog_lib:get_property(retry_timeout, ?TIMEOUT, integer),
            State#state{timer = erlang:start_timer(Time, self(), open_device)}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send(_Data, State = #state{device = undefined}) ->
    State;
send(Data, State = #state{device = {gen_udp, S}, dest_host = H, dest_port = P}) ->
    ensure_transport(1, gen_udp:send(S, H, P, Data), State);
send(Data, State = #state{device = {gen_tcp, Socket}}) ->
    Frame = [integer_to_list(size(Data)), $\s, Data],
    ensure_transport(1, gen_tcp:send(Socket, Frame), State);
send(Data, State = #state{device = {ssl, Socket}}) ->
    Frame = [integer_to_list(size(Data)), $\s, Data],
    ensure_transport(1, ssl:send(Socket, Frame), State);
send(Data, State = #state{device = {file, IoDevice}}) ->
    ensure_transport(1, file:write(IoDevice, [Data, $\n]), State);
send(Data, State = #state{device = IoDevice})
  when IoDevice =:= standard_io; IoDevice =:= standard_error ->
    ensure_transport(1, io:fwrite(IoDevice, [Data, $\n]), State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_log(Severity, Pid, Timestamp, SD, Fmt, Args, Opts) ->
    case {map_severity(Severity), Args} of
        {SeverityInt, _Args} when SeverityInt > Opts#opts.log_level ->
            ok;
        {SeverityInt, no_format} ->
            do_log(SeverityInt, Pid, Timestamp, SD, Fmt, Opts);
        {SeverityInt, Args} ->
            try io_lib:format(Fmt, Args) of
                Msg -> do_log(SeverityInt, Pid, Timestamp, SD, Msg, Opts)
            catch
                C:E -> ?ERR("~s - io_lib:format(~p,~p) failed (~p:~p)~n",
                            [?MODULE, Fmt, Args, C, E])
            end
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_log(Severity, Pid, Timestamp, StructuredData, Msg, Opts) ->
    PRI = pri(Severity, Opts),
    HDR = hdr(Pid, Timestamp, Opts),
    MSG = unicode:characters_to_binary(Msg),
    Fun = do_log_fun(PRI, HDR, StructuredData, Opts),
    case Opts#opts.multiline of
        true  -> Fun(MSG);
        false -> lists:foreach(Fun, binary:split(MSG, ?SEPARATORS, [global]))
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_log_fun(PRI, HDR, StructuredData, Opts) ->
    fun(<<>>) when StructuredData =:= [] ->
            ok;
       (Msg) ->
            case msg(StructuredData, Msg, Opts) of
                <<>> -> ok;
                MSG  -> forward(Msg, iolist_to_binary([PRI, HDR, MSG]), Opts)
            end
    end.

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
msg(StructuredData, Msg, #opts{protocol = Protocol, cfg = Cfg}) ->
    Protocol:msg(StructuredData, Msg, Cfg).

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
new_opts(Level, Protocol) ->
    ProtocolModule = get_protocol(Protocol),
    #opts{
       function = get_function(syslog_lib:get_property(async, ?ASYNC)),
       log_level = map_severity(Level),
       protocol = ProtocolModule,
       facility = syslog_lib:get_property(facility, ?FACILITY),
       crash_facility = syslog_lib:get_property(crash_facility, ?FACILITY),
       multiline = syslog_lib:get_property(multiline_mode, ?MULTILINE),
       cfg = new_cfg(ProtocolModule)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
new_cfg(Protocol) ->
    Transform = syslog_lib:get_property(hostname_transform, ?TRANSFORM),
    #syslog_cfg{
       hostname = Protocol:hostname(syslog_lib:get_hostname(Transform)),
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
        error:badarg -> new_opts(?SYSLOG_LOGLEVEL, ?PROTOCOL)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_opts(LogLevel, State = #state{protocol = Protocol}) ->
    true = ets:insert(?MODULE, new_opts(LogLevel, Protocol)),
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
flush_msg_queue(N) ->
    receive {log, _} -> flush_msg_queue(N + 1) after 0 -> N end.

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
map_severity(info)          -> ?SYSLOG_INFO;
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
get_protocol({P, _, _}) when is_atom(P) -> get_protocol(P);
get_protocol({P, _}) when is_atom(P)    -> get_protocol(P);
get_protocol(rfc5424)                   -> syslog_rfc5424;
get_protocol(rfc3164)                   -> syslog_rfc3164;
get_protocol(P) when is_atom(P)         -> P.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_transport({rfc5424, tls, []})   -> throw({tls_options_missing});
get_transport({rfc5424, tls, Opts}) -> {tls, [{active, false} | Opts]};
get_transport({rfc5424, tls})       -> throw({tls_options_missing});
get_transport({_, tcp, Opts})       -> {tcp, [{active, false} | Opts]};
get_transport({_, tcp})             -> {tcp, ?TCP_OPTS};
get_transport({_, udp, Opts})       -> {udp, [{active, false} | Opts]};
get_transport({_, udp})             -> {udp, [{active, false}]};
get_transport({_, T, Opts})         -> {T, Opts};
get_transport({_, T})               -> {T, []};
get_transport(P) when is_atom(P)    -> get_transport({P, ?TRANSPORT}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_function(true)  -> cast;
get_function(false) -> {call, syslog_lib:get_property(timeout, ?TIMEOUT, integer)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_bom()           -> get_bom(syslog_lib:get_property(use_rfc5424_bom, false)).
get_bom({ok, true}) -> get_bom(true);
get_bom(true)       -> unicode:encoding_to_bom(utf8);
get_bom(_)          -> <<>>.

%%------------------------------------------------------------------------------
%% @private
%% Applying user provided overrides to the outgoing messages. This allows the
%% user to change the default values for any of the syslog cfg values per
%% message.
%%
%% Since these values are provided by the user they have to checked to sure
%% they are in the correct format before sending.
%%------------------------------------------------------------------------------
apply_cfg_overrides(Opts = #opts{cfg = Cfg}, Overrides) ->
    Opts#opts{cfg = apply_cfg_overrides(Cfg, Overrides)};
apply_cfg_overrides(Cfg = #syslog_cfg{}, []) ->
    Cfg;
apply_cfg_overrides(Cfg = #syslog_cfg{}, [{hostname, HostName} | T])
  when is_list(HostName) ->
    apply_cfg_overrides(Cfg#syslog_cfg{hostname = HostName}, T);
apply_cfg_overrides(Cfg = #syslog_cfg{}, [{appname, AppName} | T]) ->
    AppName1 = syslog_lib:to_type(binary, AppName),
    apply_cfg_overrides(Cfg#syslog_cfg{appname = AppName1}, T);
apply_cfg_overrides(Cfg = #syslog_cfg{}, [{beam_pid, BeamPid} | T]) ->
    BeamPid1 = syslog_lib:to_type(binary, BeamPid),
    apply_cfg_overrides(Cfg#syslog_cfg{beam_pid = BeamPid1}, T);
apply_cfg_overrides(Cfg = #syslog_cfg{}, [{bom, Bom} | T])
  when is_boolean(Bom) ->
    apply_cfg_overrides(Cfg#syslog_cfg{bom = get_bom(Bom)}, T).
