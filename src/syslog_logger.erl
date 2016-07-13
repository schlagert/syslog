%%%=============================================================================
%%% Copyright (c) 2011-2013 Basho Technologies, Inc.  All Rights Reserved.
%%% Copyright (c) 2013-2016 Tobias Schlager <schlagert@github.com>
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%
%%% @doc
%%% The main gen_event manager of the `syslog' application. All logged events
%%% will be directed through this process. This module does also define an
%%% internal event handler used for accelerate/decelerate message submission by
%%% toggling between synchronous and asynchronous gen_event notification.
%%%
%%% Since the above idea was taken from basho's
%%% <a href="https://github.com/basho/lager">lager</a> project this file was put
%%% under lager's license (Apache 2.0) and it's copyright was inherited.
%%% `lager's implementation of the same feature can be found in the module
%%% `lager_backend_throttle.erl'.
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
%%% @see syslog_logger_h
%%% @end
%%%=============================================================================
-module(syslog_logger).

-behaviour(gen_event).

%% API
-export([start_link/0,
         maybe_log/4,
         maybe_log/5,
         set_log_level/1]).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("syslog.hrl").

-define(ASYNC_LIMIT, 30).
-define(FACILITY, ?SYSLOG_FACILITY).
-define(PROTOCOL, rfc3164).

-record(opts, {
          function       :: notify | sync_notify,
          log_level      :: 0..7,
          protocol       :: module(),
          facility       :: syslog:facility(),
          crash_facility :: syslog:facility(),
          cfg            :: #syslog_cfg{}}).

%%%=============================================================================
%%% Callback Definitions (the behaviour implemented by protocol backends)
%%%=============================================================================

-callback hdr(syslog:datetime(), string(), #syslog_cfg{}) -> iodata().
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
%% Starts a registered gen_event manager.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    %% table creation will throw when event manager gets restarted
    catch ets:new(?MODULE, [named_table, public, {read_concurrency, true}]),
    gen_event:start_link({local, ?MODULE}).

%%------------------------------------------------------------------------------
%% @doc
%% Forwards a pre-formatted message directly to all registered gen_event
%% handlers. This is mainly used internally (e.g. by `syslog_error_h' or
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
%% Forwards a format message directly to all registered gen_event handlers. This
%% function never fails.
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
%% Set the log level to the given value.
%% @end
%%------------------------------------------------------------------------------
-spec set_log_level(syslog:severity()) -> ok | {error, term()}.
set_log_level(Level) -> gen_event:call(?MODULE, ?MODULE, {set_level, Level}).

%%%=============================================================================
%%% gen_event callbacks
%%%=============================================================================

-record(state, {
          async = true   :: boolean(),
          log_level      :: 0..7,
          async_limit    :: pos_integer() | infinity}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Arg) ->
    Opts = new_opts(syslog_lib:get_property(log_level, ?SYSLOG_LOGLEVEL)),
    Limit = erlang:max(1, syslog_lib:get_property(async_limit, ?ASYNC_LIMIT)),
    State = #state{async_limit = Limit, log_level = Opts#opts.log_level},
    {ok, set_fun(set_opts(Opts, State))}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event({log, _}, State = #state{async_limit = AsyncLimit})
  when AsyncLimit =/= infinity ->
    {message_queue_len, QueueLen} = process_info(self(), message_queue_len),
    case {QueueLen > AsyncLimit, State#state.async} of
        {true, true} ->
            {ok, set_fun(State#state{async = false})};
        {false, false} ->
            {ok, set_fun(State#state{async = true})};
        _ ->
            {ok, State}
    end;
handle_event(_, State) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call({set_level, Level}, State = #state{log_level = OldLevel}) ->
    case catch map_severity(Level) of
        I when is_integer(I), Level =/= OldLevel ->
            {ok, ok, set_level(State#state{log_level = I})};
        I when is_integer(I) ->
            {ok, ok, State};
        _ ->
            {ok, {error, {bad_log_level, Level}}, State}
    end;
handle_call(_Request, State) ->
    {ok, undef, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(_Info, State) -> {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Arg, #state{}) -> ok.

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
log(Severity, Pid, Timestamp, Msg, Opts) ->
    PRI = pri(Severity, Opts),
    HDR = hdr(Pid, Timestamp, Opts),
    lists:foreach(
      fun(<<>>) ->
              ok;
         (Message) ->
              forward(PRI, HDR, msg(Message, Opts), Opts)
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
%% The actual forwarding of the message to the `gen_event' manager.
%%------------------------------------------------------------------------------
forward(PRI, HDR, MSG, #opts{function = Fun}) ->
    try
        gen_event:Fun(?MODULE, {log, iolist_to_binary([PRI, HDR, MSG])})
    catch
        exit:_ -> ?ERR("~ts~n", [MSG])
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
new_opts(Level) ->
    #opts{
       function = notify,
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
       beam_pid = os:getpid(),
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
%%------------------------------------------------------------------------------
set_fun(State = #state{async = false}) ->
    set_fun(sync_notify, State);
set_fun(State = #state{async = true}) ->
    set_fun(notify, State).
set_fun(Fun, State) ->
    true = ets:update_element(?MODULE, opts, {#opts.function, Fun}),
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_level(State = #state{log_level = Level}) ->
    true = ets:update_element(?MODULE, opts, {#opts.log_level, Level}),
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
get_bom()           -> get_bom(syslog_lib:get_property(use_rfc5424_bom, false)).
get_bom({ok, true}) -> unicode:encoding_to_bom(utf8);
get_bom(_)          -> <<>>.
