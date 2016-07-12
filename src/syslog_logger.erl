%%%=============================================================================
%%% Copyright (c) 2011-2013 Basho Technologies, Inc.  All Rights Reserved.
%%% Copyright (c) 2013 Tobias Schlager <schlagert@github.com>
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

-record(opts, {function :: notify | sync_notify, log_level :: 0..7}).

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
maybe_log(Severity, Pid, Timestamp, Message) ->
    Opts = #opts{log_level = Level} = get_opts(),
    case map_severity(Severity) of
        SeverityInt when SeverityInt =< Level ->
            Msg = iolist_to_binary(Message),
            forward(SeverityInt, Pid, Timestamp, Msg, Opts);
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
                string(),
                [term()]) -> ok.
maybe_log(Severity, Pid, Timestamp, Fmt, Args) ->
    Opts = #opts{log_level = Level} = get_opts(),
    case map_severity(Severity) of
        SeverityInt when SeverityInt =< Level ->
            try
                Msg = iolist_to_binary(io_lib:format(Fmt, Args)),
                forward(SeverityInt, Pid, Timestamp, Msg, Opts)
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
          async = true :: boolean(),
          log_level    :: 0..7,
          async_limit  :: pos_integer() | infinity}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Arg) ->
    Limit = erlang:max(1, syslog_lib:get_property(async_limit, ?ASYNC_LIMIT)),
    Level = map_severity(syslog_lib:get_property(log_level, ?SYSLOG_LOGLEVEL)),
    {ok, set_opts(#state{async_limit = Limit, log_level = Level})}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event({log, _, _, _, _}, State = #state{async_limit = AsyncLimit})
  when AsyncLimit =/= infinity ->
    {message_queue_len, QueueLen} = process_info(self(), message_queue_len),
    case {QueueLen > AsyncLimit, State#state.async} of
        {true, true} ->
            {ok, set_opts(State#state{async = false})};
        {false, false} ->
            {ok, set_opts(State#state{async = true})};
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
            {ok, ok, set_opts(State#state{log_level = I})};
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
forward(SeverityInt, Pid, Timestamp, Msg, #opts{function = Fun}) ->
    try
        PidStr = syslog_lib:get_pid(Pid),
        Datetime = syslog_lib:get_utc_datetime(Timestamp),
        gen_event:Fun(?MODULE, {log, Datetime, SeverityInt, PidStr, Msg})
    catch
        _:_ -> ?ERR("~s~n", [Msg])
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec get_opts() -> #opts{}.
get_opts() -> hd(ets:lookup(?MODULE, opts)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_opts(State = #state{async = false}) ->
    set_opts(sync_notify, State);
set_opts(State = #state{async = true}) ->
    set_opts(notify, State).
set_opts(Fun, State = #state{log_level = Level}) ->
    true = ets:insert(?MODULE, #opts{function = Fun, log_level = Level}),
    State.

%%------------------------------------------------------------------------------
%% @private
%% Note that `crash' is a fake severity which will become `error' after the
%% final translation in `syslog_logger_h'.
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
