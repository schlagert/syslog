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
         msg/3]).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("syslog.hrl").

-define(ASYNC_LIMIT, 30).

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
%% Forwards a log message to all registered gen_event handlers.
%% @end
%%------------------------------------------------------------------------------
-spec msg(syslog:severity(), pid() | atom(), binary()) -> ok.
msg(Severity, Pid, Msg) ->
    Fun = get_fun(),
    PidStr = syslog_lib:get_pid(Pid),
    SeverityInt = map_severity(Severity),
    gen_event:Fun(?MODULE, {log, os:timestamp(), SeverityInt, PidStr, Msg}).

%%%=============================================================================
%%% gen_event callbacks
%%%=============================================================================

-record(state, {
          async = true  :: boolean(),
          async_limit   :: pos_integer() | infinity}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Arg) ->
    AsyncLimit = syslog_lib:get_property(async_limit, ?ASYNC_LIMIT),
    {ok, set_fun(async, #state{async_limit = erlang:max(1, AsyncLimit)})}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event({log, _, _, _, _}, State = #state{async_limit = AsyncLimit})
  when AsyncLimit =/= infinity ->
    {message_queue_len, QueueLen} = process_info(self(), message_queue_len),
    case {QueueLen > AsyncLimit, State#state.async} of
        {true, true} ->
            {ok, set_fun(sync, State#state{async = false})};
        {false, false} ->
            {ok, set_fun(async, State#state{async = true})};
        _ ->
            {ok, State}
    end;
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
get_fun() -> ets:lookup_element(?MODULE, function, 2).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_fun(sync,  State) -> set_fun(sync_notify), State;
set_fun(async, State) -> set_fun(notify), State.
set_fun(Fun)          -> true = ets:insert(?MODULE, {function, Fun}).

%%------------------------------------------------------------------------------
%% @private
%% Note that `crash' is fake severity which will be `error' after the final
%% translation in `syslog_logger_h'.
%%------------------------------------------------------------------------------
map_severity(emergency)     -> ?SYSLOG_EMERGENCY;
map_severity(alert)         -> ?SYSLOG_ALERT;
map_severity(critical)      -> ?SYSLOG_CRITICAL;
map_severity(error)         -> ?SYSLOG_ERROR;
map_severity(warning)       -> ?SYSLOG_WARNING;
map_severity(notice)        -> ?SYSLOG_NOTICE;
map_severity(informational) -> ?SYSLOG_INFO;
map_severity(debug)         -> ?SYSLOG_DEBUG;
map_severity(crash)         -> ?SYSLOG_CRASH.
