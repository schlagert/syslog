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

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts a registered gen_event manager.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> gen_event:start_link({local, ?MODULE}).

%%------------------------------------------------------------------------------
%% @doc
%% Forwards a log message to all registered gen_event handlers.
%% @end
%%------------------------------------------------------------------------------
-spec msg(syslog:severity(), pid(), string()) -> ok.
msg(Severity, Pid, Msg) ->
    gen_event:sync_notify(?MODULE, {log, Severity, Pid, Msg}).

%%%=============================================================================
%%% gen_event callbacks
%%%=============================================================================

-record(state, {}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Arg) -> {ok, #state{}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event(_, State) -> {ok, State}.

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
