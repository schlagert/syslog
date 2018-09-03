%%%=============================================================================
%%% Copyright 2013-2018, Tobias Schlager <schlagert@github.com>
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
%%% A server responsible for event handler registrations. The server will attach
%%% and re-attach event handlers at requested event managers monitoring their
%%% registration. This has the nice side-effect that as soon as this server gets
%%% shutdown the registered event handlers will be removed automatically.
%%%
%%% @see syslog_error_h
%%% @end
%%%=============================================================================
-module(syslog_monitor).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include("syslog.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Start a monitor server which in turn will attach the {@link syslog_error_h}
%% event handler at the appropriate event manager (`error_logger').
%% @end
%%------------------------------------------------------------------------------
-spec start_link(boolean()) -> {ok, pid()} | {error, term()}.
start_link(HasErrorLogger) ->
    gen_server:start_link(?MODULE, [HasErrorLogger], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-record(state, {}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([HasErrorLogger]) ->
    UseErrorLogger = syslog_lib:get_property(syslog_error_logger, true),
    {ok = add_handler(HasErrorLogger andalso UseErrorLogger), #state{}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(_Request, _From, State) -> {reply, undef, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast(_Request, State) -> {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({gen_event_EXIT, syslog_error_h, Reason}, State) ->
    %% accidential unregistration, try to re-subscribe the event handler
    ok = ?ERR("~s - handler syslog_error_h exited with ~w~n", [?MODULE, Reason]),
    ok = add_handler(true),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
add_handler(true) ->
    ok = ?ERR("~s - adding handler syslog_error_h to error_logger~n", [?MODULE]),
    gen_event:add_sup_handler(error_logger, syslog_error_h, []);
add_handler(false) ->
    ok.
