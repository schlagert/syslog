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
%%% A server responsible for event handler registrations. The server will attach
%%% and re-attach event handlers at requested event managers monitoring their
%%% registration. This has the nice side-effect that as soon as this server gets
%%% shutdown the registered event handlers will be removed automatically.
%%%
%%% @see syslog_error_h
%%% @see syslog_logger_h
%%% @end
%%%=============================================================================
-module(syslog_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         code_change/3,
         terminate/2]).

-define(REGISTRATIONS,
        [
         %% Manager      Handler
         {error_logger,  syslog_error_h},
         {syslog_logger, syslog_logger_h}
        ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Start a monitor server which in turn will attach the {@link syslog_error_h}
%% and {@link syslog_logger_h} at the appropriate event managers (`error_logger'
%% and `syslog_logger'.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> gen_server:start_link(?MODULE, [], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-record(state, {}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    [ok = gen_event:add_sup_handler(M, H, []) || {M, H} <- ?REGISTRATIONS],
    {ok, #state{}}.

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
handle_info({gen_event_EXIT, {_EventHandler, _EventMgr}, shutdown}, State) ->
    %% the respective event manager was shutdown properly, we can also RIP
    {stop, normal, State};
handle_info({gen_event_EXIT, {EventHandler, EventMgr}, _}, State) ->
    %% accidential unregistration, try to re-subscribe the event handler
    ok = gen_event:add_sup_handler(EventMgr, EventHandler, []),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _State) -> ok.
