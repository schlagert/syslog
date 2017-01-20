%%%=============================================================================
%%% Copyright 2013-2017, Tobias Schlager <schlagert@github.com>
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
         %% Manager     Handler
         {error_logger, syslog_error_h}
        ]).

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
    UseErrLogger = syslog_lib:get_property(syslog_error_logger, true),
    Regs = [R || R = {M, _} <- ?REGISTRATIONS,
                 M =/= error_logger orelse UseErrLogger],
    ok = lists:foreach(fun add_handler/1, Regs),
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
handle_info({gen_event_EXIT, _Handler, shutdown}, State) ->
    %% the respective event manager was shutdown properly, we can also RIP
    {stop, normal, State};
handle_info({gen_event_EXIT, Handler, Reason}, State) ->
    %% accidential unregistration, try to re-subscribe the event handler
    ok = ?ERR("~s - handler ~w exited with ~w~n", [?MODULE, Handler, Reason]),
    ok = add_handler(lists:keyfind(Handler, 2, ?REGISTRATIONS)),
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
add_handler({Manager, Handler}) ->
    ok = gen_event:add_sup_handler(Manager, Handler, []),
    ?ERR("~s - added handler ~w to manager ~s~n", [?MODULE, Handler, Manager]).
