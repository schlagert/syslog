%%%=============================================================================
%%% Copyright 2013-2016, Tobias Schlager <schlagert@github.com>
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
%%% A simple event handler that sends incoming `iodata()' to a configured
%%% destination.
%%%
%%% @see syslog_monitor
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

-define(DEST_HOST, {127, 0, 0, 1}).
-define(DEST_PORT, 514).

%%%=============================================================================
%%% gen_event callbacks
%%%=============================================================================

-record(state, {
          socket    :: inet:socket(),
          dest_host :: inet:ip_address() | inet:hostname(),
          dest_port :: inet:port_number()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Arg) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    {ok, #state{
            socket = Socket,
            dest_host = syslog_lib:get_property(dest_host, ?DEST_HOST),
            dest_port = syslog_lib:get_property(dest_port, ?DEST_PORT)}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event({log, Msg}, State) -> {ok, send(Msg, State)};
handle_event(_, State)          -> {ok, State}.

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
send(Data, State = #state{socket = S, dest_host = H, dest_port = P}) ->
    ok = gen_udp:send(S, H, P, Data),
    State.
