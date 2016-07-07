%%%=============================================================================
%%% Copyright 2016, Tobias Schlager <schlagert@github.com>
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
%%% A backend for `lager' redirecting its messages into the `syslog'
%%% application. Configure like this:
%%% <pre>
%%%   {handlers, [{syslog_lager_backend, []}]},
%%% </pre>
%%%
%%% Note:
%%% This modules uses apply/3 to call lager-specific functions in order to
%%% prevent dialyzer from complaining when lager is not in path.
%%%
%%% @see https://github.com/basho/lager
%%% @end
%%%=============================================================================
-module(syslog_lager_backend).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("syslog.hrl").

-define(CFG, [message]).
-define(FORMAT(Msg), apply(lager_default_formatter, format, [Msg, ?CFG])).

%%%=============================================================================
%%% gen_event callbacks
%%%=============================================================================

-record(state, {log_level :: integer() | {mask, integer()}}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([])      -> init([syslog_lib:get_property(log_level, ?SYSLOG_LOGLEVEL)]);
init([Level]) -> {ok, #state{log_level = level_to_mask(Level)}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event({log, Level, _, [_, Location, Message]}, State)
  when Level =< State#state.log_level ->
    Timestamp = os:timestamp(),
    Binary = iolist_to_binary(Message),
    syslog:forward_msg(get_severity(Level), get_pid(Location), Timestamp, Binary),
    {ok, State};
handle_event({log, Msg}, State = #state{log_level = Level}) ->
    case apply(lager_util, is_loggable, [Msg, Level, ?MODULE]) of
        true ->
            Timestamp = apply(lager_msg, timestamp, [Msg]),
            Binary = iolist_to_binary(?FORMAT(Msg)),
            syslog_logger:msg(get_severity(Msg), get_pid(Msg), Timestamp, Binary);
        false ->
            ok
    end,
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(get_loglevel, State = #state{log_level = Level}) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    try
        ok = syslog:set_log_level(config_to_level(Level)),
        {ok, ok, State#state{log_level = level_to_mask(Level)}}
    catch
        exit:{timeout, _} -> {ok, {error, timeout}, State};
        _:_               -> {ok, {error, {bad_log_level, Level}}, State}
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
get_severity(info) ->
    informational;
get_severity(Level) when is_atom(Level) ->
    Level;
get_severity(Level) when is_integer(Level) ->
    get_severity(apply(lager_util, num_to_level, [Level]));
get_severity(Msg) ->
    get_severity(apply(lager_msg, severity, [Msg])).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_pid(Location) when is_list(Location) ->
    Location;
get_pid(Msg) ->
    try
        Metadata = apply(lager_msg, metadata, [Msg]),
        case lists:keyfind(pid, 1, Metadata) of
            {pid, Pid} when is_pid(Pid) -> Pid;
            _                           -> self()
        end
    catch
        _:_ -> self()
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
level_to_mask(informational) ->
    level_to_mask(info);
level_to_mask(Level) ->
    try
        apply(lager_util, config_to_mask, [Level])
    catch
        error:undef -> apply(lager_util, level_to_num, [Level])
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
config_to_level(Config) ->
    try
        hd(apply(lager_util, config_to_levels, [Config]))
    catch
        error:undef when is_atom(Config) -> Config
    end.
