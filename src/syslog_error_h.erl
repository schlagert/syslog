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
%%% The event handler to be attached to the `error_logger' event manager.
%%% The received messages will be formatted and forwarded to the
%%% {@link syslog_logger} event manager.
%%%
%%% @see syslog_monitor
%%% @end
%%%=============================================================================
-module(syslog_error_h).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("syslog.hrl").

%% ** Generic server ... terminating
-define(SERVER_ERR, [$*,$*,32,$G,$e,$n,$e,$r,$i,$c,32,$s,$e,$r,$v,$e,$r,32 | _]).
%% ** State machine ... terminating
-define(FSM_ERR,    [$*,$*,32,$S,$t,$a,$t,$e,32,$m,$a,$c,$h,$i,$n,$e,32 | _]).
%% ** gen_event handler ... crashed
-define(EVENT_ERR,  [$*,$*,32,$g,$e,$n,$_,$e,$v,$e,$n,$t,32,$h,$a,$n,$d,$l,$e,$r,32 | _]).

%%%=============================================================================
%%% gen_event callbacks
%%%=============================================================================

-record(state, {
          msgs_to_drop = 0    :: non_neg_integer(),
          dropped = {0, 0, 0} :: {integer(), integer(), integer()},
          no_progress         :: boolean(),
          verbose             :: true | {false, pos_integer()},
          msg_queue_limit     :: pos_integer() | infinity,
          extra_report        :: boolean()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Arg) ->
    Facility = syslog_lib:get_property(facility, ?FACILITY),
    CrashFacility = syslog_lib:get_property(crash_facility, ?FACILITY),
    {ok, #state{
            no_progress     = syslog_lib:get_property(no_progress, ?NO_PROGRESS),
            verbose         = syslog_lib:get_property(verbose, ?VERBOSITY),
            msg_queue_limit = syslog_lib:get_property(msg_queue_limit, ?LIMIT),
            extra_report    = Facility =/= CrashFacility}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event(Event, State = #state{msg_queue_limit = infinity}) ->
    {ok, handle_msg(Event, State)};
handle_event(Event, State = #state{msgs_to_drop = ToDrop}) when ToDrop > 0 ->
    {ok, drop_msg(Event, State#state{msgs_to_drop = ToDrop - 1})};
handle_event(Event, State = #state{msg_queue_limit = Limit}) ->
    {message_queue_len, QueueLen} = process_info(self(), message_queue_len),
    case QueueLen - Limit of
        Len when Len =< 0 ->
            {ok, handle_msg(Event, State)};
        Len ->
            {ok, drop_msg(Event, State#state{msgs_to_drop = Len})}
    end.

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
terminate(_Arg, _State) -> ok.

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
drop_msg(Msg, State = #state{msgs_to_drop = 0, dropped = Dropped}) ->
    {E, W, I} = drop_msg_(Msg, Dropped),
    Fmt = "dropped ~p errors, ~p warnings, ~p notices",
    log_msg(?ERROR, self(), Fmt, [E, W, I], State#state{dropped = {0, 0, 0}});
drop_msg(Msg, State = #state{dropped = Dropped}) ->
    State#state{dropped = drop_msg_(Msg, Dropped)}.
drop_msg_({error, _, _}         , {E, W, I}) -> {E + 1, W, I};
drop_msg_({error_report, _, _}  , {E, W, I}) -> {E + 1, W, I};
drop_msg_({warning_msg, _, _}   , {E, W, I}) -> {E, W + 1, I};
drop_msg_({warning_report, _, _}, {E, W, I}) -> {E, W + 1, I};
drop_msg_({info_msg, _, _}      , {E, W, I}) -> {E, W, I + 1};
drop_msg_({info_report, _, _}   , {E, W, I}) -> {E, W, I + 1};
drop_msg_(_                     , {E, W, I}) -> {E, W, I}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_msg({error, _, {Pid, Fmt = ?SERVER_ERR, Args}}, State) ->
    log_msg(?CRASH, Pid, Fmt, Args, State);
handle_msg({error, _, {Pid, Fmt = ?FSM_ERR, Args}}, State) ->
    log_msg(?CRASH, Pid, Fmt, Args, State);
handle_msg({error, _, {Pid, Fmt = ?EVENT_ERR, Args}}, State) ->
    log_msg(?CRASH, Pid, Fmt, Args, State);
handle_msg({error, _, {Pid, Fmt, Args}}, State) ->
    log_msg(?ERROR, Pid, Fmt, Args, State);
handle_msg({error_report, _, {Pid, Type, Report}}, State) ->
    log_report(?ERROR, Pid, Type, Report, State);
handle_msg({warning_msg, _, {Pid, Fmt, Args}}, State) ->
    log_msg(?WARNING, Pid, Fmt, Args, State);
handle_msg({warning_report, _, {Pid, Type, Report}}, State) ->
    log_report(?WARNING, Pid, Type, Report, State);
handle_msg({info_msg, _, {Pid, Fmt, Args}}, State) ->
    log_msg(?INFORMATIONAL, Pid, Fmt, Args, State);
handle_msg({info_report, _, {Pid, Type, Report}}, State) ->
    log_report(?INFORMATIONAL, Pid, Type, Report, State);
handle_msg(_, State) ->
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log_msg(Severity, Pid, Fmt, Args, State) ->
    syslog:msg(Severity, Pid, Fmt, Args),
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log_report(_, Pid, crash_report, Report, State) ->
    log_crash(State#state.extra_report, Pid, Report, State);
log_report(_, Pid, _, [{application, A}, {started_at, N} | _], State) ->
    log_msg(?INFORMATIONAL, Pid, "started application ~s on node ~s", [A, N], State);
log_report(_, Pid, _, [{application, A}, {exited, R} | _], State = #state{verbose = true}) ->
    log_msg(?ERROR, Pid, "application ~s exited with ~p", [A, R], State);
log_report(_, Pid, _, [{application, A}, {exited, R} | _], State = #state{verbose = {false, D}}) ->
    log_msg(?ERROR, Pid, "application ~s exited with ~P", [A, R, D], State);
log_report(_, _, progress, _, State = #state{no_progress = true}) ->
    State;
log_report(_, Pid, progress, Report, State = #state{verbose = true}) ->
    Details = proplists:get_value(started, Report, []),
    Child = syslog_lib:get_pid(proplists:get_value(pid, Details)),
    Mfargs = proplists:get_value(mfargs, Details),
    log_msg(?INFORMATIONAL, Pid, "started child ~s with ~p", [Child, Mfargs], State);
log_report(_, Pid, progress, Report, State = #state{verbose = {false, D}}) ->
    Details = proplists:get_value(started, Report, []),
    Child = syslog_lib:get_pid(proplists:get_value(pid, Details)),
    Mfargs = proplists:get_value(mfargs, Details),
    log_msg(?INFORMATIONAL, Pid, "started child ~s with ~P", [Child, Mfargs, D], State);
log_report(_, Pid, supervisor_report, Report, State) ->
    Time = calendar:now_to_local_time(os:timestamp()),
    Event = {Time, {error_report, self(), {Pid, supervisor_report, Report}}},
    Msg = iolist_to_binary([<<"[crash] ">> | sasl_report:format_report(fd, all, Event)]),
    syslog:msg(?CRASH, Pid, Msg),
    State;
log_report(Severity, Pid, _, Report, State = #state{verbose = true}) ->
    log_msg(Severity, Pid, "~p", [Report], State);
log_report(Severity, Pid, _, Report, State = #state{verbose = {false, D}}) ->
    log_msg(Severity, Pid, "~P", [Report, D], State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log_crash(false, Pid, Report, State) ->
    Time = calendar:now_to_local_time(os:timestamp()),
    Event = {Time, {error_report, self(), {Pid, crash_report, Report}}},
    Msg = iolist_to_binary([<<"[crash] ">> | sasl_report:format_report(fd, all, Event)]),
    syslog:msg(?CRASH, Pid, Msg),
    State;
log_crash(true, Pid, Report = [SubReport | _], State) ->
    NewState = log_crash(false, Pid, Report, State),
    try proplists:get_value(error_info, SubReport) of
        {Class, {Reason, Stack}, _} when is_list(Stack) ->
            log_msg(?ERROR, Pid, "exited with ~w", [{Class, Reason}], NewState);
        {Class, Reason, _} ->
            log_msg(?ERROR, Pid, "exited with ~w", [{Class, Reason}], NewState);
        _ ->
            log_msg(?ERROR, Pid, "exited with ~w", [SubReport], NewState)
    catch
        _:_ -> log_msg(?ERROR, Pid, "exited with ~w", [SubReport], NewState)
    end.
