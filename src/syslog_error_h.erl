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
%%% An event handler to be attached to the `error_logger' event manager.
%%%
%%% The received messages will be formatted and forwarded to the
%%% {@link syslog_logger} server.
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

-define(PERCENTAGE, 10).
-define(LIMIT, infinity).
-define(VERBOSITY, true).

%% ** Generic server ... terminating
-define(SERVER_ERR, "** Generic server " ++ _).
%% ** State machine ... terminating
-define(FSM_ERR, "** State machine " ++ _).
%% ** gen_event handler ... crashed
-define(EVENT_ERR, "** gen_event handler " ++ _).

%%%=============================================================================
%%% gen_event callbacks
%%%=============================================================================

-record(state, {
          msgs_to_drop = 0    :: non_neg_integer(),
          dropped = {0, 0, 0} :: {integer(), integer(), integer()},
          init_pid            :: pid(),
          no_progress         :: boolean(),
          verbose             :: true | {false, pos_integer()},
          msg_queue_limit     :: pos_integer() | infinity,
          drop_percentage     :: pos_integer(),
          extra_report        :: boolean()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Arg) ->
    Facility = syslog_lib:get_property(facility, ?SYSLOG_FACILITY),
    CrashFacility = syslog_lib:get_property(crash_facility, ?SYSLOG_FACILITY),
    {ok, #state{
            %% Starting with OTP 20.1 one does need a valid group_leader to call
            %% sasl_format:format_report/3. Since init (<0.0.0>) is the
            %% group_leader of all group_leaders and thus kind of static, we'll
            %% use that to determine the supported system encoding.
            init_pid        = whereis(init),
            no_progress     = syslog_lib:get_property(no_progress, ?SYSLOG_NO_PROGRESS),
            verbose         = syslog_lib:get_property(verbose, ?VERBOSITY),
            msg_queue_limit = syslog_lib:get_property(msg_queue_limit, ?LIMIT),
            drop_percentage = syslog_lib:get_property(drop_percentage, ?PERCENTAGE),
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
        Len when Limit < (100 div State#state.drop_percentage) ->
            {ok, drop_msg(Event, State#state{msgs_to_drop = Len})};
        Len ->
            ToDrop = Len + (Limit div (100 div State#state.drop_percentage)),
            {ok, drop_msg(Event, State#state{msgs_to_drop = ToDrop})}
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
    ?ERR("~s - DROPPED ~w errors, ~w warnings, ~w notices~n", [?MODULE, E, W, I]),
    State#state{dropped = {0, 0, 0}};
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
    log_msg(crash, Pid, Fmt, Args, State);
handle_msg({error, _, {Pid, Fmt = ?FSM_ERR, Args}}, State) ->
    log_msg(crash, Pid, Fmt, Args, State);
handle_msg({error, _, {Pid, Fmt = ?EVENT_ERR, Args}}, State) ->
    log_msg(crash, Pid, Fmt, Args, State);
handle_msg({error, _, {Pid, Fmt, Args}}, State) ->
    log_msg(error, Pid, Fmt, Args, State);
handle_msg({error_report, _, {Pid, Type, Report}}, State) ->
    log_report(error, Pid, Type, Report, State);
handle_msg({warning_msg, _, {Pid, Fmt, Args}}, State) ->
    log_msg(warning, Pid, Fmt, Args, State);
handle_msg({warning_report, _, {Pid, Type, Report}}, State) ->
    log_report(warning, Pid, Type, Report, State);
handle_msg({info_msg, _, {Pid, Fmt, Args}}, State) ->
    log_msg(informational, Pid, Fmt, Args, State);
handle_msg({info_report, _, {Pid, Type, Report}}, State) ->
    log_report(informational, Pid, Type, Report, State);
handle_msg(_, State) ->
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log_msg(Severity, Pid, Fmt, Args, State) ->
    syslog_logger:async_log(Severity, Pid, os:timestamp(), [], Fmt, Args),
    State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log_report(_, Pid, crash_report, Report, State) ->
    log_crash(State#state.extra_report, Pid, Report, State);
log_report(_, Pid, _, [{application, A}, {started_at, N} | _], State) ->
    log_msg(informational, Pid, "started application ~s on node ~s", [A, N], State);
log_report(_, Pid, _, [{application, A}, {exited, R} | _], State = #state{verbose = true}) ->
    log_msg(error, Pid, "application ~s exited with ~p", [A, R], State);
log_report(_, Pid, _, [{application, A}, {exited, R} | _], State = #state{verbose = {false, D}}) ->
    log_msg(error, Pid, "application ~s exited with ~P", [A, R, D], State);
log_report(_, _, progress, _, State = #state{no_progress = true}) ->
    State;
log_report(_, Pid, progress, Report, State = #state{verbose = true}) ->
    Details = proplists:get_value(started, Report, []),
    Child = syslog_lib:get_pid(proplists:get_value(pid, Details)),
    Mfargs = proplists:get_value(mfargs, Details),
    log_msg(informational, Pid, "started child ~s with ~p", [Child, Mfargs], State);
log_report(_, Pid, progress, Report, State = #state{verbose = {false, D}}) ->
    Details = proplists:get_value(started, Report, []),
    Child = syslog_lib:get_pid(proplists:get_value(pid, Details)),
    Mfargs = proplists:get_value(mfargs, Details),
    log_msg(informational, Pid, "started child ~s with ~P", [Child, Mfargs, D], State);
log_report(_, Pid, supervisor_report, Report, State = #state{init_pid = Init}) ->
    Timestamp = os:timestamp(),
    Time = calendar:now_to_local_time(Timestamp),
    Event = {Time, {error_report, self(), {Pid, supervisor_report, Report}}},
    Msg = sasl_report:format_report(Init, all, Event),
    syslog_logger:async_log(crash, Pid, Timestamp, [], Msg, no_format),
    State;
log_report(Severity, Pid, _, Report, State = #state{verbose = true}) ->
    log_msg(Severity, Pid, "~p", [Report], State);
log_report(Severity, Pid, _, Report, State = #state{verbose = {false, D}}) ->
    log_msg(Severity, Pid, "~P", [Report, D], State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log_crash(false, Pid, Report, State = #state{init_pid = Init}) ->
    Timestamp = os:timestamp(),
    Time = calendar:now_to_local_time(Timestamp),
    Event = {Time, {error_report, self(), {Pid, crash_report, Report}}},
    Msg = sasl_report:format_report(Init, all, Event),
    syslog_logger:async_log(crash, Pid, Timestamp, [], Msg, no_format),
    State;
log_crash(true, Pid, Report = [SubReport | _], State) ->
    NewState = log_crash(false, Pid, Report, State),
    try proplists:get_value(error_info, SubReport) of
        {Class, {Reason, [{M, F, Args, Ps} | _]}, _} when is_list(Args) ->
            As = string:join([io_lib:format("~w", [A]) || A <- Args], ","),
            log_msg(error, Pid, "exited with ~w at ~s:~s(~s)~s",
                    [{Class, Reason}, M, F, As, get_line(Ps)], NewState);
        {Class, {Reason, [{M, F, Arity, Ps} | _]}, _} when is_integer(Arity) ->
            log_msg(error, Pid, "exited with ~w at ~s:~s/~w~s",
                    [{Class, Reason}, M, F, Arity, get_line(Ps)], NewState);
        {Class, {Reason, []}, _} ->
            log_msg(error, Pid, "exited with ~w", [{Class, Reason}], NewState);
        {Class, Reason, _} ->
            log_msg(error, Pid, "exited with ~w", [{Class, Reason}], NewState);
        _ ->
            log_msg(error, Pid, "exited with ~w", [SubReport], NewState)
    catch
        _:_ ->
            ?ERR("~w exited with ~w~n", [Pid, SubReport]),
            NewState
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_line(Proplist) ->
    case proplists:get_value(line, Proplist) of
        L when is_integer(L) -> [" line ", integer_to_list(L)];
        undefined            -> ""
    end.
