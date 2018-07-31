%%%=============================================================================
%%% Copyright 2011, Travelping GmbH <info@travelping.com>
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
%%% Main module of the `syslog' application. This module contains functions to
%%% send messages directly over Syslog bypassing the `error_logger'. Like its
%%% `error_logger' counterparts these functions never fail.
%%%
%%% The logging functions have the same name than the `error_logger' functions
%%% making switching from one to another easy.
%%% @end
%%%=============================================================================
-module(syslog).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([debug_msg/1,
         debug_msg/2,
         info_msg/1,
         info_msg/2,
         warning_msg/1,
         warning_msg/2,
         error_msg/1,
         error_msg/2,
         msg/2,
         msg/3,
         msg/4,
         msg/5,
         set_log_level/1,
         set_log_mode/1]).

%% Application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([init/1]).

-type facility() :: kern | kernel | user | mail | daemon | auth | syslog | lpr |
                    news | uucp | cron | authpriv | ftp | ntp | audit | alert |
                    clock | local0 | local1 | local2 | local3 | local4 |
                    local5 | local6 | local7.

-type severity() :: emergency | alert | critical | error | warning | notice |
                    informational | debug | crash.

-type proc_name() :: atom() | pid() | string().

-type datetime() :: {calendar:datetime(), non_neg_integer()}.

-type sd_id() :: atom() | binary() | iolist().
-type param_name() :: atom() | binary() | iolist().
-type param_value() :: atom() | binary() | iolist() | integer() | float().
-type sd_param() :: {param_name(), param_value()}.
-type sd_element() :: {sd_id(), [sd_param()]}.

-export_type([facility/0,
              severity/0,
              proc_name/0,
              datetime/0,
              sd_id/0,
              sd_param/0,
              sd_element/0]).

-include("syslog.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Sends a message with severity `debug'. This function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec debug_msg(io:format()) -> ok.
debug_msg(Msg) -> debug_msg(Msg, []).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a format message with severity `debug'. This function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec debug_msg(io:format(), [term()] | no_format) -> ok.
debug_msg(Fmt, Args) -> msg(debug, Fmt, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a message with severity `informational'. Like the `error_logger'
%% counterpart this function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec info_msg(io:format()) -> ok.
info_msg(Msg) -> info_msg(Msg, []).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a format message with severity `informational'. Like the `error_logger'
%% counterpart this function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec info_msg(io:format(), [term()] | no_format) -> ok.
info_msg(Fmt, Args) -> msg(informational, Fmt, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a message with severity `warning'. Like the `error_logger' counterpart
%% this function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec warning_msg(io:format()) -> ok.
warning_msg(Msg) -> warning_msg(Msg, []).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a format message with severity `warning'. Like the `error_logger'
%% counterpart this function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec warning_msg(io:format(), [term()] | no_format) -> ok.
warning_msg(Fmt, Args) -> msg(warning, Fmt, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a message with severity `error'. Like the `error_logger' counterpart
%% this function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec error_msg(io:format()) -> ok.
error_msg(Msg) -> error_msg(Msg, []).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a format message with severity `error'. Like the `error_logger'
%% counterpart this function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec error_msg(io:format(), [term()] | no_format) -> ok.
error_msg(Fmt, Args) -> msg(error, Fmt, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a message with a specific severity. This function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec msg(severity(), io:format()) -> ok.
msg(Severity, Msg) -> msg(Severity, Msg, []).

%%------------------------------------------------------------------------------
%% @doc
%% Logs a format message with a specific severity. This function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec msg(severity(), io:format(), [term()] | no_format) -> ok.
msg(Severity, Fmt, Args) -> msg(Severity, self(), Fmt, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Logs a format message with a specific severity from a specific process. This
%% function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec msg(severity(), proc_name(), io:format(), [term()] | no_format) -> ok.
msg(Severity, Pid, Fmt, Args) ->
    msg(Severity, Pid, [], Fmt, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Logs a format message with a specific severity from a specific process with
%% the specified STRUCTURED-DATA (if this is supported by the formatting
%% backend). Note that STRUCTURED-DATA is not checked for validity.
%% This function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec msg(severity(),
          proc_name(),
          [sd_element()],
          io:format(),
          [term()] | no_format) -> ok.
msg(Severity, Pid, SD, Fmt, Args) ->
    syslog_logger:log(Severity, Pid, os:timestamp(), SD, Fmt, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Change the log level to the given value.
%% @end
%%------------------------------------------------------------------------------
-spec set_log_level(severity()) -> ok | {error, term()}.
set_log_level(Level) -> syslog_logger:set_log_level(Level).

%%------------------------------------------------------------------------------
%% @doc
%% Change the log mode to the given mode.
%% @end
%%------------------------------------------------------------------------------
-spec set_log_mode(async | sync | {sync, pos_integer()}) ->
                          ok | {error, term()}.
set_log_mode(Mode) -> syslog_logger:set_log_mode(Mode).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    ok = syslog_lib:ensure_error_logger(),
    DisableTty = syslog_lib:get_property(disable_tty, true),
    ok = iff(DisableTty, fun() -> error_logger:tty(false) end),
    case supervisor:start_link(?MODULE, []) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            ok = iff(DisableTty, fun() -> error_logger:tty(true) end),
            Error
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
stop(_State) -> ok.

%%%=============================================================================
%%% supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    Specs = [server(syslog_logger), server(syslog_monitor)],
    {ok, {{one_for_one, 5, 10}, Specs}}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
server(M) -> spec(M, [M]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
spec(M, Ms) -> {M, {M, start_link, []}, transient, brutal_kill, worker, Ms}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
iff(true, Fun)   -> Fun();
iff(false, _Fun) -> ok.
