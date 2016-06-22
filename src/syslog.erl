%%%=============================================================================
%%% Copyright 2011, Travelping GmbH <info@travelping.com>
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

-define(CHILD(I, Type, Mod), {I, {I, start_link, []}, transient, brutal_kill, Type, Mod}).
-define(CHILD(I, Type), ?CHILD(I, Type, [I])).

-include("syslog.hrl").

%% API
-export([debug_msg/1,
         debug_msg/2,
         info_msg/1,
         info_msg/2,
         warning_msg/1,
         warning_msg/2,
         error_msg/1,
         error_msg/2,
         msg/3,
         msg/4]).

%% Application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([init/1]).

-type facility() :: kern | kernel | user | mail | daemon | auth | syslog | lpr |
                    news | uucp | cron | authpriv | ftp | ntp | audit | alert |
                    clock | local0 | local1 | local2 | local3 | local4 |
                    local5 | local6 | local7.

-type severity() :: ?EMERGENCY | ?ALERT | ?CRITICAL | ?ERROR | ?WARNING | ?NOTICE |
                    ?INFORMATIONAL | ?DEBUG | ?CRASH.

-type option() :: {dest_host, inet:ip_address() | inet:hostname()} |
                  {dest_port, inet:port_number()} |
                  {error_facility, facility()} |
                  {facility, facility()} |
                  {msg_queue_limit, Limit :: pos_integer() | infinity} |
                  {protocol, rfc3164 | rfc5424} |
                  {use_rfc5424_bom, boolean()} |
                  {verbose, true | {false, Depth :: pos_integer()}} |
                  {no_progress, boolean()}.

-export_type([facility/0, severity/0, option/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Sends a message with severity `debug'. This function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec debug_msg(string()) -> ok.
debug_msg(Msg) -> debug_msg(Msg, []).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a format message with severity `notice'. This function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec debug_msg(string(), [term()]) -> ok.
debug_msg(Fmt, Args) -> msg(?DEBUG, Fmt, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a message with severity `notice'. Like the `error_logger' counterpart
%% this function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec info_msg(string()) -> ok.
info_msg(Msg) -> info_msg(Msg, []).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a format message with severity `notice'. Like the `error_logger'
%% counterpart this function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec info_msg(string(), [term()]) -> ok.
info_msg(Fmt, Args) -> msg(?NOTICE, Fmt, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a message with severity `warning'. Like the `error_logger' counterpart
%% this function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec warning_msg(string()) -> ok.
warning_msg(Msg) -> warning_msg(Msg, []).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a format message with severity `warning'. Like the `error_logger'
%% counterpart this function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec warning_msg(string(), [term()]) -> ok.
warning_msg(Fmt, Args) -> msg(?WARNING, Fmt, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a message with severity `error'. Like the `error_logger' counterpart
%% this function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec error_msg(string()) -> ok.
error_msg(Msg) -> error_msg(Msg, []).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a format message with severity `error'. Like the `error_logger'
%% counterpart this function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec error_msg(string(), [term()]) -> ok.
error_msg(Fmt, Args) -> msg(?ERROR, Fmt, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Logs a format message with a specific severity. This function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec msg(severity(), string() | pid(), [term()] | binary()) -> ok.
msg(Severity, Fmt, Args) when is_list(Fmt)  -> msg(Severity, self(), Fmt, Args);
msg(Severity, Pid, Msg) when is_binary(Msg) -> forward_msg(Severity, Pid, Msg).

%%------------------------------------------------------------------------------
%% @doc
%% Logs a format message with a specific severity from a specific process. This
%% function never fails.
%% @end
%%------------------------------------------------------------------------------
-spec msg(severity(), pid(), string(), [term()]) -> ok.
msg(Severity, Pid, Fmt, Args) ->
    try
        forward_msg(Severity, Pid, iolist_to_binary(io_lib:format("[~s] " ++ Fmt, [Severity | Args])))
    catch
        C:E -> ?ERR("io_lib:format(~p, ~p) failed (~p:~p)~n", [Fmt, Args, C, E])
    end.

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    ok = error_logger:tty(false),
    case supervisor:start_link(?MODULE, []) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
	    ok = error_logger:tty(true),
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
    Specs =
      [
        ?CHILD(syslog_udp_sup, supervisor),
        ?CHILD(syslog_logger, worker, dynamic),
        ?CHILD(syslog_monitor, worker)
      ],
    {ok, {{one_for_one, 5, 10}, Specs}}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
forward_msg(Severity, Pid, Msg) ->
    try
        syslog_logger:msg(Severity, Pid, Msg)
    catch
        _:_ -> ?ERR("~s~n", [Msg])
    end.
