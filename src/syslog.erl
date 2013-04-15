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
%%% enable or disable logging via syslog as well as two convinience functions
%%% to log messages with a certain severity.
%%%
%%% This module also defines the behaviour that must be implemented by protocol
%%% backends.
%%% @end
%%%=============================================================================
-module(syslog).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([enable/0, disable/0, log/2, log/3]).

%% Application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([init/1]).

-include("syslog.hrl").

-define(CHILD, syslog_monitor).
-define(SPEC, {?CHILD, {?CHILD, start_link, []}, transient, 1000, worker, [?CHILD]}).

-type facility() :: kern | kernel | user | mail | daemon | auth | syslog | lpr |
                    news | uucp | cron | authpriv | ftp | ntp | audit | alert |
                    clock | local0 | local1 | local2 | local3 | local4 |
                    local5 | local6 | local7.

-type severity() :: emergency | alert | critical | error |  warning | notice |
                    informational | debug.

-type option() :: {msg_queue_limit, pos_integer() | infinity} |
                  {protocol, rfc3164 | rfc5424} |
                  {dest_host, inet:ip_address() | inet:hostname()} |
                  {dest_port, inet:port_number()} |
                  {facility, facility()} |
                  {error_facility, facility()} |
                  {use_rfc5424_bom, boolean()}.

-export_type([facility/0, severity/0, option/0]).

%%%=============================================================================
%%% callback definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% This is the behaviour that must be implemented by protocol backends.
%%------------------------------------------------------------------------------

-callback to_iolist(#syslog_report{}) -> iolist().

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Enable SASL report logging via `syslog'. This will also disable the standard
%% `error_logger' TTY output, if successful.
%% @end
%%------------------------------------------------------------------------------
-spec enable() -> ok.
enable() ->
    supervisor:delete_child(?MODULE, ?CHILD),
    {ok, _} = supervisor:start_child(?MODULE, ?SPEC),
    error_logger:tty(false).

%%------------------------------------------------------------------------------
%% @doc
%% Disable SASL report logging via `syslog'. This will also re-enable the
%% standard `error_logger' TTY output, if successful.
%% @end
%%------------------------------------------------------------------------------
-spec disable() -> ok.
disable() ->
    ok = supervisor:terminate_child(?MODULE, ?CHILD),
    supervisor:delete_child(?MODULE, ?CHILD),
    error_logger:tty(true).

%%------------------------------------------------------------------------------
%% @doc
%% Log a message with a specific severity.
%% @end
%%------------------------------------------------------------------------------
-spec log(severity(), string()) -> ok.
log(Severity, Msg) -> log(Severity, Msg, []).

%%------------------------------------------------------------------------------
%% @doc
%% Log a format message with a specific severity.
%% @end
%%------------------------------------------------------------------------------
-spec log(severity(), string(), [term()]) -> ok.
log(Severity, Fmt, Args) ->
    Report = [{args, Args}, {fmt, Fmt}, {severity, Severity}],
    error_logger:info_report(syslog, Report).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    Result = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    Enabled = application:get_env(?MODULE, enabled),
    maybe_enable(Result, Enabled).

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
init([]) -> {ok, {{one_for_one, 5, 10}, []}}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_enable(Result = {ok, _}, {ok, true}) -> ok = enable(), Result;
maybe_enable(Result, _)                    -> Result.
