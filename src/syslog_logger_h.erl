%%%=============================================================================
%%% Copyright 2018-2018, Tobias Schlager <schlagert@github.com>
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
%%% An event handler for the new (OTP 21) `logger' API. This handler will
%%% forward processed log messages to the `syslog_logger' API. All message
%%% formatting and metadata processing is done in the calling process. Thus,
%%% the `syslog' model of load distribution is not affected and properly
%%% in-place.
%%%
%%% There is no performance or safety penalty for using the official
%%% `logger' API instead of the `syslog' API. It is even highly recommended to
%%% use the new OTP-built-in API.
%%%
%%% This handler will generate structured data from log message metadata if
%%% configured to do so. This works quite similar to the mechanism used in the
%%% `syslog_lager_backend'.
%%% @end
%%%=============================================================================
-module(syslog_logger_h).

%% API
-export([add_handler/0,
         remove_handler/0]).

%% logger callbacks
-export([log/2,
         adding_handler/1,
         removing_handler/1,
         changing_config/2]).

-include("syslog.hrl").

-define(FORMATTER_CFG, #{single_line => false, template => [msg]}).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Add `syslog' handler(s) to the OTP `logger'. This is done, the recommended
%% way using {@link logger:add_handlers/1}. Thus, the handler configuration is
%% purely done using the application/system configuration.
%% @end
%%------------------------------------------------------------------------------
-spec add_handler() -> ok | {error, term()}.
add_handler() -> logger:add_handlers(syslog).

%%------------------------------------------------------------------------------
%% @doc
%% Remove all `syslog' handler(s) from the OTP `logger'.
%% @end
%%------------------------------------------------------------------------------
-spec remove_handler() -> ok.
remove_handler() ->
    lists:foreach(
      fun logger:remove_handler/1,
      [Id || Id <- logger:get_handler_ids(), is_my_handler(Id)]).

%%%=============================================================================
%%% logger callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(LogEvent, Cfg) ->
    try
        log_impl(LogEvent, Cfg)
    catch
        _:_ -> ?ERR("~s - failed to process log event ~w", [?MODULE, LogEvent])
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec adding_handler(logger:handler_config()) ->
                            {ok, logger:handler_config()} | {error, term()}.
adding_handler(Cfg) ->
    ?ERR("~s - adding handler ~s to logger~n", [?MODULE, ?MODULE]),
    verify_cfg(Cfg).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec removing_handler(logger:handler_config()) -> ok.
removing_handler(_Cfg) ->
    ?ERR("~s - removing handler ~s from logger~n", [?MODULE, ?MODULE]).

%%------------------------------------------------------------------------------
%% @private
%% Propagate the log level change to syslog.
%%------------------------------------------------------------------------------
-spec changing_config(logger:handler_config(), logger:handler_config()) ->
                             {ok, logger:handler_config()} | {error, term()}.
changing_config(#{level := Old}, NewCfg = #{level := New}) ->
    ok = case {Old, New} of
             {Old, Old} -> ok;
             {_, all}   -> syslog_logger:set_log_level(debug);
             {_, none}  -> syslog_logger:set_log_level(emergency);
             {_, info}  -> syslog_logger:set_log_level(informational);
             {_, New}   -> syslog_logger:set_log_level(New)
         end,
    verify_cfg(NewCfg);
changing_config(_, NewCfg) ->
    {error, {invalid_config, NewCfg}}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% Find out whether a certain logger handler belongs to this application.
%%------------------------------------------------------------------------------
is_my_handler(HandlerId) ->
    case logger:get_handler_config(HandlerId) of
        {ok, #{module := ?MODULE}} -> true;
        _                          -> false
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log_impl(LogEvent = #{level := Level, msg := Msg, meta := Metadata},
         #{formatter := {Fmt, FmtCfg}, config := HandlerCfg}) ->
    Pid = maps_get(pid, Metadata, self()),
    Time = maps_get(time, Metadata, os:timestamp()),
    SD = structured_data(Msg, Metadata, HandlerCfg),
    LogMsg = Fmt:format(LogEvent, FmtCfg),
    Overrides = overrides(Metadata, HandlerCfg),
    case {maps:find(extra_report, HandlerCfg), Level, Msg} of
        {{ok, true}, error, {report, #{label := {supervisor, _}}}} ->
            syslog_logger:log(crash, Pid, Time, SD, LogMsg, no_format, Overrides);
        {{ok, true}, error, {report, #{label := {_, terminate}}}} ->
            syslog_logger:log(crash, Pid, Time, SD, LogMsg, no_format, Overrides);
        {{ok, true}, error, {report, Report = #{label := {_, crash}}}} ->
            syslog_logger:log(crash, Pid, Time, SD, LogMsg, no_format, Overrides),
            case maps_get(report, Report, []) of
                [ProcEnv | _] when is_list(ProcEnv) ->
                    ErrorInfo = proplists:get_value(error_info, ProcEnv),
                    log_extra_report(Pid, Time, Overrides, ErrorInfo);
                _ ->
                    ok
            end;
        _ ->
            Severity = level_to_severity(Level),
            syslog_logger:log(Severity, Pid, Time, SD, LogMsg, no_format, Overrides)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log_extra_report(Pid, Time, Overrides, {Class, Reason, [{M, F, Args, Ps} | _]})
  when is_list(Args) ->
    As = lists:join($,, [io_lib:format("~w", [A]) || A <- Args]),
    Fmt = "exited with ~w at ~s:~s(~s)~s",
    Args = [{Class, Reason}, M, F, As, get_line(Ps)],
    syslog_logger:log(error, Pid, Time, [], Fmt, Args, Overrides);
log_extra_report(Pid, Time, Overrides, {Class, Reason, [{M, F, Arity, Ps} | _]})
  when is_integer(Arity) ->
    Fmt = "exited with ~w at ~s:~s/~w~s",
    Args = [{Class, Reason}, M, F, Arity, get_line(Ps)],
    syslog_logger:log(error, Pid, Time, [], Fmt, Args, Overrides);
log_extra_report(Pid, Time, Overrides, {Class, Reason, _}) ->
    Fmt = "exited with ~w",
    Args = [{Class, Reason}],
    syslog_logger:log(error, Pid, Time, [], Fmt, Args, Overrides);
log_extra_report(Pid, Time, Overrides, {Class, {Reason, Stack}})
  when is_list(Stack) ->
    log_extra_report(Pid, Time, Overrides, {Class, Reason, Stack});
log_extra_report(Pid, Time, Overrides, Reason) ->
    Fmt = "exited with ~w",
    Args = [Reason],
    syslog_logger:log(error, Pid, Time, [], Fmt, Args, Overrides).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_line(Proplist) ->
    case proplists:get_value(line, Proplist) of
        L when is_integer(L) -> [" line ", integer_to_list(L)];
        undefined            -> ""
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
structured_data(_, _, #{sd_id := undefined}) ->
    [];
structured_data(_, _, #{meta_keys := []}) ->
    [];
structured_data({report, Report}, Metadata, HandlerCfg) when is_map(Report) ->
    structured_data(ignored, maps:merge(Report, Metadata), HandlerCfg);
structured_data({report, Report}, Metadata, HandlerCfg) when is_list(Report) ->
    structured_data({report, maps:from_list(Report)}, Metadata, HandlerCfg);
structured_data(_, Metadata, #{sd_id := SDId, meta_keys := MDKeys}) ->
    syslog_lib:get_structured_data(Metadata, SDId, MDKeys).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
overrides(Metadata, HandlerCfg) -> appname_override(Metadata, HandlerCfg).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
appname_override(_, #{appname_key := undefined}) ->
    [];
appname_override(Metadata, #{appname_key := AppnameKey}) ->
    case maps:find(AppnameKey, Metadata) of
        {ok, Value} -> [{appname, Value}];
        error       -> []
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
level_to_severity(info)                      -> informational;
level_to_severity(Level) when is_atom(Level) -> Level.

%%------------------------------------------------------------------------------
%% @private
%% Ensures the necessary configuration mappings.
%%------------------------------------------------------------------------------
verify_cfg(Cfg) when is_map(Cfg) ->
    HandlerCfg0 = maps_get(config, Cfg, #{}),
    HandlerCfg1 = maps_put_if_not_present(sd_id, undefined, HandlerCfg0),
    HandlerCfg2 = maps_put_if_not_present(meta_keys, [], HandlerCfg1),

    Facility = syslog_lib:get_property(facility, ?SYSLOG_FACILITY),
    CrashFacility = syslog_lib:get_property(crash_facility, ?SYSLOG_FACILITY),
    ExtraReport = Facility =/= CrashFacility,
    HandlerCfg3 = maps:put(extra_report, ExtraReport, HandlerCfg2),

    AppnameKey = syslog_lib:get_name_metdata_key(),
    HandlerCfg4 = maps_put_if_not_present(appname_key, AppnameKey, HandlerCfg3),
    Cfg1 = maps:put(config, HandlerCfg4, Cfg),

    NoProgress = syslog_lib:get_property(no_progress, ?SYSLOG_NO_PROGRESS),
    ProgressAction = case NoProgress of true -> stop; false -> log end,
    Filters = [{progress, {fun logger_filters:progress/2, ProgressAction}}],
    Cfg2 = maps_put_if_not_present(filters, Filters, Cfg1),

    FormatterCfg = syslog_lib:get_property(formatter_cfg, ?FORMATTER_CFG),
    {ok, maps:put(formatter, {logger_formatter, FormatterCfg}, Cfg2)};
verify_cfg(Cfg) ->
    {error, {invalid_config, Cfg}}.

%%------------------------------------------------------------------------------
%% @private
%% Support OTP-17.0
%%------------------------------------------------------------------------------
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error       -> Default
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maps_put_if_not_present(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, _} -> Map;
        error   -> maps:put(Key, Value, Map)
    end.
