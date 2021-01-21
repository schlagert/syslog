%%%=============================================================================
%%% Copyright 2018-2021, Tobias Schlager <schlagert@github.com>
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

-define(FORMATTER, {logger_formatter, #{template => [msg]}}).

-dialyzer({no_missing_calls, log_extra_report/4}).

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
    Overrides = overrides(Metadata, HandlerCfg),
    case Msg of
        {report, #{label := {supervisor, _}}} ->
            NewFmtCfg = maps:merge(FmtCfg, Metadata),
            LogMsg = Fmt:format(LogEvent, NewFmtCfg#{single_line => false}),
            syslog_logger:log(crash, Pid, Time, SD, LogMsg, no_format, Overrides);
        {report, #{label := {_, terminate}}} ->
            NewFmtCfg = maps:merge(FmtCfg, Metadata),
            LogMsg = Fmt:format(LogEvent, NewFmtCfg#{single_line => false}),
            syslog_logger:log(crash, Pid, Time, SD, LogMsg, no_format, Overrides);
        {report, Report = #{label := {_, crash}}} ->
            NewFmtCfg = maps:merge(FmtCfg, Metadata),
            LogMsg = Fmt:format(LogEvent, NewFmtCfg#{single_line => false}),
            syslog_logger:log(crash, Pid, Time, SD, LogMsg, no_format, Overrides),
            case maps:find(extra_report, HandlerCfg) of
                {ok, true} ->
                    case maps:find(report, Report) of
                        {ok, [ProcEnv | _]} when is_list(ProcEnv) ->
                            ErrorInfo = proplists:get_value(error_info, ProcEnv),
                            log_extra_report(Pid, Time, Overrides, ErrorInfo);
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end;
        Msg ->
            Severity = level_to_severity(Level),
            LogMsg = Fmt:format(LogEvent, maps:merge(FmtCfg, Metadata)),
            syslog_logger:log(Severity, Pid, Time, SD, LogMsg, no_format, Overrides)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log_extra_report(Pid, Time, Overrides, ErrorInfo) ->
    {Fmt, Args} = extra_report(ErrorInfo),
    syslog_logger:log(error, Pid, Time, [], Fmt, Args, Overrides).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
extra_report({Class, Reason, [{M, F, Args, Ps} | _]})
  when is_list(Args) ->
    As = lists:join($,, [io_lib:format("~w", [A]) || A <- Args]),
    {"exited with ~w at ~s:~s(~s)~s", [{Class, Reason}, M, F, As, get_line(Ps)]};
extra_report({Class, Reason, [{M, F, Arity, Ps} | _]})
  when is_integer(Arity) ->
    {"exited with ~w at ~s:~s/~w~s", [{Class, Reason}, M, F, Arity, get_line(Ps)]};
extra_report({Class, Reason, _}) ->
    {"exited with ~w", [{Class, Reason}]};
extra_report({Class, {Reason, Stack}})
  when is_list(Stack) ->
    extra_report({Class, Reason, Stack});
extra_report(Reason) ->
    {"exited with ~w", [Reason]}.

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
structured_data(_, _, #{structured_data := []}) ->
    [];
structured_data({report, Report}, Metadata, HandlerCfg) when is_map(Report) ->
    structured_data(ignored, maps:merge(Report, Metadata), HandlerCfg);
structured_data({report, Report}, Metadata, HandlerCfg) when is_list(Report) ->
    structured_data({report, maps:from_list(Report)}, Metadata, HandlerCfg);
structured_data(_, Metadata, #{structured_data := SDMappings}) ->
    lists:append(
      [syslog_lib:get_structured_data(Metadata, SDId, MDKeys)
       || {SDId, MDKeys} <- SDMappings, is_list(MDKeys)]).

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
    HandlerCfg1 = maps_put_if_not_present(structured_data, [], HandlerCfg0),
    true = is_list(maps:get(structured_data, HandlerCfg1)),

    Facility = syslog_lib:get_property(facility, ?SYSLOG_FACILITY),
    CrashFacility = syslog_lib:get_property(crash_facility, ?SYSLOG_FACILITY),
    ExtraReport = Facility =/= CrashFacility,
    HandlerCfg2 = maps:put(extra_report, ExtraReport, HandlerCfg1),

    AppnameKey = syslog_lib:get_name_metdata_key(),
    HandlerCfg3 = maps_put_if_not_present(appname_key, AppnameKey, HandlerCfg2),
    Cfg1 = maps:put(config, HandlerCfg3, Cfg),

    NoProgress = syslog_lib:get_property(no_progress, ?SYSLOG_NO_PROGRESS),
    ProgressAction = case NoProgress of true -> stop; false -> log end,
    ProgressFilter = {progress, {fun logger_filters:progress/2, ProgressAction}},
    RemoteGlFilter = {remote_gl, {fun logger_filters:remote_gl/2, stop}},
    CustomFilters = lists:foldl(fun(F, Fs) -> lists:keydelete(F, 1, Fs) end,
                                maps_get(filters, Cfg1, []),
                                [progress, remote_gl]),
    Filters = [ProgressFilter, RemoteGlFilter | CustomFilters],
    Cfg2 = maps:put(filters, Filters, Cfg1),

    {ok, case maps:find(formatter, Cfg2) of
             error ->
                 maps:put(formatter, ?FORMATTER, Cfg2);
             {ok, {logger_formatter, FormatterCfg}}
               when map_size(FormatterCfg) =:= 0 -> %% the OTP default
                 maps:put(formatter, ?FORMATTER, Cfg2);
             {ok, _} ->
                 Cfg2
         end};
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
