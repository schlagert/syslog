%%%=============================================================================
%%% Copyright 2016-2017, Tobias Schlager <schlagert@github.com>
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
%%% @see https://github.com/basho/lager
%%% @end
%%%=============================================================================
-module(syslog_lager_backend).

-behaviour(gen_event).

%% API
-export([set_log_level/1]).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("syslog.hrl").

-define(CFG, [message]).

%% avoid warnings when lager is not a project dependency
-dialyzer(no_missing_calls).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Set a specific log level for this lager backend, never fails.
%% @end
%%------------------------------------------------------------------------------
-spec set_log_level(syslog:severity() | info) -> ok.
set_log_level(informational) ->
    set_log_level(info);
set_log_level(Level) ->
    catch lager:set_loglevel(?MODULE, Level),
    ok.

%%%=============================================================================
%%% gen_event callbacks
%%%=============================================================================

-record(state, {
          log_level       :: integer() | {mask, integer()},
          formatter       :: atom(),
          format_cfg      :: list(),
          sd_id           :: string() | undefined,
          metadata_keys   :: [atom()],
          appname_key     :: atom() | undefined}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    init([syslog_lib:get_property(log_level, ?SYSLOG_LOGLEVEL)]);
init([Level]) ->
    init([Level, {}, {lager_default_formatter, ?CFG}]);
init([Level, {}, {Formatter, FormatterConfig}]) when is_atom(Formatter) ->
    init([Level, {undefined, []}, {Formatter, FormatterConfig}]);
init([Level, SData, {Formatter, FormatterConfig}])
  when is_atom(Formatter) ->
    AppnameKey = syslog_lib:get_name_metdata_key(),
    init([Level, SData, {Formatter, FormatterConfig}, AppnameKey]);
init([Level, {}, {Formatter, FormatterConfig}, AppnameKey])
  when is_atom(Formatter) ->
    init([Level, {undefined, []}, {Formatter, FormatterConfig}, AppnameKey]);
init([Level, {SDataId, MDKeys}, {Formatter, FormatterConfig}, AppnameKey])
  when is_atom(Formatter) ->
    {ok, #state{
            log_level = level_to_mask(Level),
            sd_id = SDataId,
            metadata_keys = MDKeys,
            formatter = Formatter,
            format_cfg = FormatterConfig,
            appname_key = get_appname_key(AppnameKey)}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event({log, Level, _, [_, Location, Message]}, State)
  when Level =< State#state.log_level ->
    Severity = get_severity(Level),
    Pid = get_pid(Location),
    Timestamp = os:timestamp(),
    syslog_logger:log(Severity, Pid, Timestamp, [], Message, no_format),
    {ok, State};
handle_event({log, Msg}, State = #state{log_level = Level}) ->
    case lager_util:is_loggable(Msg, Level, ?MODULE) of
        true ->
            syslog_logger:log(get_severity(Msg),
                              get_pid(Msg),
                              lager_msg:timestamp(Msg),
                              get_structured_data(Msg, State),
                              format_msg(Msg, State),
                              no_format,
                              get_appname_override(Msg, State));
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
        {ok, ok, State#state{log_level = level_to_mask(Level)}}
    catch
        _:_ -> {ok, {error, {bad_log_level, Level}}, State}
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
    get_severity(lager_util:num_to_level(Level));
get_severity(Msg) ->
    get_severity(lager_msg:severity(Msg)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_pid(Location) when is_list(Location) ->
    Location;
get_pid(Msg) ->
    try lager_msg:metadata(Msg) of
        Metadata ->
            case lists:keyfind(pid, 1, Metadata) of
                {pid, Pid} when is_pid(Pid)    -> Pid;
                {pid, List} when is_list(List) -> List;
                _                              -> self()
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
        lager_util:config_to_mask(Level)
    catch
        error:undef -> lager_util:level_to_num(Level)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_structured_data(_Msg, #state{sd_id = undefined}) ->
    [];
get_structured_data(_Msg, #state{metadata_keys = []}) ->
    [];
get_structured_data(Msg, #state{sd_id = SDId, metadata_keys = MDKeys}) ->
    try lager_msg:metadata(Msg) of
        Metadata -> syslog_lib:get_structured_data(Metadata, SDId, MDKeys)
    catch
        _:_ -> []
    end.

%%------------------------------------------------------------------------------
%% @private
%% Map `true' to `application' for backwards compatibility.
%%------------------------------------------------------------------------------
get_appname_key(true)                      -> application;
get_appname_key(false)                     -> undefined;
get_appname_key(Value) when is_atom(Value) -> Value.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_appname_override(_, #state{appname_key = undefined}) ->
    [];
get_appname_override(Msg, #state{appname_key = Key}) ->
    try lager_msg:metadata(Msg) of
        Metadata ->
            case proplists:get_value(Key, Metadata) of
                undefined -> [];
                Result    -> [{appname, Result}]
            end
    catch
        _:_ -> []
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
format_msg(Msg, #state{formatter = Formatter, format_cfg = FormatterConfig}) ->
    Formatter:format(Msg, FormatterConfig).
