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
%%% An RFC5424 compliant protocol backend for syslog messages.
%%% @end
%%%=============================================================================
-module(syslog_rfc5424).

-behaviour(syslog_logger).

%% API
-export([hdr/3, msg/3]).

-include("syslog.hrl").

-define(VERSION, $1).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Format the HDR part of RFC 5424 excluding the PRI, including structured
%% elements.
%% @end
%%------------------------------------------------------------------------------
-spec hdr(syslog:datetime(), binary(), #syslog_cfg{}) -> iodata().
hdr(Datetime, Pid, #syslog_cfg{hostname = H, appname = A, beam_pid = B}) ->
    [
     ?VERSION, $\s,
     syslog_lib:format_rfc5424_date(Datetime), $\s,
     syslog_lib:truncate(255, H), $\s,
     syslog_lib:truncate(48, A), $\s,
     syslog_lib:truncate(128, B), $\s,
     syslog_lib:truncate(32, Pid), $\s
    ].

%%------------------------------------------------------------------------------
%% @doc
%% Format the MSG part of RFC 5424.
%% @end
%%------------------------------------------------------------------------------
-spec msg([syslog:sd_element()], binary(), #syslog_cfg{}) -> binary().
msg(StructuredData, <<>>, _SyslogCfg) when StructuredData =/= [] ->
    unicode:characters_to_binary(sd(StructuredData));
msg(StructuredData, Msg, #syslog_cfg{bom = Bom}) ->
    unicode:characters_to_binary([sd(StructuredData), $\s, Bom, Msg]).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
sd([])       -> $-;
sd(Elements) -> [sd_element(Element) || Element <- Elements].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
sd_element({Id, Params}) ->
    [$[, to_binary(Id), [sd_param(Param) || Param <- Params], $]].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
sd_param({Name, Value}) -> [$\s, to_binary(Name), $=, $", to_binary(Value), $"].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
to_binary(A) when is_atom(A)    -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B)  -> B;
to_binary(F) when is_float(F)   -> float_to_binary(F, [compact, {decimals, 5}]);
to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(L) when is_list(L)    -> iolist_to_binary(L).
