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
-export([hdr/3, msg/2]).

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
     syslog_lib:truncate(32, Pid), $\s, $-, $\s
    ].

%%------------------------------------------------------------------------------
%% @doc
%% Format the MSG part of RFC 5424.
%% @end
%%------------------------------------------------------------------------------
-spec msg(binary(), #syslog_cfg{}) -> binary().
msg(Msg, #syslog_cfg{bom = Bom}) -> unicode:characters_to_binary([Bom, Msg]).
