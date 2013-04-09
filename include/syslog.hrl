%%%=============================================================================
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
%%%=============================================================================

-ifndef(syslog_hrl_).
-define(syslog_hrl_, 1).

%%%=============================================================================
%%% The syslog report record.
%%%=============================================================================

-record(syslog_report, {
          severity  :: 0..7,
          facility  :: 0..23,
          timestamp :: erlang:timestamp(),
          hostname  :: string(),
          domain    :: string(),
          appname   :: string(),
          beam_pid  :: string(),
          pid       :: string(),
          bom       :: binary(),
          msg       :: string()}).

%%%=============================================================================
%%% Defines for default values.
%%%=============================================================================

-define(FACILITY,  daemon).
-define(PROTOCOL,  rfc3164).
-define(DEST_HOST, {127, 0, 0, 1}).
-define(DEST_PORT, 514).
-define(LIMIT,     10000).

-endif. %% syslog_hrl_
