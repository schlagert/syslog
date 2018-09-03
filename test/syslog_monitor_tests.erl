%%%=============================================================================
%%% Copyright 2016-2018, Tobias Schlager <schlagert@github.com>
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
%%%=============================================================================

-module(syslog_monitor_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

monitor_test() ->
    process_flag(trap_exit, true),

    {ok, Started} = application:ensure_all_started(syslog),

    case syslog_lib:has_error_logger() of
        true ->
            ?assert(has_hander(syslog_error_h, error_logger)),
            gen_event:delete_handler(error_logger, syslog_error_h, []),
            timer:sleep(500),
            ?assert(has_hander(syslog_error_h, error_logger));
        false ->
            ok
    end,

    ok = lists:foreach(fun application:stop/1, Started).

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
has_hander(Handler, Manager) ->
    lists:member(Handler, gen_event:which_handlers(Manager)).
