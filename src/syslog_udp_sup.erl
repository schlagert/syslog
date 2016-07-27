%%%=============================================================================
%%% Copyright 2016, comtihon <com.tihon@mail.ru>
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
%%% Supervisor for udp senders pool. Uses poolboy for setting a pool.
%%%
%%% @see syslog_udp_sender
%%% @end
%%%=============================================================================
-module(syslog_udp_sup).
-author("tihon").

-behaviour(supervisor).

-include("syslog.hrl").

%% API
-export([start_link/0, send/3, return_worker/0, send_if_available/2, status/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SENDER_POOL, udp_pool).
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
%% Request worker (may create new, if overflow specified) and order him
%% to send report. Worker will do checkin itself.
%% If no workers available - wait for worker.
send(Report, Protocol, Timeout) ->
  Worker = poolboy:checkout(?SENDER_POOL, true, Timeout),
  Worker ! {send, Report, Protocol},
  ok.

%% Request worker (may create new, if overflow specified) and order him
%% to send report. Worker will do checkin itself.
%% If no workers available - drop this report.
send_if_available(Report, Protocol) ->
  case poolboy:checkout(?SENDER_POOL, false) of
    full -> ok;
    Worker ->
      Worker ! {send, Report, Protocol},
      ok
  end.

return_worker() ->
  poolboy:checkin(?SENDER_POOL, self()).

status() ->
  poolboy:status(?SENDER_POOL).

%%--------------------------------------------------------------------
%% @doc
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init([]) ->
  PoolConf = form_pool_conf(),
  WorkerConf = form_worker_conf(),
  {ok, {{one_for_one, 1000, 3600},
    [{?SENDER_POOL, {poolboy, start_link, [PoolConf, WorkerConf]}, transient, brutal_kill, worker, [poolboy]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
form_pool_conf() ->
  [{name, {local, ?SENDER_POOL}} | [{worker_module, syslog_udp_sender} | syslog_lib:get_property(pool_conf, [])]].

%% @private
form_worker_conf() ->
  Host = syslog_lib:get_property(dest_host, ?DEST_HOST),
  Port = syslog_lib:get_property(dest_port, ?DEST_PORT),
  {Host, Port}.