%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2016 15:18
%%%-------------------------------------------------------------------
-module(syslog_udp_sup).
-author("tihon").

-behaviour(supervisor).

-include("syslog.hrl").

%% API
-export([start_link/0, send/2, return_worker/0, send_if_available/2]).

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
send(Report, Protocol) ->
  Worker = poolboy:checkout(?SENDER_POOL),
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