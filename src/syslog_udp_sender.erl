%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jun 2016 15:18
%%%-------------------------------------------------------------------
-module(syslog_udp_sender).
-author("tihon").

-behaviour(gen_server).

-include("syslog.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
{
  hibernate_timer :: timer:tref() | undefined,
  socket :: inet:socket(),
  dest_host :: inet:ip_address() | inet:hostname(),
  dest_port :: inet:port_number()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(tuple()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init({Host, Port}) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
  {ok, #state{dest_host = Host, dest_port = Port, socket = Socket}}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_info({send, Report, Protocol}, State = #state{socket = S, dest_host = H, dest_port = P}) ->
  [send_datagram(S, H, P, Protocol:to_iolist(R)) || R <- split(Report)],
  syslog_udp_sup:return_worker(),
  {noreply, need_timer(Report, State)};
handle_info({udp_closed, S}, #state{socket = S}) ->
  {error, {udp_closed, S}};
handle_info(hibernate, State) ->
  {noreply, State#state{hibernate_timer = undefined}, hibernate};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
terminate(_Arg, #state{socket = Socket}) ->
  gen_udp:close(Socket).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
need_timer(#syslog_report{msg = Msg}, State = #state{hibernate_timer = undefiner}) when byte_size(Msg) > 64 ->
  TRef = erlang:send_after(1000, self(), hibernate),
  State#state{hibernate_timer = TRef};
need_timer(_, State) ->
  State.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_datagram(S, H, P, Data) ->
  ok = gen_udp:send(S, H, P, Data).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
split(R = #syslog_report{msg = Msg}) ->
  [R#syslog_report{msg = Line} || Line <- split_impl(Msg), Line =/= <<>>].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
split_impl(Bin) when is_binary(Bin) -> binary:split(Bin, <<"\n">>, [global]).