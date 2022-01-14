%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(consumer_registrar).

-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(consumer_registrar_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start() ->
  io:format("Hello from consumer_reg!~n"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

init([]) ->
{ok, #consumer_registrar_state{}}.

handle_call(_Request, _From, State = #consumer_registrar_state{}) ->
  {reply, ok, State};
handle_call(stop, _From, Tab) ->
  {stop, normal, stopped, Tab}.

handle_cast(_Request, State = #consumer_registrar_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #consumer_registrar_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #consumer_registrar_state{}) ->
  ok.

code_change(_OldVsn, State = #consumer_registrar_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
