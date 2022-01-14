%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(producer).

-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(producer_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start() ->
  io:format("Hello from producer!~n"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

init([]) ->
  {ok, #producer_state{}}.

handle_call(_Request, _From, State = #producer_state{}) ->
  {reply, ok, State};
handle_call(stop, _From, Tab) ->
  {stop, normal, stopped, Tab}.

handle_cast(_Request, State = #producer_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #producer_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #producer_state{}) ->
  ok.

code_change(_OldVsn, State = #producer_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
