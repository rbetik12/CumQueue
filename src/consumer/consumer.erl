%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(consumer).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(consumer_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  io:format("Hello from consumer!~n"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #consumer_state{}}.

handle_call(_Request, _From, State = #consumer_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #consumer_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #consumer_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #consumer_state{}) ->
  ok.

code_change(_OldVsn, State = #consumer_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
