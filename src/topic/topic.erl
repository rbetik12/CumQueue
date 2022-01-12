%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(topic).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(topic_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  io:format("Hello from topic!~n"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #topic_state{}}.

handle_call(_Request, _From, State = #topic_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #topic_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #topic_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #topic_state{}) ->
  ok.

code_change(_OldVsn, State = #topic_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
