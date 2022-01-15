%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(producer).

-behaviour(gen_server).

-include("../include/message.hrl").

-export([start/1, stop/0, new_message/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(producer_state, {topic_pid = lcnt:pid(node(), 0, 0)}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start(TopicPid) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [TopicPid], []).

stop() ->
  gen_server:call(?MODULE, stop).

new_message(#message{message_header = #message_header{topic = TopicName}} = Message) ->
  {ok, Pid} = producer_registrar:get_producer_pid(TopicName),
  gen_server:call(Pid, {new_message, Message}).

init([TopicPid]) ->
  lager:log(debug, self(), "Started producer for topic with pid: ~p~n", [TopicPid]),
  {ok, #producer_state{topic_pid = TopicPid}}.

handle_call({new_message, #message{} = Message}, _From, State = #producer_state{topic_pid = TopicPid}) ->
  lager:log(debug, self(), "~p~n", [Message]),
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
