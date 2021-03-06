%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(topic).

-behaviour(gen_server).

-export([start/1, stop/1]).
-export([test_push_message/2]).
-export([push_message/2, new_consumer/3, broadcast/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-include("../include/message.hrl").

-record(state, {name, consumers = sets:new(), queue = []}).

test_push_message(TopicPid, Id) ->
  Message = #message{id = Id, topic = testname, message_payload = <<"some message">>},
  push_message(TopicPid, Message).


%%%===================================================================
%%% API
%%%===================================================================

% Returns {ok, {}}
push_message(TopicPid, Message) ->
  %TODO Timeout can be here (also pid inconsistency)
  gen_server:call(TopicPid, {push_message, Message}).

% Returns {ok, {Messages, LastMessageId}}
new_consumer(TopicPid, ConsumerPid, ReplyType) ->
  gen_server:call(TopicPid, {new_consumer, ConsumerPid, ReplyType}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start(TopicName) ->
  gen_server:start_link(?MODULE, [TopicName], []).

stop(TopicPid) ->
  gen_server:call(TopicPid, stop).

init([TopicName]) ->
  InitState = #state{name = TopicName},
  topic_manager:update_topic_pid(TopicName, self()),
  case utils:is_dets_exists(TopicName, TopicName ++ ".ets", bag) of
    true ->
      State = restore_state(TopicName, InitState);
    false ->
      init_dets(TopicName),
      State = InitState
  end,
  lager:log(debug, self(), "Started topic with name ~p on pid ~p~n", [TopicName, self()]),
  {ok, State}.


handle_call({push_message, Message}, _From, #state{name = TopicName, queue = Queue, consumers = Consumers} = State) ->
  NewState = State#state{
    queue = [Message | Queue]
  },
  dets:insert(TopicName, Message),
  spawn_link(?MODULE, broadcast, [Consumers, Message]),
  {reply, {ok, {}}, NewState};

handle_call({new_consumer, ConsumerPid, ReplyType}, _From, #state{name = TopicName, consumers = Consumers, queue = Queue} = State) ->
  NewState = State#state{
    consumers = sets:add_element(ConsumerPid, Consumers)
  },
  dets:insert(TopicName, {consumer, ConsumerPid}),
  if
    length(Queue) =:= 0 ->
      Messages = [],
      LastMessageId = undefined;
    true ->
      case ReplyType of
        {all} ->
          Messages = Queue;
        {by_offset, OffsetId} ->
          Messages = get_messages_by_offset(Queue, OffsetId)
      end,
      LastMessage = lists:nth(1, Queue),
      LastMessageId = LastMessage#message.id
  end,
  {reply, {ok, {Messages, LastMessageId}}, NewState};

handle_call(stop, _From, #state{name = TopicName} = State) ->
  dets:close(TopicName),
  {stop, normal, stopped, State}.

handle_cast(_Request, #state{} = State) ->
  {noreply, State}.

handle_info(_Info, #state{} = State) ->
  {noreply, State}.

terminate(_Reason, #state{}) ->
  ok.

code_change(_OldVsn, #state{} = State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

restore_state(TabName, InitState) ->
  InitState#state{
    consumers = sets:from_list(proplists:get_all_values(consumer, dets:lookup(TabName, consumer))),
    queue = lists:reverse(dets:lookup(TabName, message))
  }.

init_dets(TabName) ->
  dets:insert(TabName, {topic_name, TabName}).

get_messages_by_offset(Queue, OffsetId) ->
  [Message || Message <- Queue, Message#message.id > OffsetId].

broadcast(Consumers, Message) ->
  lists:foreach(
    fun(ConsumerPid) ->
      consumer:send_message(ConsumerPid, Message)
    end,
    sets:to_list(Consumers)).