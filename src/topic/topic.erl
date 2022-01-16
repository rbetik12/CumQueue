%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(topic).

-behaviour(gen_server).

-export([start/1, stop/0]).
-export([test_push_message/1]).
-export([push_message/2, new_consumer/3, broadcast/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-include("../include/message.hrl").

-record(state, {name, consumers, queue = [], queueSize = 0}).

test_push_message(TopicPid) ->
  Message = #message{id = 1, topic = testname, message_payload = <<"some message">>},
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
  lager:log(debug, self(), "Started topic with name ~p~n", [TopicName]),
  open_dets(TopicName, TopicName ++ ".ets", bag),
  gen_server:start_link(?MODULE, [TopicName], []).

stop() ->
  % TODO close dets
  gen_server:call(?MODULE, stop).

init([Name]) ->
  topic_manager:update_topic_pid(Name, self()),
  {ok, #state{name = Name, consumers = sets:new()}}.


handle_call({push_message, Message}, _From, #state{queue = Queue, queueSize = QueueSize, consumers = Consumers} = State) ->
  NewState = State#state{
    queue = [Message | Queue],
    queueSize = QueueSize + 1
  },
  spawn_link(?MODULE, broadcast, [Consumers, Message]),
  {reply, {ok, {}}, NewState};

handle_call({new_consumer, ConsumerPid, ReplyType}, _From, #state{consumers = Consumers, queue = Queue, queueSize = QueueSize} = State) ->
  NewState = State#state{
    consumers = sets:add_element(ConsumerPid, Consumers)
  },
  if
    QueueSize =:= 0 ->
      Messages = [],
      LastMessageId = undefined;
    true ->
      case ReplyType of
        {all} ->
          Messages = get_all_messages(Queue);
        {by_offset, OffsetId} ->
          Messages = get_messages_by_offset(Queue, OffsetId)
      end,
      LastMessage = lists:nth(1, Queue),
      LastMessageId = LastMessage#message.id
  end,
  {reply, {ok, {Messages, LastMessageId}}, NewState};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State}.

handle_cast({restore_state}, #state{name = TopicName} = State) ->
  NewState = State#state{
    consumers = sets:from_list(proplists:get_all_values(consumer, dets:lookup(TopicName, consumer))),
    queue = dets:lookup(TopicName, message)
  },
  {noreply, NewState}.

handle_info(_Info, #state{} = State) ->
  {noreply, State}.

terminate(_Reason, #state{}) ->
  ok.

code_change(_OldVsn, #state{} = State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

open_dets(TabName, File, TabType) ->
  IsExists = filelib:is_file(File),
  case dets:open_file(TabName, [{file, File}, {type, TabType}]) of
    {ok, TabName} ->
      case IsExists of
        true ->
          restore_state();
        false ->
          init_dets(TabName)
      end;
    {error,Reason} ->
      lager:log(error, self(), "Error opening dets table. Reason: ~p~n", [Reason]),
      exit(eDetsOpen)
  end.

restore_state() ->
  gen_server:cast(self(), {restore_state}).

init_dets(TabName) ->
  dets:insert(TabName, {topic_name, TabName}).

get_all_messages(Queue) ->
  Queue.

get_messages_by_offset(Queue, OffsetId) ->
  [Message || Message <- Queue, Message#message.id > OffsetId].

broadcast(Consumers, Message) ->
  lists:foreach(
    fun(ConsumerPid) ->
      consumer:send_message(ConsumerPid, Message)
    end,
    sets:to_list(Consumers)).