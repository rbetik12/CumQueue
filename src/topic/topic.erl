%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(topic).

-behaviour(gen_server).

-export([start/1, stop/0]).
-export([get_all_messages/1, get_messages_by_offset/2, push_message/2, new_consumer/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-include("../include/message.hrl").

-record(state, {name, consumers, queue = [], queueSize = 0}).

%%%===================================================================
%%% API
%%%===================================================================

% called by producer
push_message(TopicPid, Message) ->
  %TODO Timeout can be here (also pid inconsistency)
  gen_server:call(TopicPid, {push_message, Message}).

% called by consumer -> topic_manager
new_consumer(TopicPid, ConsumerPid, ReplyType) ->
  gen_server:call(TopicPid, {new_consumer, ConsumerPid, ReplyType}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start(Name) ->
  lager:log(debug, self(), "Started topic with name ~p~n", [Name]),
  gen_server:start_link(?MODULE, [Name], []).

stop() ->
  gen_server:call(?MODULE, stop).

init([Name]) ->
  {ok, #state{name = Name, consumers = sets:new()}}.


handle_call({push_message, Message}, _From, #state{queue = Queue, queueSize = QueueSize, consumers = Consumers} = State) ->
  NewState = State#state{
    queue = [Message | Queue],
    queueSize = QueueSize + 1
  },
  lists:foreach(
    fun(ConsumerPid) ->
      gen_server:call(ConsumerPid, {new_message})
    end,
    sets:to_list(Consumers)),
  {reply, {ok, {}}, NewState};

handle_call({new_consumer, ConsumerPid, ReplyType}, _From, #state{consumers = Consumers, queue = Queue} = State) ->
  NewState = State#state{
    consumers = sets:add_element(ConsumerPid, Consumers)
  },
  case ReplyType of
    {all} ->
      Messages = get_all_messages(State);
    {by_offset, OffsetId} ->
      Messages = get_messages_by_offset(Queue, OffsetId)
  end,
  LastMessageId = lists:nth(1, Queue),
  {reply, {ok, {Messages, LastMessageId}}, NewState};

handle_call(stop, _From, State) ->
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

get_all_messages(Queue) ->
  Queue.

get_messages_by_offset(Queue, OffsetId) ->
  [Message || Message <- Queue, Message#message.id > OffsetId].
