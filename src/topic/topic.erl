%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(topic).

-behaviour(gen_server).

-export([start/1, stop/0, get_messages/2, push_message/2, new_consumer/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-include("../include/message.hrl").

-record(state, {name, consumers, queue = [], queueSize = 0}).

%%%===================================================================
%%% API
%%%===================================================================

get_messages(TopicPid, Amount) ->
  gen_server:call(TopicPid, {get_messages, Amount}).

push_message(TopicPid, Message) ->
  %TODO Timeout can be here (also pid inconsistency)
  gen_server:call(TopicPid, {push_message, Message}).

new_consumer(TopicPid, ConsumerPid) ->
  gen_server:call(TopicPid, {new_consumer, ConsumerPid}).

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


handle_call({get_messages, MessageAmount}, _From, #state{queue = Queue} = State) ->
  Messages = nth_head(MessageAmount, Queue),
  {reply, {ok, {Messages}}, State};

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

handle_call({new_consumer, ConsumerPid}, _From, #state{consumers = Consumers} = State) ->
  NewState = State#state{
    consumers = sets:add_element(ConsumerPid, Consumers)
  },
  {reply, {ok, {}}, NewState};

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

nth_head(Amount, List) ->
  nth_head(Amount, List, []).

nth_head(0, _, Res) ->
  Res;
nth_head(Amount, [Elem | List], Res) ->
  nth_head(Amount - 1, List, [Elem | Res]).