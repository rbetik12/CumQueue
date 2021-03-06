-module(producer_registrar).

-behaviour(gen_server).

-export([start/1, stop/0, register_producer/1, get_producer_pid/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {producers = maps:new(), mode = production}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

register_producer(ProducerName) ->
  %TODO Timeout can be get here
  gen_server:call(producer_registrar, {register_producer, ProducerName}).

get_producer_pid(TopicName) ->
  %TODO Timeout can be get here
  gen_server:call(producer_registrar, {get_producer_pid, TopicName}).

start(Mode) ->
  lager:log(info, self(), "Started producer_reg in mode: ~p~n", [Mode]),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Mode], []).

stop() ->
  gen_server:call(?MODULE, stop).

init([Mode]) ->
  {ok, #state{mode = Mode}}.

handle_call({register_producer, TopicName}, _From, #state{producers = Producers, mode = production}) ->
  lager:log(debug, self(), "Registring producer with name: ~p...~n", [TopicName]),
  %TODO We can do that in separate thread
  case maps:get(TopicName, Producers, badkey) of
    badkey ->
      {_, {TopicPid}} = topic_manager:new_topic(TopicName),
      start_producer(TopicPid, TopicName, Producers, production);
    ProducerPid ->
      %TODO Change answer code for case when producer already exists
      lager:log(debug, self(), "Producer for topic: ~p already exists~n", [TopicName]),
      {reply, {ok, {ProducerPid}}, #state{producers = Producers}}
  end;

handle_call({register_producer, TopicName}, _From, #state{producers = Producers, mode = testing}) ->
  case maps:get(TopicName, Producers, badkey) of
    badkey ->
      start_producer(lcnt:pid(node(), 0, 0), TopicName, Producers, testing);
    ProducerPid ->
      {reply, {ok, {ProducerPid}}, #state{producers = Producers, mode = testing}}
  end;

handle_call({get_producer_pid, TopicName}, _From, State = #state{producers = Producers}) ->
  case maps:get(TopicName, Producers, badkey) of
    badkey ->
      {reply, {notfound, TopicName}, State};
    ProducerPid ->
      {reply, {ok, ProducerPid}, State}
  end;

handle_call(stop, _From, Tab) ->
  {stop, normal, stopped, Tab}.

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

handle_info(_Info, State = #state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_producer(TopicPid, TopicName, Producers, Mode) ->
  {ok, ProducerPid} = supervisor:start_child(producer_sup, [TopicPid]),
  Producers1 = maps:put(TopicName, ProducerPid, Producers),
  lager:log(debug, self(), "Successfully created producer for topic ~p~n", [TopicName]),
  {reply, {ok, {ProducerPid}}, #state{producers = Producers1, mode = Mode}}.