%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(consumer_registrar).

-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3,
  register_consumer/1,
  new_consumer/3,
  inc_group/2]).

-define(SERVER, ?MODULE).

-record(state, {consumers = maps:new(), groups = maps:new()}).
-record(consumer_state, {topic, group}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

register_consumer(ConsumerDataMap) when is_map(ConsumerDataMap) ->
  gen_server:call(consumer_registrar, {register_consumer, ConsumerDataMap}).

start() ->
  io:format("Hello from consumer_reg!~n"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

init([]) ->
  {ok, #state{}}.

handle_call({inc_group, #{new_msg_id := NewMsgId, url := Url}},
    _From, State = #state{consumers = Consumers, groups = Groups}) ->
  case maps:get(Url, Consumers, notfound) of
    notfound ->
      {reply, cannot_find_group, State};
    ConsumerState ->
      {reply, ok, State#state{
      groups = Groups#{ConsumerState#consumer_state.group := NewMsgId}
      }}
  end;
handle_call({register_consumer, #{callback_url := Url, topic := Topic, group := Group}},
    _From,
    State = #state{consumers = Consumers, groups = Groups}) ->
  case maps:get(Url, Consumers, badkey) of
    %% Consumer does not exists
    badkey ->
      %% Try to connect to topic
      case topic_manager:new_topic(Topic) of
        %% Created topic
        {ok, _} ->
          case new_consumer(Url, Topic, {all}) of
            {ok, {Messages, LastMsgId}} ->
              {reply, {ok, {Messages}}, State#state{
                consumers = Consumers#{Url => #consumer_state{topic = Topic, group = Group}},
                groups = Groups#{Group => LastMsgId}}
              };
            {notfound, {TopicName}} ->
              {reply, {notfound, {TopicName}}, State}
          end;
        %% Found exists topic
        {already_exists, _} ->
          GroupId = case maps:get(Group, Groups, notfound) of
                      notfound -> 0;
                      Id -> Id
                    end,
          case GroupId of
            %% Return all data from topic
            0 ->
              case new_consumer(Url, Topic, {all}) of
                {ok, {Messages, LastMsgId}} ->
                  {reply, {ok, {Messages}}, State#state{
                    consumers = Consumers#{Url => #consumer_state{topic = Topic, group = Group}},
                    groups = Groups#{Group => LastMsgId}}
                  };
                {notfound, {TopicName}} ->
                  {reply, {notfound, {TopicName}}, State}
              end;
            %% Return data after GroupId from topic
            LastGroupId ->
              case new_consumer(Url, Topic, {by_offset, LastGroupId}) of
                {ok, {Messages, LastMsgId}} ->
                  {reply, {ok, {Messages}}, State#state{
                    consumers = Consumers#{Url => #consumer_state{topic = Topic, group = Group}},
                    groups = Groups#{Group := LastMsgId}}
                  };
                {notfound, {TopicName}} ->
                  {reply, {notfound, {TopicName}}, State}
              end
          end
      end;
    %% Consumer already exists
    _ ->
      lager:log(debug, self(), "Consumer with URL ~p for topic: ~p", [Url, Topic]),
      {reply, {ok, {already_exists}}, State}
  end;
handle_call(stop, _From, Tab) ->
  lager:log(info, self(),"stop"),
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

new_consumer(Url, Topic, ReplyType) ->
  lager:log(info, self(),"spawning new consumer"),
  {ok, Pid} = consumer:start_link(Url),
  lager:log(info, self(),"spawned new consumer"),
  topic_manager:new_consumer(Topic, Pid, ReplyType).

new_consumer(Url, Topic, ReplyType, State, Group) ->
  lager:log(info, self(),"spawning new consumer"),
  {ok, Pid} = consumer:start_link(Url),
  lager:log(info, self(),"spawned new consumer"),
  Consumers = State#state.consumers,
  Groups = State#state.groups,
  case topic_manager:new_consumer(Topic, Pid, ReplyType) of
    {ok, {Messages, LastMsgId}} ->
      {reply, {ok, {Messages}}, State#state{
        consumers = Consumers#{Url => #consumer_state{topic = Topic, group = Group}},
        groups = Groups#{Group := LastMsgId}}
      };
    {notfound, {TopicName}} ->
      {reply, {notfound, {TopicName}}, State}
  end.

inc_group(NewMsgId, Url) ->
  gen_server:call(?MODULE, {inc_group, #{new_msg_id => NewMsgId, url => Url}}),
  lager:log(info, self(), "new GroupId: ~p in Group with url: ~p", [NewMsgId, Url]).
