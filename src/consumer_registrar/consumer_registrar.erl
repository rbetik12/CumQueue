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
  register_consumer/1]).

-define(SERVER, ?MODULE).

-record(state, {consumers = maps:new(), groups = maps:new()}).

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

handle_call({register_consumer, #{callback_url := Url, topic := Topic, group := Group} = ConsumerDataMap},
    _From,
    State = #state{consumers = Consumers, groups = Groups}) ->
  case maps:get(Url, Consumers, badkey) of
    %% Consumer does not exists
    badkey ->
      GroupId = case maps:get(Group, Groups, notfound) of
                  notfound -> 0;
                  {ok, Id} -> Id
                end,
      %% Try to connect to topic
      case topic_manager:get_topic_pid(Topic) of
        %% No such topic
        {notfound, _} ->
          topic_manager:new_topic(Topic),
          {reply, {ok, {empty_topic}}, State#state{
            consumers = Consumers#{Url => Topic}
          }};
        %% Found topic
        {ok, _} ->
          case GroupId of
            %% Return all data from topic
            0 ->
              %%Messages = topic_manager:get_all(Topic),
              {Messages, MsgId} = not_implemented, not_implemented,
              {reply, {ok, {Messages}}, State#state{
                consumers = Consumers#{Url => Topic},
                groups = Groups#{Group => MsgId}}
              };
            %% Return data after GroupId from topic
            GroupId ->
              %%Messages = topic_manager:get_from(Topic, GroupId),
              {Messages, MsgId} = not_implemented, not_implemented,
              {reply, {ok, {Messages}}, State#state{
                consumers = Consumers#{Url => Topic},
                groups = Groups#{Group := MsgId}}
              }
          end
      end;
    %% Consumer already exists
    _ ->
      lager:log(debug, self(), "Consumer with URL ~p for topic: ~p", [Url, Topic]),
      {reply, {ok, {already_exists}}, State}
  end;
handle_call({new_consumer, #{callback_url := Url} = ConsumerDataMap},
    _From, State) ->
  {ok, CPID} = consumer:start_link(Url),
  {reply, {ok, {CPID}}, State};
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State};
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
