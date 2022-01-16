%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(topic_manager).

-behaviour(gen_server).

-export([start/0, stop/0]).
-export([new_topic/1, new_consumer/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {topics = #{}}).

%%%===================================================================
%%% API
%%%===================================================================

% Returns {ok, {TopicPid}} | {already_exists, {TopicPid}}
new_topic(Name) ->
    gen_server:call(?MODULE, {new_topic, Name}).

% ReplyType = {all} | {by_offset, term()}
% Returns {ok, {Messages, LastMessageId}} | {notfound, {TopicName}}
new_consumer(TopicName, ConsumerPid, ReplyType) ->
    gen_server:call(?MODULE, {new_consumer, TopicName, ConsumerPid, ReplyType}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start() ->
    lager:log(debug, self(), "Started topic manager", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

init([]) ->
    {ok, #state{}}.

handle_call({new_topic, TopicName}, _From, #state{topics = Topics} = State) ->
    case maps:find(TopicName, Topics) of
        error ->
            {ok, TopicPid} = supervisor:start_child(topic_sup, [TopicName]),
            NewState = State#state{topics = maps:put(TopicName, TopicPid, Topics)},
            {reply, {ok, {TopicPid}}, NewState};
        {ok, TopicPid} ->
            {reply, {already_exists, {TopicPid}}, State}
    end;

handle_call({new_consumer, TopicName, ConsumerPid, ReplyType}, _From, #state{topics = Topics} = State) ->
    case maps:find(TopicName, Topics) of
        error ->
            Reply = {notfound, {TopicName}};
        {ok, TopicPid} ->
            Reply = topic:new_consumer(TopicPid, ConsumerPid, ReplyType)
    end,
    {reply, Reply, State};

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
