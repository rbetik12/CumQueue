%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(topic_manager).

-behaviour(gen_server).

-export([start/0, stop/0, new_topic/1, get_topic_pid/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {topics = #{}}).

%%%===================================================================
%%% API
%%%===================================================================

get_topic_pid(Name) ->
    gen_server:call(topic_manager, {get_topic_pid, Name}).

new_topic(Name) ->
    gen_server:call(topic_manager, {new_topic, Name}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start() ->
    io:format("Hello from topic_manager~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

init([]) ->
    {ok, #state{}}.


handle_call({get_topic_pid, Name}, _From, #state{topics = Topics} = State) ->
    case maps:get(Name, Topics, badkey) of
        badkey ->
            {reply, {notfound, {Name}}, State};
        TopicPid ->
            {reply, {ok, {TopicPid}}, State}
    end;

handle_call({new_topic, Name}, _From, #state{topics = Topics}) ->
    ChildSpecs = #{
        id => topic,
        start => {topic, start, []},
        restart => transient,
        shutdown => 2000,
        type => worker,
        modules => [topic]
    },
    {ok, TopicPid} = supervisor:start_child(topic_sup, ChildSpecs),
    NewState = #state{topics = maps:put(Name, TopicPid, Topics)},
    {reply, {ok, {TopicPid}}, NewState};

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
