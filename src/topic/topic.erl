%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(topic).

-behaviour(gen_server).

-export([start/1, stop/0, get_messages/2, push_message/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-record(state, {name, consumers = [], queue = [], queueSize = 0}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start(Name) ->
  lager:log(debug, self(), "Started topic with name ~p~n", [Name]),
  gen_server:start_link(?MODULE, [Name], []).

stop() ->
  gen_server:call(?MODULE, stop).

push_message(Pid, Message) ->
  %TODO Timeout can be here (also pid inconsistency)
  gen_server:call(Pid, {push, Message}).

get_messages(Amount, Queue) ->
  get_messages(Amount, Queue, []).

get_messages(0, _, Res) ->
  Res;

get_messages(Amount, [Message | Queue], Res) ->
  get_messages(Amount - 1, Queue, [Message | Res]).

init([Name]) ->
  {ok, #state{name = Name}}.

handle_call({get, MessageAmount}, _From, #state{queue = Queue} = State) ->
  Messages = get_messages(MessageAmount, Queue),
  {reply, {ok, {Messages}}, State};

handle_call({push, Message}, _From, #state{queue = Queue, queueSize = QueueSize}) ->
  NewState = #state{
    queue = [Message | Queue],
    queueSize = QueueSize + 1
  },
  {reply, ok, NewState};

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
