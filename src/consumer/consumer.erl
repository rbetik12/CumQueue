%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(consumer).

-behaviour(gen_server).

-include("../include/message.hrl").

-export([start_link/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {url, message = #message{}}).

%% NOTE Пример отправки сообщения через consumer
%%{ok, CPID} = consumer:start_link("http://localhost:9000", #message{}),
%%gen_server:call(CPID, send_message),
%%gen_server:call(CPID, stop),

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Url, Message) ->
  io:format("Hello from consumer!~n"),
  gen_server:start_link(?MODULE, [Url, Message], []).

stop() ->
  gen_server:call(?MODULE, stop).

init([Url, #message{} = Message]) ->
  lager:log(debug, self(), "Started consumer for url: ~p~n", [Url]),
  {ok, #state{url = Url, message = Message}}.

handle_call(send_message, _From, State = #state{url = Url, message = Message}) ->
  Body = jsone:encode(Message),
  Request = {Url, [], "application/json", Body},
  lager:log(debug, self(), "Sending request for url ~p with body ~p~n", [Url, Body]),
  case httpc:request(post, Request, [], []) of
    {error, Error} -> lager:log(debug, self(), "Can't sent requesr to ~p, error: ~p", [Url, Error]);
    {ok, Result} -> lager:log(debug, self(), "Sended request to ~p, result: ~p", [Url, Result])
  end,
  {reply, ok, State};
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
