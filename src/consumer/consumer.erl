%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(consumer).

-behaviour(gen_server).

-include("../include/message.hrl").

-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, send_message/2]).

-define(SERVER, ?MODULE).

-record(state, {url}).

%% NOTE Пример отправки сообщения через consumer
%%{ok, {CPID}} = gen_server:call(consumer_registrar, {new_consumer, #{callback_url => "http://localhost:9000"}}),
%%gen_server:call(CPID, {send_message, #message{}}),

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Url) ->
  io:format("Hello from consumer!~n"),
  gen_server:start_link(?MODULE, [Url], []).

stop() ->
  gen_server:call(?MODULE, stop).

init([Url]) ->
  lager:log(debug, self(), "Started consumer for url: ~p~n", [Url]),
  {ok, #state{url = Url}}.

handle_call({send_message, Message}, _From, State = #state{url = Url}) ->
  Body = jsone:encode(utils:message_to_json(Message)),
  Request = {Url, [], "application/json", Body},
  lager:log(debug, self(), "Sending request for url ~p with body ~p~n", [Url, Body]),
  case httpc:request(post, Request, [], []) of
    {error, Error} -> lager:log(debug, self(), "Can't sent requesr to ~p, error: ~p", [Url, Error]);
    {ok, Result} ->
      ok =
      lager:log(debug, self(), "Sended request to ~p, result: ~p", [Url, Result])
  end,
  case consumer_registrar:inc_group(Message#message.id, Url) of
    cannot_find_group -> lager:log(debug, self(), "Cannot inc group id with url ~p", [Url]);
    ok -> lager:log(debug, self(), "Incremented group id with url ~p", [Url])
  end,
  {reply, {ok, {}}, State};
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

send_message(Pid, Message) ->
  gen_server:call(Pid, {send_message, Message}).
