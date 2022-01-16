%%%-------------------------------------------------------------------
%%% @author salvoroni
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jan 2022 4:33 PM
%%%-------------------------------------------------------------------
-module(tcp_handler).
-author("salvoroni").

-include("../include/message.hrl").

-behavior(gen_server).

-record(state, {}).

%% API
-export([init/1, handle_call/3, handle_cast/2, start/0]).

start() ->
  lager:log(info, self(), "starting tcp listener worker"),
  gen_server:start_link(?MODULE, init, []).

init(_) ->
  lager:log(info, self(), "try listening to localhost:5678"),
  {ok, LSock} = gen_tcp:listen(5678, [binary, {packet, 0}, {active, false}]),
  lager:log(info, self(), "listening to localhost:5678"),
  spawn_link(fun () -> tcp_loop(LSock) end),
  {ok, #state{}}.

tcp_loop(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  tcp_inner_loop(LSock, Sock, []).
%%  lager:log(info, self(), "received from socker ~p raw data", [binary_to_list(Bin)]),
%%  ok = gen_tcp:close(Sock).

tcp_inner_loop(LSock, Sock, Bs) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, B} ->
      tcp_inner_loop(LSock, Sock, [B|Bs]);
    _ ->
      handle_data(Bs),
      ok = gen_tcp:close(Sock),
      tcp_loop(LSock)
  end.

handle_data(Data) ->
  Data1 = string:trim(Data, trailing, "\n"),
  lager:log(info, self(), "receiver data: ~p", [Data1]),
  JsonData = jsone:decode(Data1),
  lager:log(info, self(), "receiver data: ~p", [Data1]),
  %% Choose what to do
  case maps:get(<<"action">>,JsonData) of
    <<"register_consumer">> -> register_consumer(maps:get(<<"data">>, JsonData));
    <<"register_producer">> -> register_producer(maps:get(<<"data">>, JsonData));
    <<"new_message">> -> new_message(maps:get(<<"data">>, JsonData));
    Other -> lager:log(debug, self(), "Unsupported action ~w", [Other])
  end.

new_message(JsonData) ->
  case producer:new_message(#message{
      topic = binary_to_list(maps:get(<<"topic">>, JsonData)),
      message_payload = maps:get(<<"data">>, JsonData)}) of
    {ok, _} -> <<"{\"status\": \"OK\"}">>;
    {notfound, _} -> <<"{\"status\": \"Not found\"}">>;
    _ -> <<"{\"status\": \"Registration producer error\"}">>
  end
.

register_producer(JsonData) ->
  case producer_registrar:register_producer(binary_to_list(maps:get(<<"topic">>, JsonData))) of
    {ok, _} -> <<"{\"status\": \"OK\"}">>;
    _ -> <<"{\"status\": \"Registration producer error\"}">>
  end.

register_consumer(JsonData) ->
  case consumer_registrar:register_consumer(#{
    callback_url => maps:get(<<"url">>, JsonData),
    topic => binary_to_list(maps:get(<<"topic">>, JsonData)),
    group => maps:get(<<"group">>, JsonData)
  }) of
    {ok, {empty_topic}} -> <<"{\"status\": \"OK\"}">>;
    {ok, {already_exists}} -> <<"{\"status\": \"already exists\"}">>;
    {ok, {Messages}} -> jsone:encode(utils:messages_to_json(Messages));
    _ -> <<"{\"status\": \"Cannot register consumer\"}">>
  end.

handle_call(Request, From, State) ->
  erlang:error(not_implemented).

handle_cast(Request, State) ->
  erlang:error(not_implemented).