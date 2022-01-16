%%%-------------------------------------------------------------------
%%% @author salvoroni
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2022 2:53 AM
%%%-------------------------------------------------------------------
-module(consumer_register_controller).
-author("salvoroni").

%% API
-export([init/2,
  register_consumer/2,
  allowed_methods/2,
  content_types_accepted/2]).


%% TODO апи для главного по топикам для отправки сообщения

init(Req0, Opts) ->
  {cowboy_rest, Req0, Opts}.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  case cowboy_req:method(Req) of
    <<"POST">> ->
      {[{<<"application/json">>, register_consumer}], Req, State}
  end.

register_consumer(Req, State) ->
  {ok, Data, Req1} = cowboy_req:read_body(Req),
  JsonData = jsone:decode(Data),
  case consumer_registrar:register_consumer(#{
    callback_url => maps:get(<<"url">>, JsonData),
    topic => maps:get(<<"topic">>, JsonData),
    group => maps:get(<<"group">>, JsonData)
  }) of
    {ok, {empty_topic}} -> Body = <<"{\"status\": \"OK\"}">>;
    {ok, {already_exists}} -> Body = <<"{\"status\": \"already exists\"}">>;
    {ok, {Messages}} -> Body = jsone:encode(utils:messages_to_json(Messages));
    _ -> Body = <<"{\"status\": \"Cannot register consumer\"}">>
  end,
  Req2 = cowboy_req:set_resp_headers(#{
    <<"content-type">> => <<"application/json">>
  }, Req1),
  Req3 = cowboy_req:reply(200, cowboy_req:resp_headers(Req2), Body, Req1),
  {stop, Req3, State}.