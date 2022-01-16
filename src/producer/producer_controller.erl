%%%-------------------------------------------------------------------
%%% @author salvoroni
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2022 2:59 AM
%%%-------------------------------------------------------------------
-module(producer_controller).
-author("salvoroni").

-include("../include/message.hrl").

%% API
-export([init/2,
  allowed_methods/2,
  content_types_accepted/2,
  post_json/2
]).

init(Req0, Opts) ->
  {cowboy_rest, Req0, Opts}.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
  case cowboy_req:method(Req) of
    <<"POST">> ->
      Accepted = {[{<<"application/json">>, post_json}], Req, State};
    <<"PUT">> ->
      Accepted = {[{<<"application/json">>, put_json}], Req, State}
  end,
  Accepted.

post_json(Req, State) ->
  ReqPath = lists:foldl(
    fun(Item, Path) ->
      string:concat(Path, binary_to_list(Item))
    end, "", cowboy_req:path_info(Req)),
  case ReqPath of
    "newMessage" ->
      %TODO We can call new_message in separate thread
      new_message(Req, State)
  end.

new_message(Req, State) ->
  {ok, Data, Req1} = cowboy_req:read_body(Req),

  Message = get_message(Data),
  case producer:new_message(Message) of
    {ok, {_}} ->
      Req2 = cowboy_req:reply(200, Req1),
      {stop, Req2, State};
    {notfound, {_}} ->
      Req2 = cowboy_req:reply(400, Req1),
      {stop, Req2, State}
  end.

get_message(Data) ->
  Json = jsone:decode(Data),
  TopicName = binary_to_list(maps:get(<<"topicName">>, Json)),
  #message{
    topic = TopicName,
    message_payload = maps:get(<<"data">>, Json)
  }.

