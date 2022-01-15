%%%-------------------------------------------------------------------
%%% @author salvoroni
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2022 2:59 AM
%%%-------------------------------------------------------------------
-module(producer_register_controller).
-author("salvoroni").

%% API
-export([init/2,
  allowed_methods/2,
  content_types_accepted/2, post_json/2]).

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
  {ok, Data, Req1} = cowboy_req:read_body(Req),

  Json = jsone:decode(Data),
  TopicName = binary_to_list(maps:get(<<"topicName">>, Json)),
  %TODO Handle timeout
  producer_registrar:register_producer(TopicName),

  Req3 = cowboy_req:reply(200, Req1),
  {stop, Req3, State}.
