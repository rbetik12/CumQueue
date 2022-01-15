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
  io:format("init~n"),
  {cowboy_rest, Req0, Opts}.

allowed_methods(Req, State) ->
  io:format("methods~n"),
  {[<<"POST">>, <<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
  io:format("types~n"),
  case cowboy_req:method(Req) of
    <<"POST">> ->
      Accepted = {[{<<"application/json">>, post_json}], Req, State};
    <<"PUT">> ->
      Accepted = {[{<<"application/json">>, put_json}], Req, State}
  end,
  Accepted.

post_json(Req, State) ->
  io:format("post_json: ~n"),
  {ok, Data, Req1} = cowboy_req:read_body(Req),
  Req2 = cowboy_req:set_resp_body(Data, Req1),
  Req3 = cowboy_req:reply(200, Req2),
  {stop, Req3, State}.
