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
  content_types_provided/2,
  register_json/2,
  allowed_methods/2,
  content_types_accepted/2, post_json/2]).

init(Req0, Opts) ->
  {cowboy_rest, Req0, Opts}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, register_json}
  ], Req, State}.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"GET">>], Req, State}.

register_json(Req, State) ->
  Has = cowboy_req:has_body(Req),
  io:format("~w~n~n", [Has]),
  io:format("~w~n~w~n", [Req, State]),
  {<<"{\"rest\": \"Oh shit\"}">>, Req, State}.

content_types_accepted(Req, State) ->
  case cowboy_req:method(Req) of
    <<"POST">> ->
      Accepted = {[{<<"application/json">>, post_json}], Req, State};
    <<"PUT">> ->
      Accepted = {[{<<"application/json">>, post_json}], Req, State}
  end,
  Accepted.

post_json(Req, State) ->
  {ok, Data, Req1} = cowboy_req:read_body(Req),
  io:format("~w~n", [Data]),
  {true, Req1, State}.