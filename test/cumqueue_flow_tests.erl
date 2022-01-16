%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jan 2022 02:42
%%%-------------------------------------------------------------------
-module(cumqueue_flow_tests).
-author("prikotav").

-include_lib("eunit/include/eunit.hrl").
-include("include/test_env.hrl").

setup_test_env() ->
  cumqueue_app:start_and_setup_deps(),
  producer_registrar_sup:start(testing),
  producer_sup:start(),
  cumqueue_app:start_http_server(8080).

register_one_producer_test() ->
  setup_test_env(),
  Body = "{\"topicName\":\"test topic 1\"}",
  Request = {string:concat(?CUMKA_HOST, "/producerRegistration"), [], "application/json", Body},
  {ok, {{_, AnswerCode, ReasonPhrase}, _, _}} = httpc:request(post, Request, [], []),
  ?assert(AnswerCode == 200),
  ?assert(ReasonPhrase == "OK").