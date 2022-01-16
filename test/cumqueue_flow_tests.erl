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
  application:start(cumqueue).

teardown_test_env() ->
  application:stop(cumqueue).

register_producer(TopicName) ->
  Body = lists:concat(["{\"topicName\":\"", TopicName, "\"}"]),
  Request = {string:concat(?CUMKA_HOST, "/producerRegistration"), [], "application/json", Body},
  {ok, {{_, 200, _}, _, _}} = httpc:request(post, Request, [], []).

register_one_producer_test() ->
  setup_test_env(),
  Body = "{\"topicName\":\"test topic 1\"}",
  Request = {string:concat(?CUMKA_HOST, "/producerRegistration"), [], "application/json", Body},
  {ok, {{_, AnswerCode, ReasonPhrase}, _, _}} = httpc:request(post, Request, [], []),
  ?assert(AnswerCode == 200),
  ?assert(ReasonPhrase == "OK").

register_two_producers_test() ->
  Body = "{\"topicName\":\"test topic 1\"}",
  Body1 = "{\"topicName\":\"test topic 2\"}",
  Request = {string:concat(?CUMKA_HOST, "/producerRegistration"), [], "application/json", Body},
  Request1 = {string:concat(?CUMKA_HOST, "/producerRegistration"), [], "application/json", Body1},
  {ok, {{_, AnswerCode, ReasonPhrase}, _, _}} = httpc:request(post, Request, [], []),
  {ok, {{_, AnswerCode1, ReasonPhrase1}, _, _}} = httpc:request(post, Request1, [], []),
  ?assert(AnswerCode == 200),
  ?assert(ReasonPhrase == "OK"),
  ?assert(AnswerCode == AnswerCode1),
  ?assert(ReasonPhrase == ReasonPhrase1).

try_to_write_message_without_registering_producer_test() ->
  Body = "{\"topicName\":\"topic doesn't exist\", \"data\":\"kek\"}",
  Request = {string:concat(?CUMKA_HOST, "/producer/newMessage"), [], "application/json", Body},
  {ok, {{_, AnswerCode, ReasonPhrase}, _, _}} = httpc:request(post, Request, [], []),
  ?assert(AnswerCode == 400),
  ?assert(ReasonPhrase == "Bad Request").

send_message_to_producer_normal_test() ->
  register_producer("send message topic"),
  Body = "{\"topicName\":\"send message topic\", \"data\":\"some data\"}",
  Request = {string:concat(?CUMKA_HOST, "/producer/newMessage"), [], "application/json", Body},
  {ok, {{_, AnswerCode, ReasonPhrase}, _, _}} = httpc:request(post, Request, [], []),
  ?assert(AnswerCode == 200),
  ?assert(ReasonPhrase == "OK").