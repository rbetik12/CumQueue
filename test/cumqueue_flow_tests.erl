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

register_one_producer_test() ->
  setup_test_env(),
  Body = "{\"topicName\":\"test topic 1\"}",
  Request = {string:concat(?CUMKA_HOST, "/producerRegistration"), [], "application/json", Body},
  {ok, {{_, AnswerCode, ReasonPhrase}, _, _}} = httpc:request(post, Request, [], []),
  ?assert(AnswerCode == 200),
  ?assert(ReasonPhrase == "OK").

register_two_producers_test() ->
  Body = "{\"topicName\":\"test topic 2\"}",
  Body1 = "{\"topicName\":\"test topic 3\"}",
  Request = {string:concat(?CUMKA_HOST, "/producerRegistration"), [], "application/json", Body},
  Request1 = {string:concat(?CUMKA_HOST, "/producerRegistration"), [], "application/json", Body1},
  {ok, {{_, AnswerCode, ReasonPhrase}, _, _}} = httpc:request(post, Request, [], []),
  {ok, {{_, AnswerCode1, ReasonPhrase1}, _, _}} = httpc:request(post, Request1, [], []),
  ?assert(AnswerCode == 200),
  ?assert(ReasonPhrase == "OK"),
  ?assert(AnswerCode1 == 200),
  ?assert(ReasonPhrase1 == "OK").

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

send_message_full_flow_test() ->
  ExpectedTopic = "full flow test",
  ExpectedData = "some data",
  Body = io_lib:format("{
      \"topicName\":\"~s\",
      \"data\":\"~s\"
    }", [ExpectedTopic, ExpectedData]),
  Request = {string:concat(?CUMKA_HOST, "/producer/newMessage"), [], "application/json", list_to_binary(Body)},

  register_producer(ExpectedTopic),
  register_consumer(ExpectedTopic, "localhost", 9000, 9000),

  {ok, ListenSocket} = net_utils:setup_socket(9000, false, http, true),
  {ok, {{_, 200, "OK"}, _, _}} = httpc:request(post, Request, [], []),
  {Topic, Payload} = accept_message(ListenSocket),
  net_utils:teardown_socket(ListenSocket),

  ?assert(Topic == ExpectedTopic),
  ?assert(Payload == ExpectedData).

multiple_consumers_test() ->
  ExpectedTopic = "multiple consumers",
  ExpectedData = "some data",
  ConsumerPort1 = 9001,
  ConsumerPort2 = 9002,
  Body = io_lib:format("{
      \"topicName\":\"~s\",
      \"data\":\"~s\"
    }", [ExpectedTopic, ExpectedData]),
  Request = {string:concat(?CUMKA_HOST, "/producer/newMessage"), [], "application/json", list_to_binary(Body)},

  register_producer(ExpectedTopic),
  register_consumer(ExpectedTopic, "localhost", ConsumerPort1, ConsumerPort1),
  register_consumer(ExpectedTopic, "localhost", ConsumerPort2, ConsumerPort2),

  {ok, ListenSocket1} = net_utils:setup_socket(ConsumerPort1, false, http, true),
  {ok, ListenSocket2} = net_utils:setup_socket(ConsumerPort2, false, http, true),
  {ok, {{_, 200, "OK"}, _, _}} = httpc:request(post, Request, [], []),

  {Topic1, Data1} = accept_message(ListenSocket1),
  net_utils:teardown_socket(ListenSocket1),
  {Topic2, Data2} = accept_message(ListenSocket2),
  net_utils:teardown_socket(ListenSocket2),

  ?assert(Topic1 == ExpectedTopic),
  ?assert(Topic2 == ExpectedTopic),
  ?assert(Data1 == ExpectedData),
  ?assert(Data2 == ExpectedData).

consumer_subscribe_twice_test() ->
  ExpectedTopic = "subscribe twice",
  ExpectedData = "some data",
  ConsumerPort = 9004,
  Body = io_lib:format("{
      \"topicName\":\"~s\",
      \"data\":\"~s\"
    }", [ExpectedTopic, ExpectedData]),
  Request = {string:concat(?CUMKA_HOST, "/producer/newMessage"), [], "application/json", list_to_binary(Body)},
  {ok, ListenSocket} = net_utils:setup_socket(ConsumerPort, false, http, true),

  register_producer(ExpectedTopic),
  register_consumer(ExpectedTopic, "localhost", ConsumerPort, ConsumerPort),
  {ok, {{_, 200, "OK"}, _, _}} = httpc:request(post, Request, [], []),

  {Topic, Data} = accept_message(ListenSocket),

  {ok, {{_, 200, _}, _, ResponseBody}} = register_consumer(ExpectedTopic, "localhost", ConsumerPort, ConsumerPort),
  Json = jsone:decode(list_to_binary(ResponseBody)),
  Status = binary_to_list(maps:get(<<"status">>, Json)),
  ?assert(Topic == ExpectedTopic),
  ?assert(Data == ExpectedData),
  ?assert(Status == "already exists").

send_multiple_messages_than_subscribe_test() ->
  ExpectedTopic = "send_multiple_messages_than_subscribe",
  ExpectedData = "some data",
  ConsumerPort = 9005,
  Body = io_lib:format("{
      \"topicName\":\"~s\",
      \"data\":\"~s\"
    }", [ExpectedTopic, ExpectedData]),
  Request = {string:concat(?CUMKA_HOST, "/producer/newMessage"), [], "application/json", list_to_binary(Body)},

  register_producer(ExpectedTopic),
  {ok, {{_, 200, "OK"}, _, _}} = httpc:request(post, Request, [], []),
  {ok, {{_, 200, "OK"}, _, _}} = httpc:request(post, Request, [], []),
  {ok, {{_, 200, "OK"}, _, _}} = httpc:request(post, Request, [], []),
  {ok, {{_, 200, _}, _, Messages}} = register_consumer(ExpectedTopic, "localhost", ConsumerPort, ConsumerPort),
  [MessageJson1, MessageJson2, MessageJson3] = jsone:decode(list_to_binary(Messages)),

  {Topic1, Data1} = {maps:get(<<"topic">>, MessageJson1), binary_to_list(maps:get(<<"message_payload">>, MessageJson1))},
  {Topic2, Data2} = {maps:get(<<"topic">>, MessageJson2), binary_to_list(maps:get(<<"message_payload">>, MessageJson2))},
  {Topic3, Data3} = {maps:get(<<"topic">>, MessageJson3), binary_to_list(maps:get(<<"message_payload">>, MessageJson3))},

  ?assert(Topic1 == ExpectedTopic),
  ?assert(Topic2 == ExpectedTopic),
  ?assert(Topic3 == ExpectedTopic),
  ?assert(Data1 == ExpectedData),
  ?assert(Data2 == ExpectedData),
  ?assert(Data3 == ExpectedData).

%TODO fix
one_consumer_subscribes_on_two_topics_test_ignore() ->
  ExpectedTopic1 = "cool topic 1",
  ExpectedTopic2 = "cool topic 2",
  ExpectedData = "some data",
  ConsumerPort = 9006,
  MessageBody1 = io_lib:format("{
      \"topicName\":\"~s\",
      \"data\":\"~s\"
    }", [ExpectedTopic1, ExpectedData]),
  MessageBody2 = io_lib:format("{
      \"topicName\":\"~s\",
      \"data\":\"~s\"
    }", [ExpectedTopic2, ExpectedData]),
  Request1 = {string:concat(?CUMKA_HOST, "/producer/newMessage"), [], "application/json", list_to_binary(MessageBody1)},
  Request2 = {string:concat(?CUMKA_HOST, "/producer/newMessage"), [], "application/json", list_to_binary(MessageBody2)},
  {ok, ListenSocket} = net_utils:setup_socket(ConsumerPort, false, http, true),

  register_producer(ExpectedTopic1),
  register_producer(ExpectedTopic2),
  {ok, {{_, 200, "OK"}, _, _}} = httpc:request(post, Request1, [], []),
  {ok, {{_, 200, "OK"}, _, _}} = httpc:request(post, Request2, [], []),
  register_consumer(ExpectedTopic1, "localhost", 9006, 9006),
  register_consumer(ExpectedTopic2, "localhost", 9006, 9006),

  {Topic1, Data1} = accept_message(ListenSocket),
  {Topic2, Data2} = accept_message(ListenSocket),
  lager:log(debug, "~p~n", [[Topic1, Data1, Topic2, Data2]]),
  ?assert(false).

% Test utility functions

get_consumer_message(Socket) ->
  net_utils:skip_http_header(Socket),
  inet:setopts(Socket, [{packet, raw}]),
  {ok, MessageBody} = gen_tcp:recv(Socket, 0),
  MessageJson = jsone:decode(list_to_binary(MessageBody)),
  {maps:get(<<"topic">>, MessageJson), binary_to_list(maps:get(<<"message_payload">>, MessageJson))}.

accept_message(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  MessageData = get_consumer_message(Socket),
  net_utils:teardown_socket(Socket),
  MessageData.

setup_test_env() ->
  application:start(cumqueue).

teardown_test_env() ->
  application:stop(cumqueue).

register_producer(TopicName) ->
  Body = lists:concat(["{\"topicName\":\"", TopicName, "\"}"]),
  Request = {string:concat(?CUMKA_HOST, "/producerRegistration"), [], "application/json", Body},
  {ok, {{_, 200, _}, _, _}} = httpc:request(post, Request, [], []).

register_consumer(TopicName, Host, Port, GroupId) ->
  Body = io_lib:format("{
      \"topic\":\"~s\",
      \"url\":\"http://~s:~B\",
      \"group\": ~p
    }", [TopicName, Host, Port, GroupId]),
  Request = {string:concat(?CUMKA_HOST, "/consumerRegistration"), [], "application/json", list_to_binary(Body)},
  {ok, {{_, 200, _}, _, _}} = httpc:request(post, Request, [], []).


