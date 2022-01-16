%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jan 2022 15:25
%%%-------------------------------------------------------------------
-module(cumqueue_full_flow).
-author("prikotav").

-include_lib("eunit/include/eunit.hrl").
-include("include/test_env.hrl").

%%setup_test_env() ->
%%  application:start(cumqueue_app).
%%
%%create_producer_and_send_message_test() ->
%%  setup_test_env(),
%%  Body = "{\"topicName\":\"test topic 1\"}",
%%  Request = {string:concat(?CUMKA_HOST, "/producerRegistration"), [], "application/json", Body},
%%  {ok, {{_, AnswerCode, ReasonPhrase}, _, _}} = httpc:request(post, Request, [], []),
%%  MessageBody = "{\"topicName\":\"test topic 1\", \"data\":\"kek\"}",
%%  MessageRequest = {string:concat(?CUMKA_HOST, "/producer/newMessage"), [], "application/json", MessageBody},
%%  {ok, {{_, MessageAnswerCode, MessageReasonPhrase}, _, _}} = httpc:request(post, MessageRequest, [], []),
%%  io:format("~p~n", [[MessageAnswerCode, MessageReasonPhrase]]),
%%  ?assert(false).