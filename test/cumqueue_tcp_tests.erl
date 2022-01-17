%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Jan 2022 08:33
%%%-------------------------------------------------------------------
-module(cumqueue_tcp_tests).
-author("prikotav").

-include_lib("eunit/include/eunit.hrl").

tcp_flow_test() ->
  {ok, Socket} = gen_tcp:connect(localhost, 5678, [{packet, raw}, {active, false}]),
  gen_tcp:send(Socket, "{\"action\":\"register_producer\",\"data\":{\"url\":\"localhost:9000\", \"topic\":\"tcp cons\", \"group\":\"cons1\"}}"),
  ProdAnswer = binary_to_list(gen_tcp:recv(Socket, 0)),
  gen_tcp:send(Socket,  "{\"action\":\"register_consumer\",\"data\":{\"url\":\"localhost:9000\", \"topic\":\"tcp cons\", \"group\":\"cons1\"}}"),
  ConsAnswer = binary_to_list(gen_tcp:recv(Socket, 0)),
  gen_tcp:send(Socket, "{\"action\":\"new_message\",\"data\":{\"topic\":\"tcp cons\", \"data\":\"a ne spet li mne pesnu\"}}"),
  MsgAnswer = binary_to_list(gen_tcp:recv(Socket, 0)),
  ?assert(ProdAnswer == "{\"status\": \"OK\"}"),
  ?assert(ConsAnswer == "[]"),
  ?assert(MsgAnswer == "{\"status\": \"OK\"}").
