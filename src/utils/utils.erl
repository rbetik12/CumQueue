%%%-------------------------------------------------------------------
%%% @author salvoroni
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jan 2022 1:52 AM
%%%-------------------------------------------------------------------
-module(utils).
-author("salvoroni").

-include("../include/message.hrl").

%% API
-export([message_to_json/1, messages_to_json/1]).

message_to_json(Message) ->
  Res = #{
    id => Message#message.id,
    topic => Message#message.topic,
    message_payload => Message#message.message_payload
  },
  Res.

messages_to_json(Messages) ->
  messages_to_json(Messages, []).

messages_to_json([], Result) ->
  Result;
messages_to_json([Head|Tail], Result) ->
  messages_to_json(Tail, [message_to_json(Head)|Result]).