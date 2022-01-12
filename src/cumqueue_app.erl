-module(cumqueue_app).

-behaviour(application).

-export([stop/1, start/2]).

start(_, _) ->
    producer_registrar_sup:start_link(),
    producer_sup:start_link(),
    consumer_registrar_sup:start_link(),
    consumer_sup:start_link(),
    topic_sup:start_link().

stop(_State) ->
    ok.