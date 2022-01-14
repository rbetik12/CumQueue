-module(cumqueue_app).

-behaviour(application).

-export([stop/1, start/2]).

start(_, _) ->
    ok = application:start(cowlib),
    ok = application:start(ranch),
    ok = application:start(cowboy),

    producer_registrar_sup:start_link(),
    producer_sup:start_link(),
    consumer_registrar_sup:start_link(),
    consumer_sup:start_link(),
    topic_sup:start_link(),

    start_http_server(8080).

start_http_server(Port) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/consumerRegistration", consumer_register_controller, []},
            {"/consumer/[:someDataIfYouNeed]", consumer_controller, []},
            {"/producerRegistration", producer_register_controller, []},
            {"/producer/[:someDataIfYouNeed]", producer_controller, []},
            {"/topic", topic_controller, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    cumqueue_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).