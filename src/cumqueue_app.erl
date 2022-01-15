-module(cumqueue_app).

-behaviour(application).

-export([stop/1, start/2]).

start(_, _) ->
    ok = application:start(cowlib),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    lager:start(),
    lager:set_loglevel(lager_console_backend, debug),

    producer_registrar_sup:start(),
    producer_sup:start(),
    consumer_registrar_sup:start(),
    consumer_sup:start(),
    topic_sup:start(),
    topic_manager_sup:start(),

    start_http_server(8080).

start_http_server(Port) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/consumerRegistration", consumer_register_controller, []},
            {"/producerRegistration", producer_register_controller, []},
            {"/producer/[...]", producer_controller, []},
            {"/topic", topic_controller, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    cumqueue_sup:start().

stop(_State) ->
    ok = cowboy:stop_listener(http).