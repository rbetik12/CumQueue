-module(cumqueue_app).

-behaviour(application).

-export([stop/1, start/2]).

% Exported only for testing
-export([start_http_server/1, start_and_setup_deps/0]).

start(_, _) ->
    start_and_setup_deps(),
    start_modules(),

    start_http_server(8080).

stop(_State) ->
    ok = cowboy:stop_listener(http).

start_and_setup_deps() ->
    ok = application:start(cowlib),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    lager:start(),
    lager:set_loglevel(lager_console_backend, debug).

start_modules() ->
    producer_registrar_sup:start(production),
    producer_sup:start(),
    consumer_registrar_sup:start(),
    consumer_sup:start(),
    topic_sup:start(),
    topic_manager_sup:start().

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