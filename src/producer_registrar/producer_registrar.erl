-module(producer_registrar).

-behaviour(gen_server).

-export([start/0, stop/0, register_producer/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(producer_registrar_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

register_producer(ProducerName) ->
  %TODO Timeout can be get here
  gen_server:call(producer_registrar, {register_producer, ProducerName}).

start() ->
  lager:log(info, self(), "Started producer_reg!~n"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

init([]) ->
  {ok, #producer_registrar_state{}}.

handle_call({register_producer, ProducerName}, _From, State = #producer_registrar_state{}) ->
  lager:log(debug, self(), "Registring producer with name: ~p...~n", [ProducerName]),
  %TODO Spawn topic and return its meta info to caller
  lager:log(debug, self(), "Successfully registred producer with name: ~p...~n", [ProducerName]),
  {reply, ok, State};

handle_call(stop, _From, Tab) ->
  {stop, normal, stopped, Tab}.

handle_cast(_Request, State = #producer_registrar_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #producer_registrar_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #producer_registrar_state{}) ->
  ok.

code_change(_OldVsn, State = #producer_registrar_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
