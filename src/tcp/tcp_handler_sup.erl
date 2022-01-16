%%%-------------------------------------------------------------------
%%% @author salvoroni
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jan 2022 4:33 PM
%%%-------------------------------------------------------------------
-module(tcp_handler_sup).
-behavior(supervisor).

%% API
-export([start/0]).

%% supervisor
-export([init/1]).

-spec start() -> {ok, pid()}.
start() ->
  lager:log(info, self(), "starting tcp listener..."),
  Return = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  io:format("RETURN ~w~n", [Return]),
  lager:log(info, self(), "started tcp listener with pid ~p", [1]).

%%supervisor
init([]) ->
  SupFlags = #{
    strategy => one_for_one,
    intensity => 10,
    period => 1000
  },
  ChildSpecs = [
    #{id => tcp_handler,
      start => {tcp_handler, start, []},
      restart => transient,
      shutdown => 2000,
      type => worker,
      modules => [tcp_handler]}
  ],
  {ok, {SupFlags, ChildSpecs}}.