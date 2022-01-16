%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(producer_registrar_sup).

-behaviour(supervisor).

-export([start/1, init/1]).

start(Mode) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Mode]).

init([Mode]) ->
  SupFlags = #{},
  ChildSpecs = [
    #{id => producer_registrar,
    start => {producer_registrar, start, [Mode]},
    shutdown => brutal_kill}
  ],
  {ok, {SupFlags, ChildSpecs}}.
