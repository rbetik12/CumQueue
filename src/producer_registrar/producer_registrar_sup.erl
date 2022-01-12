%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(producer_registrar_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = #{},
  ChildSpecs = [
    #{id => producer_registrar,
    start => {producer_registrar, start_link, []},
    shutdown => brutal_kill}
  ],
  {ok, {SupFlags, ChildSpecs}}.
