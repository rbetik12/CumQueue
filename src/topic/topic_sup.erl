%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(topic_sup).

-behaviour(supervisor).

-export([start/0, init/1]).

start() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = #{
    strategy => one_for_one,
    intensity => 10,
    period => 1000
  },
  ChildSpecs = [
    #{id => topic_manager,
      start => {topic_manager, start, []},
      restart => transient,
      shutdown => 2000,
      type => worker,
      modules => [topic_manager]}
  ],
  {ok, {SupFlags, ChildSpecs}}.
