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
    strategy => simple_one_for_one,
    intensity => 10,
    period => 1000
  },
  ChildSpecs = [
    #{id => topic,
      start => {topic, start, []},
      restart => transient,
      shutdown => 2000,
      type => worker,
      modules => [topic]}
  ],
  {ok, {SupFlags, ChildSpecs}}.
