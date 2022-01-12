%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(topic_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = #{},
  ChildSpecs = [
    #{id => topic,
      start => {topic, start_link, []},
      shutdown => brutal_kill}
  ],
  {ok, {SupFlags, ChildSpecs}}.
