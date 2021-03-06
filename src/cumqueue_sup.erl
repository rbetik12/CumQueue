%%%-------------------------------------------------------------------
%%% @author salvoroni
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2022 2:36 AM
%%%-------------------------------------------------------------------
-module(cumqueue_sup).
-behavior(supervisor).

-export([start/0]).
-export([init/1]).

-spec start() -> {ok, pid()}.
start() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% TODO this sup must run other sups
init([]) ->
  Procs = [],
  {ok, {{one_for_one, 10, 10}, Procs}}.
