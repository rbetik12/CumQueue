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

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%supervisor
init([]) ->
  Procs = [],
  {ok, {{one_for_one, 10, 10}, Procs}}.
