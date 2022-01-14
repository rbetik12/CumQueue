%%%-------------------------------------------------------------------
%%% @author salvoroni
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2022 2:53 AM
%%%-------------------------------------------------------------------
-module(consumer_register_controller).
-author("salvoroni").

%% API
-export([init/2]).

init(Req0, Opts) ->
  Req = cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/plain">>
  }, <<"Hello in registration!">>, Req0),
  {ok, Req, Opts}.
