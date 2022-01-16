%%%-------------------------------------------------------------------
%%% @author prikotav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jan 2022 21:12
%%%-------------------------------------------------------------------
-module(net_utils).
-author("prikotav").

%% API
-export([skip_http_header/1, setup_socket/4, teardown_socket/1]).

skip_http_header(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, http_eoh} -> ok;
    _ -> skip_http_header(Socket)
  end.

setup_socket(Port, IsActive, PacketType, ReuseAddr) ->
  {ok, _} = gen_tcp:listen(Port, [list, {active, IsActive}, {packet, PacketType}, {reuseaddr, ReuseAddr}]).

teardown_socket(Socket) ->
  gen_tcp:shutdown(Socket, read_write).
