%%%----------------------------------------------------------------------
%%%
%%% @author : Edward Tsang <etsang@spark.net>
%%% @doc http post offline module to ejabberd. modified from mod_http_offline
%%% Created : 13 May 2013
%%%---------------------------------------------------------------------
%%%
%%% Copyright (c)
%%%
%%%----------------------------------------------------------------------
%%% @end

-module(mod_spark_http_offline_sup).
-author("etsang").
-behaviour(supervisor).

-export([start_link/0, init/1]).
start_link() ->
	mod_spark_http_offline:start_link([], []).

init([Args]) ->
	ok.

