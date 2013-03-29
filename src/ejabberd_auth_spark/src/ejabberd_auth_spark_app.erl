
%%%----------------------------------------------------------------------
%%%
%%% @author : Edward Tsang <etsang@spark.net>
%%% @doc Retrieve configurtion for spark authentication
%%% Created : 20 Mar 2013
%%%---------------------------------------------------------------------
%%%
%%% Copyright (c)
%%%
%%%----------------------------------------------------------------------
%%% @end

%% @doc Retrieve configurtion for spark authentication
%%      
%% @end
-module(ejabberd_auth_spark_app).
-author('etsang@spark.net').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ejabberd_auth_spark_sup:start_link().

stop(_State) ->
    ok.
