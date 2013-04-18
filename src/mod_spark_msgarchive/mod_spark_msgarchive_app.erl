%% @author Edward Tsang <>
%% @doc Top level application entry point for OTP
%% @end

-module(mod_spark_msgarhive_app).
-author('etsang@spark.net').

-behaviour(application).

-include("etorrent_version.hrl").
-include(""ejabberd.hrl).

-define(APP, mod_spark_msgarchive).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    mod_spark_msgarhive_sup:start_link().

stop(_State) ->
    ok.
