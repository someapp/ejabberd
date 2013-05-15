%% @author Edward Tsang <>
%% @doc Top level application entry point for OTP
%% @end

-module(mod_spark_http_offline_app).
-author('etsang@spark.net').

-behaviour(application).

-include("ejabberd.hrl").

-define(APP, mod_spark_msgarchive).

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    mod_spark_http_offline_sup:start_link().

stop(_State) ->
    ok.



%%%%%% EUNIT %%%%%%%%%%%%%%%%%%
-ifdef(TEST).


-endif.


