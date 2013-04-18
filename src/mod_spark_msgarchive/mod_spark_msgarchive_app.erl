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

%% type 
-type configKV() :: { atom(), term()}.


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    loadConfig(),
    checkConfig(),
    mod_spark_msgarhive_sup:start_link().

stop(_State) ->
    ok.



%% ===================================================================
%% Private  
%% ===================================================================

%% @doc
%% A list of configuration specification that lists out which keys are 
%% require, and which are optional
%% @end
-spec configuration_spec()-> [configKV()].
configuration_spec() ->
   [ required(spark_auth_endpoint),
     required(send_missed_im),
     required(profile_memberstatus),
     required(rabbitmq_endpoint),
     optional(spark_oauth_access_token),
     optional(rest_client_timeout_in_sec),
     optional(rest_call_retry_attempt)
   ].


%% @doc
%% Returns the final configuration list for application execution  
%% @end
-spec load_config([]
load_config([])->
  ok,

load_config([|])->
  load_config
  .


reload() ->

 .


check_config() ->
 

 .

%% @doc If the config key has no value, put in a default one
%% @end
optional(Key, Default)->

%% @doc If the config key value is missing, it is error
%% @end
required(Key)->

.



%%%%%% EUNIT %%%%%%%%%%%%%%%%%%
-ifdef(TEST).




optionalKey_missing_value_test()->
				.

requiredKey_missing_value_test()->
				.

-endif.


