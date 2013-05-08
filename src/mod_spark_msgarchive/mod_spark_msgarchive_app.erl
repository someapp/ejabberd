%% @author Edward Tsang <>
%% @doc Top level application entry point for OTP
%% @end

-module(mod_spark_msgarhive_app).
-author('etsang@spark.net').

-behaviour(application).

-include("mod_spark_msgarchive.hrl").
-include(""ejabberd.hrl).

-define(APP, mod_spark_msgarchive).

%% Application callbacks
-export([start/2, stop/1]).

%% type 
-type configKV() :: { atom(), term()}.
-record(serviceapi_config, 
	{ 
	  baseServiceApi,
	  send_missed_im_endpoint,
	  auth_profile_miniProfile_endpoint,          
	}).

-record(rabbitmqc_config, 
	{
	  rabbitMQ_endpoint,
	  rabbiqMQ_liveQ,
	  rabbitMQ_retryQ,
	  rabbitMQ_deadQ,
 	  rabbitc_retry_attempt,
	  rabbitc_timeout,
	  rabbitc_retry_interval	  
	}).
-record(restc_config, 
	{
	  restc_retry_interval,
	  restc_timeout,
	  restc_retry_attempt
	}).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
     ApiConf = get_service_api_config(),
     RestCConf = get_restClient_config(),
     RabbitmqCConf = get_rabbitMQclient_config(),


    mod_spark_msgarhive_sup:start_link().

stop(_State) ->
    ok.



%% ===================================================================
%% Private  
%% ===================================================================

%% @doc Get Rest Api config
%% @end
get_service_api_config()->
   #serviceapi_config{
		baseServiceApi=mod_spark_msgarchive_config:spark_api_endpoint(), 
		send_missed_im_endpoint=mod_spark_msgarchive_config:send_missed_im_endpoint(),
	        auth_profile_miniProfile_endpoint=mod_spark_msgarchive_config:auth_profile_miniProfile_endpoint()
	   }.

%% @doc Get RestClient config
%% @end
get_restClient_config()->
   #restc_config{
	  	restc_retry_interval=mod_spark_msgarchive_config:rest_client_retry_interval(),
	  	restc_timeout=mod_spark_msgarchive_config:rest_client_timeout_in_sec(),
	  	restc_retry_attempt=mod_spark_msgarchive_config:rest_call_retry_attempt()	
  	    }.

%% @doc Get RabbitMQClient config
%% @end
get_rabbitMQclient_config()->
   #rabbitmqc_config{
	  	rabbitMQ_endpoint=mod_spark_msgarchive_config:rabbitmq_endpoint(), 
	  	rabbiqMQ_liveQ=mod_spark_msgarchive_config:rabbitmq_client_liveQ(),
	  	rabbitMQ_retryQ=mod_spark_msgarchive_config:rabbitmq_client_retryQ(),
	  	rabbitMQ_deadQ=mod_spark_msgarchive_config:rabbitmq_client_deadQ(),
 	  	rabbitc_retry_attempt=mod_spark_msgarchive_config:rabbitmq_client_retry_attempt(),
	  	rabbitc_timeout=mod_spark_msgarchive_config:rabbitmq_client_timeout_in_sec(),
	  	rabbitc_retry_interval=mod_spark_msgarchive_config:rabbitmq_client_retry_interval()
  	     }.



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


