-module(mod_spark_msgarhive_config).
%% -------------------------------------------------------------------
%% @doc
%% mod_spark_msgarchive: Ejabberd Application
%% An erlang module to do peer-to-peer message archiving streaming service
%% post user authentication with spark api.
%%
%% @end
%% -------------------------------------------------------------------

%% @doc A module to provide access to mod_spark_msgarchive configuration information.
%% @end
-type spark_msgarchive_bucketprops() = [{Propkey :: atom(), Propval :: term()}].
-author('etsang@spark.net').

-export([spark_api_endpoint/0, 
	 spark_oauth_access_token/0, 
	 send_missed_im_endpoint/0,
	 auth_profile_miniProfile_endpoint/0, 
	 rabbitmq_endpoint/0,
 	 rest_client_timeout_in_sec/0,
 	 rest_call_retry_attempt/0,
 	 rabbitmq_client_timeout_in_sec/0,
 	 rabbitmq_client_retry_attempt/0,
       ]).

-include("ejabberd.hrl").
-include("mod_spark_msgarchive_version.hrl").
-include("mod_spark_msgarchive.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-define(APP_ENV,mod_spark_msgarchive).
-define(DEFAULT_RESTCONN_TIMEOUT,5).
-define(DEFAULT_RESTCONN_RETRY,3).
-define(DEFAULT_RESTCONN_RETRY_INTERVAL,1),

-define(DEFAULT_RABBITCONN_TIMEOUT,5).
-define(DEFAULT_RABBITCONN_RETRY,3).
-define(DEFAULT_RABBITCONN_RETRY_INTERVAL,1),

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Get spark api endpoint environment variable.
%% @end
-spec spark_api_endpoint()-> url() | undefined.
spark_api_endpoint() ->
    mod_spark_config_common:spark_api_endpoint().

%% @doc Get the oauth access token environment variable.
-spec spark_oauth_access_token() -> accessToken() | undefined.
spark_oauth_access_token() ->
    mod_spark_config_common:spark_oauth_access_token().

%% @doc Get api send missed Im environment variable.
%% @end
-spec auth_profile_miniProfile()-> string() | undefined.
send_missed_im_endpoint() ->
    mod_spark_config_common:get_mod_spark_msgarchive_env(send_missed_im).

%% @doc Get api miniprofile environment variable.
%% @end
-spec auth_profile_miniProfile()-> string() | undefined.
auth_profile_miniProfile_endpoint() ->
    mod_spark_config_common:auth_profile_miniProfile().

%% @doc Get the rabbitmq endpoint environment variable.
%% @end
-spec rabbitmq_endpoint()
rabbitmq_endpoint() ->
    mod_spark_config_common:rabbitmq_endpoint().

%% @doc Get the rabbitmq endpoint environment variable.
%% @end
-spec rest_client_timeout_in_sec() -> integer().
rest_client_timeout_in_sec() ->
    mod_spark_config_common:rest_client_timeout_in_sec().

%% @doc Get rest client call retry attempt environment variable.
%% @end
-spec rest_call_retry_attempt() -> integer().
rest_call_retry_attempt() ->
    mod_spark_config_common:rest_call_retry_attempt().

%% @doc Get rest client connection retry attempt interval in sec.
%% @end
-spec rest_client_retry_interval()-> integer().
rest_client_retry_interval() ->
    mod_spark_config_common:get_mod_spark_common_env(rest_call_retry_interval,?DEFAULT_RESTCONN_RETRY_INTERVAL).


%% @doc Get the rabbitmq client connection timeout environment variable.
%% @end
-spec rabbitmq_client_timeout_in_sec() -> integer().
rabbitmq_client_timeout_in_sec() ->
    mod_spark_config_common:rabbitmq_client_timeout_in_sec().

%% @doc Get ther abbitmq client connection retry attempt  variable.
%% @end
-spec rabbitmq_client_retry_attempt()-> integer().
rabbitmq_client_retry_attempt() ->
    mod_spark_config_common:rabbitmq_client_retry_attempt().

%% @doc Get the rabbitmq client connection retry attempt  variable.
%% @end
-spec rabbitmq_client_retry_interval()-> integer().
rabbitmq_client_retry_interval() ->
    mod_spark_config_common:get_mod_spark_common_env(rabbitmq_call_retry_interval,?DEFAULT_RABBITCONN_RETRY_INTERVAL).


%% @doc Get the rabbitmq client connection retry attempt  variable.
%% @end
-spec rabbitmq_client_liveQ()-> string() | {error, not_found}.
rabbitmq_client_liveQ() ->
    mod_spark_config_common:get_mod_spark_common_env(rabbitmq_call_liveQ,{error, not_found}).


%% @doc Get the rabbitmq client connection retry attempt  variable.
%% @end
-spec rabbitmq_client_retryQ()-> string() | {error, not_found}.
rabbitmq_client_retryQ() ->
    mod_spark_config_common:get_mod_spark_common_env(rabbitmq_call_retryQ,{error,not_found}).


%% @doc Get the rabbitmq dead queue.
%% @end
-spec rabbitmq_client_deadQ()-> string() | {error, not_found}.
rabbitmq_client_deadQ() ->
    mod_spark_config_common:get_mod_spark_common_env(rabbitmq_call_deadQ, {error, not_found}).


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-define(DEFAULTVAL, "DefaultTestVal").
mod_spark_msgarchive_config_test_() ->
    { setup,
      fun setup/0,
      fun cleanup/1,
      [
       fun spark_api_endpoint_test_case/0,
       fun spark_oauth_access_token_test_case/0,
       fun send_missed_im_endpoint_test_case/0,
       fun auth_profile_miniProfile_endpoint_test_case/0,
       fun rabbitmq_endpoint_test_case/0,
       fun rabbitmq_client_timeout_in_test_case/0,
       fun rabbitmq_client_retry_attempt_in_test_case/0,
       fun rest_client_timeout_in_sec_test_case/0,
       fun rest_call_retry_attempt_test_case/0
      ]
    }.

spark_api_endpoint_test_case()->
    application:set_env(mod_spark_msgarchive, spark_api_endpoint, "http://www.test.endpoint"),
    ?assertMatch("http://www.test.endpoint", spark_api_endpoint()).

spark_oauth_access_token_test_case()->
    application:set_env(mod_spark_msgarchive, spark_auth_endpoint, "oauthTESTTOKEN"),
    ?assertMatch("oauthTESTTOKEN", spark_oauth_access_token()).	

send_missed_im_endpoint_test_case()->
    application:set_env(mod_spark_msgarchive, auth_profile_miniProfile, "miniProfle/{appId}/{brandid}/"),
    ?assertEqual("miniProfle/{appId}/{brandid}/", send_missed_im_endpoint()).

auth_profile_miniProfile_test_case()->
    application:set_env(mod_spark_msgarchive, auth_profile_miniProfile, "miniProfle/{appId}/{brandid}/"),
    ?assertEqual("miniProfle/{appId}/{brandid}/", auth_profile_miniProfile_endpoint()).
	
rabbitmq_endpoint_test_case() ->
    application:set_env(mod_spark_msgarchive, rabbitmq_endpoint, "rabbitMQTEST"),
    ?assertMatch("rabbitMQTEST",rabbitmq_endpoint()).

rest_client_timeout_in_sec_test_case() ->
    application:set_env(mod_spark_msgarchive, rest_client_timeout_in_sec, 12),
    ?assertEqual(12, rest_client_timeout_in_sec()).
	
rest_call_retry_attempt_test_case() ->
    application:set_env(mod_spark_msgarchive, rest_call_retry_attempt, 15),
    ?assertEqual(15, rest_call_retry_attempt()).
	
rabbitmq_client_timeout_in_test_case() ->
    application:set_env(mod_spark_msgarchive, rabbitmq_client_timeout, 10),
    ?assertEqual(10, rabbitmq_client_timeout()).
 
rabbitmq_client_retry_attempt_in_test_case() ->
    application:set_env(mod_spark_msgarchive, rabbitmq_client_retry_attempt, 19),
    ?assertEqual(19, rabbitmq_client_retry_attempt()).
	
setup() ->   
    application:load(?APP_ENV).

cleanup(_Pid) ->
    application:stop(?APP_ENV).
    
-endif.