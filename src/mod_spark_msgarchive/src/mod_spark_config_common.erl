%% -------------------------------------------------------------------
%% @doc
%% mod_spark_config_common 
%% An erlang module host common environment settings
%%
%% @end
%% -------------------------------------------------------------------

%% @doc A module to provide access to mod_spark configuration information.
-type spark_stun_bucketprops() = [{Propkey :: atom(), Propval :: term()}]

-module(mod_spark_config_common).
-author('etsang@spark.net').

-export([spark_api_endpoint/0, 
	 spark_oauth_access_token/0, 
	 auth_profile_miniProfile/0, 
	 rabbitmq_endpoint/0,
 	 rest_client_timeout_in_sec/0,
 	 rest_call_retry_attempt/0,
 	 rabbitmq_client_timeout_in_sec/0,
 	 rabbitmq_client_retry_attempt/0,
	 get_mod_spark_common_env/1,
	 get_mod_spark_common_env/2
       ]).

-include("ejabberd.hrl").
-include("mod_spark_stun_version.hrl").
-include("mod_spark_stun.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-define(APP_ENV,mod_spark_core).

-define(DEFAULT_RESTCONN_TIMEOUT,5).
-define(DEFAULT_RESTCONN_RETRY,3).
-define(DEFAULT_RABBITCONN_TIMEOUT,5).
-define(DEFAULT_RABBITCONN_RETRY,3).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Get spark api endpoint environment variable.
%% @end
-spec spark_api_endpoint()-> url() | undefined.
spark_api_endpoint() ->
    get_mod_spark_common_env(spark_api_endpoint).

%% @doc Get the oauth access token environment variable.
-spec spark_oauth_access_token() -> accessToken() | undefined.
spark_oauth_access_token() ->
    get_mod_spark_common_env(spark_oauth_access_token).  

%% @doc Get the server list environment variable.
%% @end
-spec stun_server_list() -> list() | undefined.
stun_server_list() ->
    get_mod_spark_common_env(stun_server_list).

%% @doc Get the backup server list environment variable.
%% @end
-spec stun_server_backup_list() -> list() | undefined.
stun_server_backup_list() ->
    get_mod_spark_common_env(stun_server_backup_list,[]).

%% @doc Get api miniprofile environment variable.
%% @end
-spec auth_profile_miniProfile()-> string() | undefined.
auth_profile_miniProfile() ->
    get_mod_spark_common_env(auth_profile_miniProfile).

%% @doc Get the rabbitmq endpoint environment variable.
%% @end
-spec rabbitmq_endpoint()
rabbitmq_endpoint() ->
    get_mod_spark_common_env(rabbitmq_endpoint).

%% @doc Get the rabbitmq endpoint environment variable.
%% @end
-spec rest_client_timeout_in_sec() -> integer().
rest_client_timeout_in_sec() ->
      get_mod_spark_common_env(rest_client_timeout_in_sec, ?DEFAULT_RESTCONN_TIMEOUT).    

%% @doc Get rest client call retry attempt environment variable.
%% @end
-spec rest_call_retry_attempt() -> integer().
rest_call_retry_attempt() ->
       get_mod_spark_common_env(rest_call_retry_attempt, ?DEFAULT_RESTCONN_RETRY).

%% @doc Get the rabbitmq client connection timeout environment variable.
%% @end
-spec rabbitmq_client_timeout_in_sec() -> integer().
rabbitmq_client_timeout_in_sec() ->
       get_mod_spark_common_env(rabbitmq_client_timeout_in_sec, ?DEFAULT_RABBITCONN_TIMEOUT).

%% @doc Get ther abbitmq client connection retry attempt  variable.
%% @end
-spec rabbitmq_client_retry_attempt()-> integer().
rabbitmq_client_retry_attempt() ->
       get_mod_spark_common_env(rabbitmq_call_retry_attempt, ?DEFAULT_RABBITCONN_RETRY).


%% @private
get_mod_spark_common_env(Key) ->
   case app_helper:get_env(?APP_ENV, Key) of
	undefined -> ?ERROR_MSG("Required environment variable missing module version ~p ~p ~p~n",[?VERSION, ?CURRENT_FUNCTION_NAME(), {Key, undefined}]),
		      undefined;
	Value -> Value
   end;
get_mod_spark_common_env(Key, DEFAULT) ->
   app_helper:get_env(?APP_ENV, Key, DEFAULT).


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-define(DEFAULTVAL, "DefaultTestVal").
mod_spark_common_config_test_() ->
    { setup,
      fun setup/0,
      fun cleanup/1,
      [
       fun spark_api_endpoint_test_case/0,
       fun spark_oauth_access_token_test_case/0,
       fun auth_profile_miniProfile_test_cas/0,
       fun rabbitmq_endpoint_test_case/0,
       fun rabbitmq_client_timeout_in_test_case/0,
       fun rabbitmq_client_retry_attempt_in_test_case/0,
       fun rest_client_timeout_in_sec_test_case/0,
       fun rest_call_retry_attempt_test_case/0,
       fun non_existent_var_test_case/0,
       fun get_mod_spark_common_defaultValue_test_case/0,	
       fun get_mod_spark_common_undefinedVal_test_case/0
      ]
    }.

spark_api_endpoint_test_case()->
    application:set_env(mod_spark_, spark_api_endpoint, "http://www.test.endpoint"),
    ?assertMatch("http://www.test.endpoint", spark_api_endpoint()).

spark_oauth_access_token_test_case()->
    application:set_env(mod_spark_test, spark_auth_endpoint, "oauthTESTTOKEN"),
    ?assertMatch("oauthTESTTOKEN", spark_oauth_access_token()).	

auth_profile_miniProfile_test_case()->
    application:set_env(mod_spark_test, auth_profile_miniProfile, "miniProfle/{appId}/{brandid}/"),
    ?assertEqual("miniProfle/{appId}/{brandid}/", auth_profile_miniProfile,()).
	
rabbitmq_endpoint_test_case() ->
    application:set_env(mod_spark_test, rabbitmq_endpoint, "rabbitMQTEST"),
    ?assertMatch("rabbitMQTEST",rabbitmq_endpoint()).

rest_client_timeout_in_sec_test_case() ->
    application:set_env(mod_spark_test, rest_client_timeout_in_sec, 12),
    ?assertEqual(12, rest_client_timeout_in_sec()).
	
rest_call_retry_attempt_test_case() ->
    application:set_env(mod_spark_test, rest_call_retry_attempt, 15),
    ?assertEqual(15, rest_call_retry_attempt()).
	
rabbitmq_client_timeout_in_test_case() ->
    application:set_env(mod_spark_test, rabbitmq_client_timeout, 10),
    ?assertEqual(10, rabbitmq_client_timeout()).
 
rabbitmq_client_retry_attempt_in_test_case() ->
    application:set_env(mod_spark_test, rabbitmq_client_retry_attempt, 19),
    ?assertEqual(19, rabbitmq_client_retry_attempt()).
	
non_existent_var_test_case() ->
    ?assertEqual(undefined, get_mod_spark_common_env(bogus)).


get_mod_spark_env_defaultValue_test_case()->
    application:set_env(test_var_with_value, ?DEFAULTVAL),
    ?assertMatch(?DEFAULTVAL,get_mod_spark_common_env(test_var_with_value, ?DEFAULTVAL) ).

get_mod_spark_env_defaultValue_test_case()->
    application:set_env(test_var_no_value, ?DEFAULTVAL),
    ?assertMatch(?DEFAULTVAL, get_mod_spark_common_env(test_var_no_value, ?DEFAULTVAL)).


get_mod_spark_env_undefinedVal_test_case() ->
    application:set_env(config_var_missing),
    ?assertMatch(undefined, get_mod_spark_common_env(config_var_missing)).

setup() ->   
    application:load(?APP_ENV).

cleanup(_Pid) ->
    application:stop(?APP_ENV).
    
-endif.
