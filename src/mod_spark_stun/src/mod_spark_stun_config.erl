%% -------------------------------------------------------------------
%% @doc
%% mod_spark_stun: Ejabberd Application
%% An erlang module to do peer-to-peer video/voice connection through stun server
%% post user authentication with spark api.
%%
%% @end
%% -------------------------------------------------------------------

%% @doc A module to provide access to mod_spark_stun configuration information.
-type spark_stun_bucketprops() = [{Propkey :: atom(), Propval :: term()}]

-module(mod_spark_stun_config).
-author('etsang@spark.net').

-export([spark_api_endpoint/0, 
	 spark_oauth_access_token/0, 
	 stun_server_list/0,
	 stun_server_backup_list/0,
	 auth_profile_miniProfile/0, 
	 rabbitmq_endpoint/0,
 	 rest_client_timeout_in_sec/0,
 	 rest_call_retry_attempt/0,
 	 rabbitmq_client_timeout_in_sec/0,
 	 rabbitmq_client_retry_attempt/0,
       ]).

-include("ejabberd.hrl").
-include("mod_spark_stun_version.hrl").
-include("mod_spark_stun.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-define(APP_ENV,mod_spark_stun).
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
    get_mod_spark_stun_env(spark_api_endpoint).

%% @doc Get the oauth access token environment variable.
-spec spark_oauth_access_token() -> accessToken() | undefined.
spark_oauth_access_token() ->
    get_mod_spark_stun_env(spark_oauth_access_token).  

%% @doc Get the stun server list environment variable.
%% @end
-spec stun_server_list() -> list() | undefined.
stun_server_list() ->
    get_mod_spark_stun_env(stun_server_list).

%% @doc Get the backup stun server list environment variable.
%% @end
-spec stun_server_backup_list() -> list() | undefined.
stun_server_backup_list() ->
    get_mod_spark_stun_env(stun_server_backup_list,[]).

%% @doc Get api miniprofile environment variable.
%% @end
-spec auth_profile_miniProfile()-> string() | undefined.
auth_profile_miniProfile() ->
    get_mod_spark_stun_env(auth_profile_miniProfile).

%% @doc Get the rabbitmq endpoint environment variable.
%% @end
-spec rabbitmq_endpoint()
rabbitmq_endpoint() ->
    get_mod_spark_stun(rabbitmq_endpoint).

%% @doc Get the rabbitmq endpoint environment variable.
%% @end
-spec rest_client_timeout_in_sec() -> integer().
rest_client_timeout_in_sec() ->
      get_mod_spark_stun_env(rest_client_timeout_in_sec, ?DEFAULT_RESTCONN_TIMEOUT).    

%% @doc Get rest client call retry attempt environment variable.
%% @end
-spec rest_call_retry_attempt() -> integer().
rest_call_retry_attempt() ->
       get_mod_spark_stun_env(rest_call_retry_attempt, ?DEFAULT_RESTCONN_RETRY).

%% @doc Get the rabbitmq client connection timeout environment variable.
%% @end
-spec rabbitmq_client_timeout_in_sec() -> integer().
rabbitmq_client_timeout_in_sec() ->
       get_mod_spark_stun_env(rabbitmq_client_timeout_in_sec, ?DEFAULT_RABBITCONN_TIMEOUT).

%% @doc Get ther abbitmq client connection retry attempt  variable.
%% @end
-spec rabbitmq_client_retry_attempt()-> integer().
rabbitmq_client_retry_attempt() ->
       get_mod_spark_stun_env(rabbitmq_call_retry_attempt, ?DEFAULT_RABBITCONN_RETRY).


%% @private
get_mod_spark_stun_env(Key) ->
   case app_helper:get_env(?APP_ENV, Key) of
	undefined -> ?ERROR_MSG("Required environment variable missing module version ~p ~p ~p~n",[?VERSION, ?CURRENT_FUNCTION_NAME(), {Key, undefined}]),
		      undefined;
	Value -> Value
   end;
get_mod_spark_stun_env(Key, DEFAULT) ->
   app_helper:get_env(?APP_ENV, Key, DEFAULT).


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-define(DEFAULTVAL, "DefaultTestVal").
mod_spark_stun_config_test_() ->
    { setup,
      fun setup/0,
      fun cleanup/1,
      [
       fun spark_api_endpoint_test_case/0,
       fun stun_server_list_test_case/0,
       fun spark_oauth_access_token_test_case/0,
       fun auth_profile_miniProfile_test_cas/0,
       fun rabbitmq_endpoint_test_case/0,
       fun rabbitmq_client_timeout_in_test_case/0,
       fun rabbitmq_client_retry_attempt_in_test_case/0,
       fun rest_client_timeout_in_sec_test_case/0,
       fun rest_call_retry_attempt_test_case/0,
       fun non_existent_var_test_case/0,
       fun get_mod_spark_stun_env_defaultValue_test_case/0,	
       fun get_mod_spark_stun_env_undefinedVal_test_case/0
      ]
    }.

spark_api_endpoint_test_case()->
    application:set_env(mod_spark_stun, spark_api_endpoint, "http://www.test.endpoint"),
    ?assertMatch("http://www.test.endpoint", spark_api_endpoint()).

spark_oauth_access_token_test_case()->
    application:set_env(mod_spark_stun, spark_auth_endpoint, "oauthTESTTOKEN"),
    ?assertMatch("oauthTESTTOKEN", spark_oauth_access_token()).	

stun_server_list_test_case()->
    application:set_env(mod_spark_stun, spark_auth_endpoint, ["stun.l.google.com:19302", "stun.l.google.com:19302"]),
    ?assertMatch(["stun.l.google.com:19302"], stun_server_list()).

stun_server_list_undef_test_case()->
    application:unset_env(mod_spark_stun, spark_auth_endpoint),
    ?assertMatch(undefined, stun_server_list()).

stun_server_list_test_case()->
    application:unset_env(mod_spark_stun, spark_auth_endpoint),
    application:set_env(mod_spark_stun, spark_auth_endpoint, ["stun01.sipphone.com"]),
    ?assertMatch(["stun01.sipphone.com"], stun_server_list()).
	
stun_server_backup_list_empty_list_test_case()-> 
    application:unset_env(mod_spark_stun, spark_auth_endpoint),
    ?assertMatch([], stun_server_backup_list()).

stun_server_backup_list_test_case()-> 
    application:unset_env(mod_spark_stun, spark_auth_endpoint),
    application:set_env(mod_spark_stun, spark_auth_endpoint, ["stun.schlund.de"]),
    ?assertMatch(["stun.schlund.de"], stun_server_backup_list()	

auth_profile_miniProfile_test_case()->
    application:set_env(mod_spark_stun, auth_profile_miniProfile, "miniProfle/{appId}/{brandid}/"),
    ?assertEqual("miniProfle/{appId}/{brandid}/", auth_profile_miniProfile,()).
	
rabbitmq_endpoint_test_case() ->
    application:set_env(mod_spark_stun, rabbitmq_endpoint, "rabbitMQTEST"),
    ?assertMatch("rabbitMQTEST",rabbitmq_endpoint()).

rest_client_timeout_in_sec_test_case() ->
    application:set_env(mod_spark_stun, rest_client_timeout_in_sec, 12),
    ?assertEqual(12, rest_client_timeout_in_sec()).
	
rest_call_retry_attempt_test_case() ->
    application:set_env(mod_spark_stun, rest_call_retry_attempt, 15),
    ?assertEqual(15, rest_call_retry_attempt()).
	
rabbitmq_client_timeout_in_test_case() ->
    application:set_env(mod_spark_stun, rabbitmq_client_timeout, 10),
    ?assertEqual(10, rabbitmq_client_timeout()).
 
rabbitmq_client_retry_attempt_in_test_case() ->
    application:set_env(mod_spark_stun, rabbitmq_client_retry_attempt, 19),
    ?assertEqual(19, rabbitmq_client_retry_attempt()).
	
non_existent_var_test_case() ->
    ?assertEqual(undefined, get_mod_spark_stun_env(bogus)).


get_mod_spark_stun_env_defaultValue_test_case()->
    application:set_env(test_var_with_value, ?DEFAULTVAL),
    ?assertMatch(?DEFAULTVAL,get_mod_spark_stun_env(test_var_with_value, ?DEFAULTVAL) ).

get_mod_spark_stun_env_defaultValue_test_case()->
    application:set_env(test_var_no_value, ?DEFAULTVAL),
    ?assertMatch(?DEFAULTVAL, get_mod_spark_stun_env(test_var_no_value, ?DEFAULTVAL)).


get_mod_spark_stun_env_undefinedVal_test_case() ->
    application:set_env(config_var_missing),
    ?assertMatch(undefined, get_mod_spark_stun_env(config_var_missing)).

setup() ->   
    application:load(?APP_ENV).

cleanup(_Pid) ->
    application:stop(?APP_ENV).
    
-endif.
