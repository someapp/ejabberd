%% -------------------------------------------------------------------
%% @doc
%% mod_spark_msgarchive: Core Riak Application
%% An erlang module to save and publish the delayed/undelivered im messages to rabbimq
%%
%%
%% @end
%% -------------------------------------------------------------------

%% @doc A module to provide access to mod_spark_msgarchive configuration information.
-type spark_msgarchive_bucketprops() = [{Propkey :: atom(), Propval :: term()}]

-module(mod_spark_msgarchive_config).
-author('etsang@spark.net').

-export([spark_api_endpoint/0, 
	 spark_oauth_access_token/0, 
         send_missed_im_endpoint/0, 
	 auth_profile_miniProfile/0, 
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
-define(DEFAULT_RABBITCONN_TIMEOUT,5).
-define(DEFAULT_RABBITCONN_RETRY,3).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Get spark api endpoint environment variable.
%% @end
-spec spark_api_endpoint()-> url() | undefined.
spark_api_endpoint() ->
    get_mod_spark_msgarchive_env(spark_api_endpoint).

%% @doc Get the oauth access token environment variable.
-spec spark_oauth_access_token() -> accessToken() | undefined.
spark_oauth_access_token() ->
    get_mod_spark_msgarchive_env(spark_oauth_access_token).  

%% @doc Get the sendMissedIm endpoint environment variable.
%% @end
-spec send_missed_im_endpoint() -> string() | undefined.
send_missed_im_endpoint() ->
    get_mod_spark_msgarchive_env(send_missed_im).

%% @doc Get api miniprofile environment variable.
%% @end
-spec auth_profile_miniProfile()-> string() | undefined.
auth_profile_miniProfile() ->
    get_mod_spark_msgarchive_env(auth_profile_miniProfile).

%% @doc Get the rabbitmq endpoint environment variable.
%% @end
-spec rabbitmq_endpoint()
rabbitmq_endpoint() ->
    get_mod_spark_msgarchive(rabbitmq_endpoint).

%% @doc Get the rabbitmq endpoint environment variable.
%% @end
-spec rest_client_timeout_in_sec() -> integer().
rest_client_timeout_in_sec() ->
      get_mod_spark_msgarchive_env(rest_client_timeout_in_sec, ?DEFAULT_RESTCONN_TIMEOUT).    

%% @doc Get rest client call retry attempt environment variable.
%% @end
-spec rest_call_retry_attempt() -> integer().
rest_call_retry_attempt() ->
       get_mod_spark_msgarchive_env(rest_call_retry_attempt, ?DEFAULT_RESTCONN_RETRY).

%% @doc Get the rabbitmq client connection timeout environment variable.
%% @end
-spec rabbitmq_client_timeout_in_sec() -> integer().
rabbitmq_client_timeout_in_sec() ->
       get_mod_spark_msgarchive_env(rabbitmq_client_timeout_in_sec, ?DEFAULT_RABBITCONN_TIMEOUT).

%% @doc Get ther abbitmq client connection retry attempt  variable.
%% @end
-spec rabbitmq_client_retry_attempt()-> integer().
rabbitmq_client_retry_attempt() ->
       get_mod_spark_msgarchive_env(rabbitmq_call_retry_attempt, ?DEFAULT_RABBITCONN_RETRY).


%% @private
get_mod_spark_msgarchive_env(Key) ->
   case app_helper:get_env(?APP_ENV, Key) of
	undefined -> ?ERROR_MSG("Required envarible missing ~p ~p~n",[?CURRENT_FUNCTION_NAME(), {Key, undefined}]),
		      undefined;
	Value -> Value
   end;
get_mod_spark_msgarchive_env(Key, DEFAULT) ->
   app_helper:get_env(?APP_ENV, Key, DEFAULT).


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
       fun send_missed_im_endpoint_test_case/0,
       fun spark_oauth_access_token_test_case/0,
       fun auth_profile_miniProfile_test_cas/0,
       fun rabbitmq_endpoint_test_case/0,
       fun rabbitmq_client_timeout_in_test_case/0,
       fun rabbitmq_client_retry_attempt_in_test_case/0,
       fun rest_client_timeout_in_sec_test_case/0,
       fun rest_call_retry_attempt_test_case/0,
       fun non_existent_var_test_case/0,
       fun get_mod_spark_msgarchive_env_defaultValue_test_case/0,	
       fun get_mod_spark_msgarchive_env_undefinedVal_test_case/0
      ]
    }.

http_ip_and_port_test_case() ->
    ?assertEqual(error, http_ip_and_port()),
    %% Test the pre-0.14 style config
    application:set_env(mod_spark_msgarchive, web_ip, "127.0.0.1"),
    application:set_env(mod_spark_msgarchive, web_port, 8098),
    ?assertEqual({"127.0.0.1", 8098}, http_ip_and_port()),
    %% Test the config for 0.14 and later
    application:set_env(mod_spark_msgarchive, http, [{"localhost", 9000}]),
    ?assertEqual({"localhost", 9000}, http_ip_and_port()),

    [application:unset_env(mod_spark_msgarchive, K) || K <- [web_ip, web_port, http]],
    ok.

default_bucket_props_test_case() ->
    DefaultBucketProps = [{allow_mult,false},
                          {chash_keyfun,{mod_spark_msgarchive_util,chash_std_keyfun}},
                          {last_write_wins,false},
                          {n_val,3},
                          {postcommit,[]},
                          {precommit,[]}],
    application:set_env(mod_spark_msgarchive, default_bucket_props, DefaultBucketProps),
    ?assertEqual(DefaultBucketProps, default_bucket_props()).

target_n_val_test_case() ->
    ?assertEqual(4, target_n_val()).

gossip_interval_test_case() ->
    %% Explicitly set the value because other
    %% unit tests change the default.
    application:set_env(mod_spark_msgarchive, gossip_interval, 60000),
    ?assertEqual(60000, gossip_interval()).

cluster_name_test_case() ->
    ?assertEqual("default", cluster_name()).

ring_creation_size_test_case() ->
    %% Explicitly set the value because other
    %% unit tests change the default.
    application:set_env(mod_spark_msgarchive, ring_creation_size, 64),
    ?assertEqual(64, ring_creation_size()).

ring_state_dir_test_case() ->
    ?assertEqual("data/ring", ring_state_dir()).


%% ===================================================================


spark_api_endpoint_test_case()->
    application:set_env(mod_spark_msgarchive, spark_api_endpoint, "http://www.test.endpoint"),
    ?assertMatch("http://www.test.endpoint", spark_api_endpoint()).

spark_oauth_access_token_test_case()->
    application:set_env(mod_spark_msgarchive, spark_auth_endpoint, "oauthTESTTOKEN"),
    ?assertMatch("oauthTESTTOKEN", spark_oauth_access_token()).	

send_missed_im_endpoint_test_case()->
    application:set_env(mod_spark_msgarchive, spark_auth_endpoint, "sendmissiedIm/{appId}/{brandid}/"),
    ?assertMatch("sendmissiedIm/{appId}/{brandid}/", send_missed_im_endpoint()).
	
auth_profile_miniProfile_test_case()->
    application:set_env(mod_spark_msgarchive, auth_profile_miniProfile, "miniProfle/{appId}/{brandid}/"),
    ?assertEqual("miniProfle/{appId}/{brandid}/", auth_profile_miniProfile,()).
	
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
	
non_existent_var_test_case() ->
    ?assertEqual(undefined, get_mod_spark_msgarchive_env(bogus)).


get_mod_spark_msgarchive_env_defaultValue_test_case()->
    application:set_env(test_var_with_value, ?DEFAULTVAL),
    ?assertMatch(?DEFAULTVAL,get_mod_spark_msgarchive_env(test_var_with_value, ?DEFAULTVAL) ).

get_mod_spark_msgarchive_env_defaultValue_test_case()->
    application:set_env(test_var_no_value, ?DEFAULTVAL),
    ?assertMatch(?DEFAULTVAL, get_mod_spark_msgarchive_env(test_var_no_value, ?DEFAULTVAL)).


get_mod_spark_msgarchive_env_undefinedVal_test_case() ->
    application:set_env(config_var_missing),
    ?assertMatch(undefined, get_mod_spark_msgarchive_env(config_var_missing)).

setup() ->   
    application:load(?APP_ENV).

cleanup(_Pid) ->
    application:stop(?APP_ENV).
    
-endif.
