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

-module(mod_spark_http_offline_config).

-author("etsang").

-include_lib("../include/mod_spark_http_offline.hrl").

-export([
	getSparksOauthAccessToken/1,
	getSparkApiEndpoint/1,
	getSendMissedIMUrl/1,
	getRabbitMQEndpoint/1,
	getProfileMemberStatusEndpoint/1,
	getBrandId/2,
	getRestClientTimeout/1,
	getRestRetryAttempt/1,
	getRabbitMQClientTimeout/1,
	getRabbitMQClientRetryAttempt/1,
	getTestBrandId/1,
	getTestAppId/1,
	getTestClientSecret/1,
	getTestTargetMemberId/1,
	getTestTargetMemberEmail/1,
	getTestTargetMemberPassword/1		
	]).

%%====================================================================
%% Get Configuration functions
%%====================================================================

%% Service Api functions
%% @doc Get the OAuth Access Token from state record
%% 
%% @end
-spec getSparksOauthAccessToken(configSetting())-> string() | {error, reason()}.
 getSparksOauthAccessToken(State)->
   AccessToken=getVal_for(spark_oauth_access_token, State#state.urls),
   AccessToken.

-spec getSparkApiEndpoint(configSetting())-> string() | {error, reason()}.
getSparkApiEndpoint(State)->
   ApiUrl = url_for(spark_api_endpoint, State#state.urls),
   ApiUrl.

-spec getSendMissedIMUrl(configSetting())-> string() | {error, reason()}.
getSendMissedIMUrl(State)->
   SendMissedIMUrl = url_for(send_missed_im, State#state.urls),
   SendMissedIMUrl.

-spec getRabbitMQEndpoint(configSetting())-> string() | {error, reason()}.
getRabbitMQEndpoint(State)->
   RabbitMQEndpoint = url_for(rabbitmq_endpoint, State#state.urls),
   RabbitMQEndpoint.

-spec getProfileMemberStatusEndpoint(configSetting())-> string() | {error, reason()}.
getProfileMemberStatusEndpoint(State)->
   ProfileMemberStatus = url_for(profile_memberstatus, State#state.urls),
   ProfileMemberStatus.

%%%%% Extract from Setting Api function %%%%%

-spec getBrandId(string(), configSetting())-> {error, reason()} | {ok, integer()}.
getBrandId(SenderId, State) ->
   
   Val = case re:split(SenderId,"-") of 
 		     		  [CommunityId, MemberId] -> [CommunityId, MemberId];
                     		  {error, Reason} -> {error, Reason};
                     	          Else -> {error, Else}
  			     end,
   
   BrandIdTuple = case Val of
	{error, _} -> {error, undefined};
	[CommunityId1, _MemberId1] -> [Key,_] = Val, 
				   case lists:keyfind(CommunityId1, 2, State#state.community2brandId) of
        				{Key, Result} -> Result;
        			    	false -> {error, nothing}
    				   end
   end,
   extract_brandId(BrandIdTuple).

%%%%% Rest and RabbitMQClient %%%%%
-spec getRestClientTimeout(configSetting())-> string() | {error, reason()}.
getRestClientTimeout(State)->
   RestClientTimeout = client_setting_for(rest_client_timeout_in_sec, State#state.client_settings),
   RestClientTimeout.

-spec getRestRetryAttempt(configSetting())-> string() | {error, reason()}.
getRestRetryAttempt(State)->
   RestClientTimeout = client_setting_for(rest_call_retry_attempt, State#state.client_settings),
   RestClientTimeout.

-spec getRabbitMQClientTimeout(configSetting())-> string() | {error, reason()}.
getRabbitMQClientTimeout(State)->
   RabbitMQClientTimeout = client_setting_for(rabbitmq_client_timeout_in_sec, State#state.client_settings),
   RabbitMQClientTimeout.

-spec getRabbitMQClientRetryAttempt(configSetting())-> string() | {error, reason()}.
getRabbitMQClientRetryAttempt(State)->
   RabbitMQClientRetryAttempt = client_setting_for(rabbitmq_call_retry_attempt, State#state.client_settings),
   RabbitMQClientRetryAttempt.


%%%%% Sanity Test Settings %%%%%

-spec getTestBrandId(configSetting())-> string() | {error, reason()}.
getTestBrandId(State)->
   TestBrandId = sanity_test_setting_for(testBrandId, State#state.sanity_test_setting), 
   TestBrandId.

-spec getTestAppId(configSetting())-> string() | {error, reason()}.
getTestAppId(State)->
   TestAppId = sanity_test_setting_for(testAppId, State#state.sanity_test_setting),
   TestAppId.

-spec getTestClientSecret(configSetting())-> string() | {error, reason()}.
getTestClientSecret(State)->
   TestClientSecret = sanity_test_setting_for(testClientSecret, State#state.sanity_test_setting),
   TestClientSecret.

-spec getTestSourceMemberId(configSetting())-> string() | {error, reason()}.
getTestSourceMemberId(State)->
   TestSourceMemberId = sanity_test_setting_for(testSourceMemberId, State#state.sanity_test_setting),
   TestSourceMemberId.

-spec getTestSourceMemberEmail(configSetting())-> string() | {error, reason()}.
getTestSourceMemberEmail(State)->
   TestSourceMemberEmail = sanity_test_setting_for(testSourceMemberEmail, State#state.sanity_test_setting),
   TestSourceMemberEmail.

-spec getTestSourceMemberPassword(configSetting())-> string() | {error, reason()}.
getTestSourceMemberPassword(State)->
   TestSourceMemberPassword = sanity_test_setting_for(testSourceMemberPassword, State#state.sanity_test_setting),
   TestSourceMemberPassword.

-spec getTestTargetMemberId(configSetting())-> string() | {error, reason()}.
getTestTargetMemberId(State)->
   TestTargetMemberId = sanity_test_setting_for(testTargetMemberId, State#state.sanity_test_setting),
   TestTargetMemberId.

-spec getTestTargetMemberEmail(configSetting())-> string() | {error, reason()}.
getTestTargetMemberEmail(State)->
   TestTargetMemberEmail = sanity_test_setting_for(testTargetMemberEmail, State#state.sanity_test_setting),
   TestTargetMemberEmail.

-spec getTestTargetMemberPassword(configSetting())-> string() | {error, reason()}.
getTestTargetMemberPassword(State)->
   TestTargetMemberPassword = sanity_test_setting_for(testTargetMemberPassword, State#state.sanity_test_setting),
   TestTargetMemberPassword.

%%==================================================================
%% Utility functions
%%==================================================================
-spec getVal_for(atom(), any())->any().
getVal_for(Key, ValList) ->
    Val = case lists:keysearch(Key,1,ValList) of
        {value,{_,Result}} -> Result;
        _ -> undefined
    end,
    Val.   	

-spec url_for(atom(), any())->undefined | list().
url_for(Event, Urls) ->
    getVal_for(Event, Urls).

-spec client_setting_for(atom(), any())->undefined | list().
client_setting_for(ClientKey, Settings)->
    getVal_for(ClientKey, Settings).

-spec sanity_test_setting_for(atom(), any())->undefined | list().
sanity_test_setting_for(TestSetting, Settings)->
    getVal_for(TestSetting, Settings).

extract_brandId({error, not_found}) -> {error, not_found};
extract_brandId({_A,_B,C}) -> {ok, {brandid, C}};
extract_brandId(_)-> {error, not_found}.

