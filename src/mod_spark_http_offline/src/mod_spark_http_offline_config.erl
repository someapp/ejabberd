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
getSparksOauthAccessToken(State)->
   AccessToken=getVal_for(spark_oauth_access_token, State#state.urls),
   AccessToken.

getSparkApiEndpoint(State)->
   ApiUrl = getVal_for(spark_api_endpoint, State#state.urls),
   ApiUrl.

getSendMissedIMUrl(State)->
   SendMissedIMUrl = getVal_for(send_missed_im, State#state.urls),
   SendMissedIMUrl.

getRabbitMQEndpoint(State)->
   RabbitMQEndpoint = getVal_for(rabbitmq_endpoint, State#state.urls),
   RabbitMQEndpoint.

getProfileMemberStatusEndpoint(State)->
   ProfileMemberStatus = getVal_for(profile_memberstatus, State#state.urls),
   ProfileMemberStatus.

%%%%% Extract from Setting Api function %%%%%

getBrandId(SenderId, State) ->
   CommunityIdBrandIdMap = getVal_for(community2brandId, State#state.community2brandId),
   Val = case re:split(SenderId,"-") of 
 		     		  [CommunityId, MemberId] -> [CommunityId, MemberId];
                     		  {error, Reason} -> {error, Reason};
                     	          Else -> {error, Else}
  			     end,
   
   BrandIdTuple = case Val of
	{error, _} -> {error, undefined};
	[CommunityId1, MemberId1] -> [Key,_] = Val, 
				   case lists:keyfind(Key, 2, State#state.community2brandId) of
        				{Key, Result} -> Result;
        			    	false -> {error, nothing}
    				   end
   end,
   extract_brandId(BrandIdTuple).

%%%%% Rest and RabbitMQClient %%%%%

getRestClientTimeout(State)->
   RestClientTimeout = getVal_for(rest_client_timeout_in_sec, State#state.client_settings),
   RestClientTimeout.

getRestRetryAttempt(State)->
   RestClientTimeout = getVal_for(rest_call_retry_attempt, State#state.client_settings),
   RestClientTimeout.

getRabbitMQClientTimeout(State)->
   RabbitMQClientTimeout = getVal_for(rabbitmq_client_timeout_in_sec, State#state.client_settings),
   RabbitMQClientTimeout.

getRabbitMQClientRetryAttempt(State)->
   RabbitMQClientRetryAttempt = getVal_for(rabbitmq_call_retry_attempt, State#state.client_settings),
   RabbitMQClientRetryAttempt.


%%%%% Sanity Test Settings %%%%%

getTestBrandId(State)->
   TestBrandId = getVal_for(testBrandId, State#state.sanity_test_setting), 
   TestBrandId.

getTestAppId(State)->
   TestAppId = getVal_for(testBrandId, State#state.sanity_test_setting),
   TestAppId.

getTestClientSecret(State)->
   TestClientSecret = getVal_for(testBrandId, State#state.sanity_test_setting),
   TestClientSecret.

getTestSourceMemberId(State)->
   TestSourceMemberId = getVal_for(testBrandId, State#state.sanity_test_setting),
   TestSourceMemberId.

getTestSourceMemberEmail(State)->
   TestSourceMemberEmail = getVal_for(testBrandId, State#state.sanity_test_setting),
   TestSourceMemberEmail.

getTestTargetMemberId(State)->
   TestTargetMemberId = getVal_for(testBrandId, State#state.sanity_test_setting),
   TestTargetMemberId.

getTestTargetMemberEmail(State)->
   TestTargetMemberEmail = getVal_for(testBrandId, State#state.sanity_test_setting),
   TestTargetMemberEmail.

getTestTargetMemberPassword(State)->
   TestTargetMemberPassword = getVal_for(testBrandId, State#state.sanity_test_setting),
   TestTargetMemberPassword.

%%==================================================================
%% Utility functions
%%==================================================================
getVal_for(Key, ValList) ->
    case lists:keysearch(Key,1,ValList) of
        {value,{_,Result}} -> Val = Result;
        _ -> Val = undefined
    end,
    Val.   	

url_for(Event, Urls) ->
    getVal_for(Event, Urls).

client_setting_for(ClientKey, Settings)->
    getVal_for(ClientKey, Settings).

sanity_test_setting_for(TestSetting, Settings)->
    getVal_for(TestSetting, Settings).


extract_brandId({error, not_found}) -> {error, not_found};
extract_brandId({_A,_B,C}) -> {ok, {brandid, C}};
extract_brandId(_)-> {error, not_found}.

