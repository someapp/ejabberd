
%%%----------------------------------------------------------------------
%%%
%%% @author : Edward Tsang <etsang@spark.net>
%%% @doc Rest client to send missed IM messages to Spark Rest API
%%% Created : 24 Apr 2013
%%%---------------------------------------------------------------------
%%%
%%% Copyright (c)
%%%----------------------------------------------------------------------
%%% @end

%% @doc mod_spark_http_offline_restclient takes in IM messages to Spark Rest API
%% @end
-module(mod_spark_http_offline_restclient).
-author('etsang@spark.net').

-include("../include/mod_spark_http_offline.hrl").
-include("../include/ejabberd.hrl").
-include("../include/jlib.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-export([post_to_restapi_message/5]).

post_to_restapi_message(SenderId, RecipientId, AccessToken, Body, State) ->
		?INFO_MSG("Posting From ~p To ~p Body ~p~n",[SenderId, RecipientId, Body]),
		BrandId = mod_spark_http_offline_config:getBrandId(SenderId, State),			
		Response = case post_to_restapi(BrandId, AccessToken, RecipientId, Body, State) of
			{ok, _} -> ok;
			{error, Reason} -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Reason]),
					    ok;
			Else -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Else]),
				ok
		end,	
		Ret = case check_sendmissedIM_response(Response) of
			{ok, _} -> ?INFO_MSG("post request sent", []),
				   ok;
			{warn, _Warning} -> ?WARNING_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), _Warning]),
					    ok;
			{error, _Reason} -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), _Reason]),
					    {error, _Reason};
			_Else -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), _Else]),
				 {error, _Else}
		     end,	
		Ret.


post_to_restapi(BrandId, AccessToken, RecipientId, Messages, State) ->
	BaseServiceEndpoint = mod_spark_http_offline_config:getSparkApiEndpoint(State),
	SendMissedIMUrl = mod_spark_http_offline_config:getSendMissedIMUrl(State),
        ResourceEndpoint1 = re:replace(SendMissedIMUrl, "{brandId}", BrandId, [global, {return, list}]),
	Url = restc:construct_url(BaseServiceEndpoint, ResourceEndpoint1,
					[{"access_token", AccessToken},
					 {"&ts",get_timestamp()},
					 {"RecipientMemberId",RecipientId },
					 {"Messages", Messages}]),

	Response = case restc:request(post, json, Url, [200],[],[""]) of 
			{ok, S} -> ?INFO_MSG("post request sent", []),
				   {ok, S};
			{warn, Warning} -> ?WARNING_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Warning]),
					  {warn, Warning};
			{error, Reason} -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Reason]),
					    {error, Reason};
			Else -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Else]),
				 {error, Else}
		   end,
	Response.

%% @private
%% @doc check for the authentication http post response for Success is true and error term is null
%%      anything else is error and considered authentication error and failed.
%% @end
%%-spec check_sendmissedIM_response(Body::restResponse())-> {ok, posted_api_ok}| {error, reason()}.
check_sendmissedIM_response(Body) ->
   ?DEBUG("~p Check sendmissedIM Response Body ~p~n", [?CURRENT_FUNCTION_NAME(),Body]),
   case illegal_Post_Response(Body) of
        {ok, not_badpost} -> check_for_validStatus(Body);
	{error, Reason} -> {erro, Reason}
   end.

 
%% @doc Check for rest response has all the criteria for a success call to sendmissedIM restapi 
%% @end
%%-spec check_for_validStatus(Body::restResponse())->{ok, posted_api_ok} | {error, reason()}.
check_for_validStatus(Body) ->
   V = case check_200_status(Body) of
	 {ok, posted_to_api} -> {ok, posted_to_api};
 	 {error, Reason} -> {error, Reason}
       end,
   case V of
	{ok, posted_to_api}-> case check_Ok_status(Body) of
          			   {ok, posted_api_ok} -> {ok, posted_api_ok};
          			   {error, Reason1} -> {error, Reason1}
          		      end;
        {error, Else} -> {error, Else}
   end.


%% @doc Check for rest response http status is 200 
%% @end
%%-spec check_200_status(Body::restResponse()) -> {ok, posted_to_api} | {error, reason()}.
check_200_status(Body) ->
    case proplists:get_value(<<"code">>, Body) of
             <<"200">> -> {ok, posted_to_api};
	     {error, Reason} -> {error, Reason};
  	     Else -> {error, Else}
    end.

%% @doc Check for rest response status string is ok 
%% @end
%%-spec check_Ok_status(Body::restResponse()) -> {ok, posted_to_api} | {error, reason()}.
check_Ok_status(Body) -> 
    case proplists:get_value(<<"status">>, Body) of
    	   <<"OK">> -> {ok, posted_to_api};
	   {error, Reason} -> {error, Reason};
  	   Else -> {error, Else}
    end.

%% @doc the v2 restapi has inconsistent format for rest response. A bad post body has an extra
%% 	"Result:" level  
%% @end
%%-spec illegal_Post_Response(Body::restResponse()) -> {ok, not_badpost} | {error, post_unsupported}.
illegal_Post_Response(Body)->
    ?DEBUG("~p Illegal Post Response Body ~p~n", [?CURRENT_FUNCTION_NAME(),Body]),
    case proplist:get_value(<<"Result">> , Body) of
	 undefined -> {ok, not_badpost};
         _List -> {error, post_unsupported} 
    end.

get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.

