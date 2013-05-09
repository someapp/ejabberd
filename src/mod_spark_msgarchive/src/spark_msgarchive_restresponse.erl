
%%%----------------------------------------------------------------------
%%%
%%% @author : Edward Tsang <etsang@spark.net>
%%% @doc Rest Verify the Rest Response of calling Spark Send Missed IM API
%%% Created : 24 Apr 2013
%%%---------------------------------------------------------------------
%%%
%%% Copyright (c)
%%%----------------------------------------------------------------------
%%% @end

%% @doc spark_msgarchive_restclient takes in IM messages to Spark Rest API
%% @end
-module(spark_msgarchive_restresponse).
-author('etsang@spark.net').

-export([check_sendmissedIM_response/1]).
-type restResponse()::tuple().

-include("ejabberd.hrl").
-include("include/mod_spark_msgarchive.hrl").
-include("include/mod_spark_msgarchive_version.hrl").

%%-include("jlib.hrl").
%%-include("web/ejabberd_http.hrl").
%%-include("web/ejabberd_web_admin.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

%% @private
%% @doc check for the authentication http post response for Success is true and error term is null
%%      anything else is error and considered authentication error and failed.
%% @end
-spec check_sendmissedIM_response(Body::restResponse())-> {ok, posted_api_ok}| {error, reason()}.
check_sendmissedIM_response(Body) ->
   ?DEBUG("~p Check sendmissedIM Response Body ~p~n", [?CURRENT_FUNCTION_NAME(),Body]),
   case illegal_Post_Response(Body) of
        {ok, not_badpost} -> check_for_validStatus(Body);
	{error, Reason} -> {erro, Reason}
   end.

 
%% @doc Check for rest response has all the criteria for a success call to sendmissedIM restapi 
%% @end
-spec check_for_validStatus(Body::restResponse())->{ok, posted_api_ok} | {error, reason()}.
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
-spec check_200_status(Body::restResponse()) -> {ok, posted_to_api} | {error, reason()}.
check_200_status(Body) ->
    case proplists:get_value(<<"code">>, Body) of
             <<"200">> -> {ok, posted_to_api};
	     {error, Reason} -> {error, Reason};
  	     Else -> {error, Else}
    end.

%% @doc Check for rest response status string is ok 
%% @end
-spec check_Ok_status(Body::restResponse()) -> {ok, posted_to_api} | {error, reason()}.
check_Ok_status(Body) -> 
    case proplists:get_value(<<"status">>, Body) of
    	   <<"OK">> -> {ok, posted_to_api};
	   {error, Reason} -> {error, Reason};
  	   Else -> {error, Else}
    end.

%% @doc the v2 restapi has inconsistent format for rest response. A bad post body has an extra
%% 	"Result:" level  
%% @end
-spec illegal_Post_Response(Body::restResponse()) -> {ok, not_badpost} | {error, post_unsupported}.
illegal_Post_Response(Body)->
    ?DEBUG("~p Illegal Post Response Body ~p~n", [?CURRENT_FUNCTION_NAME(),Body]),
    case proplist:get_value(<<"Result">> , Body) of
	 undefined -> {ok, not_badpost};
         _List -> {error, post_unsupported} 
    end.


%%%%%% EUNIT %%%%%%%%%%%%%%%%%%
-ifdef(TEST).

spark_msgarchive_restresponse_test_()->
    { setup,
      fun setup/0,
      fun cleanup/1,
      [
        fun sendmissedIM_rest_response_empty_test_case/0,
        fun sendmissedIM_rest_response_valid_case/0,
        fun sendmissedIM_rest_response_error_case/0,
        fun sendmissedIM_rest_response_badpost_case/0,
   	fun sendmissedIM_rest_response_notbadpost_case/0
      ]
    }.

sendmissedIM_rest_response_empty_test_case()->
   ?assertError({error, _R},check_sendmissedIM_response([])).

sendmissedIM_rest_response_valid_case() ->
   Response = [{<<"code">>, <<"200">>}, {<<"status">>,<<"OK">>}, {<<"data">>, <<"">>}], 
   ?assertMatch({ok, posted_to_api},check_sendmissedIM_response(Response)).

sendmissedIM_rest_response_error_case()->
   Response = [{<<"code">>, <<"200">>, {<<"status">>, <<"Unauthorized">>}, 
                [{<<"code">>, <<"41002">>},{<<"message">>, <<"SomeReason">>}]],
   ?assertError({error, _R}, check_sendmissedIM_response(Response)).

sendmissedIM_rest_response_badpost_case()->
  Response = [{ <<"Result">> ,{<<"code">>, <<"200">>}, {<<"status">>,<<"OK">>}, {<<"data">>, <<"">>}}], 
  ?assertError({error, post_unsupported},illegal_Post_response(Response)).

sendmissedIM_rest_response_notbadpost_case()->
  Response = [{<<"code">>, <<"500">>}, {<<"status">>,<<"NOK">>}, {<<"data">>, <<"">>}], 
  ?assertMatch({ok, not_badpost},illegal_Post_response(Response)).


setup() ->   
    ok.

cleanup(_Pid) ->
    ok.





-endif.
