
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

-export([check_missedIM_rest_response/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

%% @doc 
%% @end
-spec check_missedIM_rest_response() -> {ok, delivered} | {error, reason()}.
check_missedIM_rest_response(Body)->
    ?DEBUG("~p Check Send MissedIM Response Body ~p~n", [?CURRENT_FUNCTION_NAME(),Body]),
    
.

%% @private
%% @doc check for the authentication http post response for Success is true and error term is null
%%      anything else is error and considered authentication error and failed.
%% @end
-spec check_auth_response(AuthStatus::[tuple()]) -> {ok, authenticated} | {error, term()} | term().
check_sendmissedIM_response(Body) ->
   ?DEBUG("~p Check sendmissedIM Response Body ~p~n", [?CURRENT_FUNCTION_NAME(),Body]),
   case Illegal_Post_response(Body)->
        {ok, not_badpost} -> check_for_validStatus(Body);
	{error, Reason} -> {erro, Reason}
   end.
  
check_for_validStatus(Body) ->
   V = case check_200_status(Body) of
	 {ok, posted_to_api} -> {ok, posted_to_api};
 	 {error, Reason} -> {error, Reason};
       end,
   V1 = case check_Ok_status(Body) of
          {ok, posted_api_ok} -> {ok, posted_api_ok};
          {error, Reason} -> {error, Reason};
        end,
   V1.
   end.

check_200_status(Body) ->
    case proplists:get_value(<<"code">>) of
             <<"200">> -> {ok, posted_to_api};
	     {error, Reason} -> {error, Reason};
  	     Else -> {error, Else}
    end.

check_Ok_status(Body) -> 
    case proplists:get_value(<<"status">>) of
    	   <<"OK">> -> {ok, posted_to_api};
	   {error, Reason} -> {error, Reason};
  	   Else -> {error, Else}
    end.

Illegal_Post_response(Body)->
    ?DEBUG("~p Illegal Post Response Body ~p~n", [?CURRENT_FUNCTION_NAME(),Body]),
    case proplist:get_value(<<"Result">> , Body) of
	 undefined -> {ok, not_badpost},
         _List -> {error, post_unsupported} 
    end.


%%%%%% EUNIT %%%%%%%%%%%%%%%%%%
-ifded(TEST).

spark_msgarchive_restresponse_test_()->
    { setup,
      fun setup/0,
      fun cleanup/1,
      [
       fun sendmissedIM_rest_response_empty_test_case/0,
       fun sendmissedIM_rest_response_valid_case/0,
       fun sendmissedIM_rest_response_error_case/0
      ]
    }.

.


sendmissedIM_rest_response_empty_test_case()->
   ?assertError({error, _R},check_sendmissedIM_response([])).

sendmissedIM_rest_response_valid_case() ->
   Response = [{<<"code">>, <<"200">>}, {<<"status">>,<<"OK">>}, {<<"data">>, <<"">>}], 
   ?assertMatch({ok, posted_to_api},check_sendmissedIM_response(Response)).

sendmissedIM_rest_response_error_case()->
   Response = [{<<"code">>, <<"200">>, {<<"status">>, <<"Unauthorized">>}, 
                [{<<"code">>, <<"41002">>},{<<"message">>, <<"SomeReason">>}]],
   ?assertError({error, _R}, check_sendmissedIM_response(Response)).

setup() ->   
    ok.

cleanup(_Pid) ->
    ok.

-endif
