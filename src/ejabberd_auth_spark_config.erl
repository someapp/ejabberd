
%%%----------------------------------------------------------------------
%%%
%%% @author : Edward Tsang <etsang@spark.net>
%%% @doc Retrieve configurtion for spark authentication
%%% Created : 20 Mar 2013
%%%---------------------------------------------------------------------
%%%
%%% Copyright (c)
%%%
%%%----------------------------------------------------------------------
%%% @end

%% @doc Retrieve configurtion for spark authentication
%%      
%% @end
-module(ejabberd_auth_spark_config).
-author('etsang@spark.net').

%% External exports

-export([
	get_spark_authservice_endpoint/1,
        get_spark_client_secrete/1,
        get_spark_application_id/1, 
	get_rest_client_timeout_in_sec/1, 
	get_rest_call_retry_attempt/1,
	get_spark_communityId_brandId_mapping/1
	]).
%TODO change it to true at end of cycle
-define(TEST, true).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-include("ejabberd.hrl").


%% @doc get the rest api authentication endpoint from config file 
%% @end
-spec get_spark_authservice_endpoint(Host::string()) -> string() | {error, endpoint_notfound}.
get_spark_authservice_endpoint(Host) ->
    get_spark_auth_service_config(Host, spark_auth_endpoint). 

%% @private
%% @doc get the rest applicationId from config file
-spec get_spark_application_id(Host::string()) -> string() | {error, not_found}.
get_spark_application_id(Host) ->
    get_spark_auth_service_config(Host, spark_application_id). 
   


%% @private
%% @doc get the rest client secrete from config file 
%% @end
-spec get_spark_client_secrete(Host::string()) -> integer() | {error, not_found}.
get_spark_client_secrete(Host) ->
    case get_spark_auth_service_config(Host,spark_client_secrete) of
       {error, REASON} -> {error, REASON}; 	
       HasValue -> string_to_integer(HasValue)
    end. 

%% @private
%% @doc get the rest client time out value in seconds from config file 
%% @end
-spec get_rest_client_timeout_in_sec(Host::string()) -> integer() | {error, not_found}.
get_rest_client_timeout_in_sec(Host) ->
    Val = case get_spark_auth_service_config(Host,rest_client_timeout_in_sec) of
       {error, REASON} -> {error, REASON}; 
        HasValue -> string_to_integer(HasValue)
    end,
    case Val of 
       {error, _} -> 15;
       Else -> Else
    end.

%% @private
%% @doc get the rest client call retry attempt from config file
%% @end
-spec get_rest_call_retry_attempt(Host::string()) -> integer() | {error, not_found}.
get_rest_call_retry_attempt(Host) ->
    Val = case get_spark_auth_service_config(Host,rest_call_retry_attempt) of
        {error, REASON} -> {error, REASON}; 
    	HasValue -> HasValue
    end,
    
    case string_to_integer(Val) of 
        {error, _Reason} -> 0;
        Else -> Else
    end
    .

%% @private
%% @doc Get the CommunityId to BrandId maping from config
%% @end
-spec get_spark_communityId_brandId_mapping(Host::string()) -> {tuple()} | {error, not_found}.
get_spark_communityId_brandId_mapping(Host) ->
    case get_spark_auth_service_config(Host,community2brandId) of
       {error, REASON} -> {error, REASON}; 	
       HasValue -> HasValue
    end. 



%% @private
%% @doc get the authentication endpoint rest client to talk to
%% @end
get_spark_auth_service_config(Host, TokenName) ->
    case ejabberd_config:get_local_option({TokenName, Host}) of
	undefined -> {error, not_found};
	Val   -> Val
    end.

%% @private
%% @doc convert string to integer
%% @end
-spec string_to_integer(StringValue::string()) -> integer() | {error, atom()}.
string_to_integer("") -> 
	{error, cannot_convert};
string_to_integer(StringValue) when is_list(StringValue)->
    case string:to_integer(StringValue) of
    	{Int, _} -> Int;
        _ -> {error, cannot_convert}
    end;
string_to_integer(_) -> 
	{error, cannot_convert}.
	
%%%%%% EUNIT %%%%%%%%%%%%%%%%%%
-ifdef(TEST).
string_to_integer_emptystring_test() ->[?assertEqual({error, cannot_convert}, string_to_integer("")), 
				       ?assertEqual({error, cannot_convert}, string_to_integer(badtype))].

-endif.
