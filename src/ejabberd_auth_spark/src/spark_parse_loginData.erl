%%%----------------------------------------------------------------------
%%%
%%% @author : Edward Tsang <etsang@spark.net>
%%% @doc Parse and extract ejaaberd Jid data into isUserData 
%%% Created : 20 Mar 2013
%%% @end
%%%---------------------------------------------------------------------
%%%
%%% Copyright (c)
%%%
%%%----------------------------------------------------------------------

%% @doc Parse and extract ejaaberd Jid data into isUserData 
%% @end 
-module(spark_parse_loginData).
-author('etsang@spark.net').

%% External exports
-export[get_loginData/1,
        get_brandId_from_communityId/1].

%% @doc Retrieve login Data from Jid
%%      returns the {brandid, integer}, {memberid, integer} or {error, Reason}
%% @end
-spec get_isUserData(UserName::string())-> {error, atom()} | {ok, {atom(), integer},  {atom(), integer()}}.
get_loginData("") ->
  {error, user_missing}
;
get_loginData(UserName) when (is_list(UserName)) ->
  Community&MemberId = 
		case re(UserName,"-") of 
 		     [CommunityId, MemberId] -> [CommunityId, MemberId]
                     {error, Reason} -> {error, Reason}
                     Else -> {error, Else}
  		end,
  BrandId = case get_brandId_from_communityId(Community&MemberId) of
                 {brandid, BrandId} -> BrandId,
                 {error, Reason} -> {error, Reason}
                 Else -> {error, Else}
            end. 

%% 
%% @doc Retrieve login Data from Jid
%%      returns the {brandid, integer}, {memberid, integer} or {error, Reason}
%% @end

get_brandId_from_communityId(CommunityId) when (CommunityId > 0)->
   Ids = case ejabberd_auth_spark:get_spark_auth_service_config({community2brandId,Host}) of
             {error, Reason} -> {error, Reason},             
             Val -> find_Value(CommunityId, Ids)
         end,
   Val1 = case Ids of
        {error, Reason} -> {error, Reason},
        Val2 -> find_value(CommunityId, Ids)
   end,
   BrandId = extract_brandId(Val1),
   BrandId.


%% @private
%% @doc get community / brand id mapping from configure file
%% @end

get_spark_auth_service_config(Host, TokenName) ->
    case ejabberd_config:get_local_option({TokenName, Host}) of
	undefined -> {error, not_found};
	Val   -> Val
    end.

find_value(Key, List) ->
    case lists:keyfind(Key, 2, List) of
        {Key, Result} -> Result;
        false -> {error, nothing}
    end.

extract_brandId({error, not_found}) -> {error, not_found};
extract_brandId({_A,_B,C}) -> C;
extract_brandId(_)-> {error, not_found}.
