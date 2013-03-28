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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

%% @doc Retrieve login Data from Jid
%%      returns the {brandid, integer}, {memberid, integer} or {error, Reason}
%% @end
-spec get_loginData(UserName::string())-> {error, {brandid, atom()}, {memberid, atom()}, atom()}| {ok, {atom(), integer},  {atom(), integer()}}.
get_loginData("") ->
  {error, user_missing}
;
get_loginData(UserName) when (is_list(UserName)) ->
  CommunityAndMemberId = 
		case re:split(UserName,"-") of 
 		     [CommunityId, MemberId] -> [CommunityId, MemberId];
                     {error, Reason} -> {error, Reason};
                     Else -> {error, Else}
  		end,
  case get_brandId_from_communityId(CommunityAndMemberId) of
        {ok, {brandid, C}, {memberId, MemberId}} -> {ok, {brandid, C}, {memberId, MemberId}};
        {error, {brandid, not_found}, {memberId, not_found}, Reason} -> {error, {brandid, not_found}, {memberId, not_found}, Reason};
        Else -> {error, {brandid, not_found}, {memberId, not_found}, Else}
  end,
  RetValue;
get_loginData(_) ->
   {error, user_missing}. 

%% 
%% @doc Retrieve login Data from Jid
%%      returns the {brandid, integer}, {memberid, integer} or {error, Reason}
%% @end
-spec get_loginData(UserName::string())-> {error, tuple(), term()} | {ok, {atom(), integer},  {atom(), integer()}}.
get_brandId_from_communityId({error, Reason}) -> 
  {error, {brandid, not_found}, {memberid, not_found}, Reason}; 
get_brandId_from_communityId([CommunityId, MemberId]) when (CommunityId > 0)->
   Ids = case ejabberd_auth_spark:get_spark_auth_service_config({community2brandId,Host}) of
             {error, Reason} -> {error, Reason};             
             Val -> find_Value(CommunityId, Ids)
         end,
   Val1 = case Ids of
        {error, Reason} -> {error, Reason};
        Val2 -> find_value(CommunityId, Ids)
   end,
   BrandId = extract_brandId(Val1),
   case BrandId of
        {ok, {brandid, C}} -> {ok, {brandid, C}, {memberId, MemberId}};
        {error, Reason} -> {error, {brandid, not_found}, {memberId, not_found}, Reason}
   end;
get_brandId_from_communityId(_) -> 
  {error, {brandid, not_found}, {memberid, not_found}, mal_formed}.

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
extract_brandId({_A,_B,C}) -> {ok, {brandid, C}};
extract_brandId(_)-> {error, not_found}.


%%%%%% EUNIT %%%%%%%%%%%%%%%%%%
-ifdef(TEST).
get_login_data_emptystring_test()-> 
 	?assertEqual({error, user_missing}, get_loginData("")).
get_login_data_baddatatype_test()-> 
 	?assertEqual({error, user_missing}, get_loginData(wrongtype)).

get_loginData_test() -> 
        [?assertEqual([{ok, {brandid,}, {memberid, }}],get_loginData("12345-3")),
         ?assertEqual({error, not_exist}, get_loginData("12345-99")),
         ?assertEqual({error, not_exist}, get_loginData("12345-99")),
         ?assertEqual({error, not_exist}, get_loginData("12345-99")),
         ?assertEqual({error, not_exist}, get_loginData("12345-99")),
        ].

get_brandId_from_communityId_emptystring_test()-> 
 	?assertEqual({error, user_missing}, get_brandId_from_communityId("")).
get_brandId_from_communityId_baddatatype_test()-> 
 	?assertEqual({error, user_missing}, get_brandId_from_communityId(wrongtype)).

get_brandId_from_communityId_test() -> 
        [?assertEqual([{ok, {brandid,}, {memberid, }}],get_brandId_from_communityId("12345-3")),
         ?assertEqual({error, not_exist}, get_brandId_from_communityId("12345-99")),
         ?assertEqual({error, not_exist}, get_brandId_from_communityId("12345-99")),
         ?assertEqual({error, not_exist}, get_brandId_from_communityId("12345-99")),
         ?assertEqual({error, not_exist}, get_brandId_from_communityId("12345-99")),
        ].

-endif.
