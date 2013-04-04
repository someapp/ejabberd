%%%----------------------------------------------------------------------
%%%
%%% @author : Edward Tsang <etsang@spark.net>
%%% @doc Parse and extract ejaaberd Jid data into isUs/erData 
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
-export[get_loginData/2,
        get_brandId_from_communityId/2].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.
-include("ejabberd.hrl").
-define(CURRENT_FUNCTION_NAME(), element(2, element(2, process_info(self(), current_function)))).

%% @doc Retrieve login Data from Jid
%%      returns the {brandid, integer}, {memberid, integer} or {error, Reason}
%% @end
-spec get_loginData(UserName::string(), Host::string())-> {error, {brandid, atom()}, {memberid, atom()}, atom()}| {ok, {atom(), integer()},  {atom(), integer()}}.
get_loginData("",_) ->
  {error, user_missing}
;
get_loginData(UserName, Host) when (is_list(UserName)) ->
  CommunityAndMemberId = 
		case re:split(UserName,"-") of 
 		     [CommunityId, MemberId] -> [CommunityId, MemberId];
                     {error, Reason} -> {error, Reason};
                     Else -> {error, Else}
  		end,
  ?DEBUG("~p CommunityAndMemberId ~p~n", [?CURRENT_FUNCTION_NAME(), CommunityAndMemberId]),
  case get_brandId_from_communityId(CommunityAndMemberId,Host) of
        {ok, {brandid, C}, {memberId, Id}} -> 
					     {ok, {brandid, C}, {memberId, Id}};
        {error, {brandid, not_found}, {memberId, not_found}, _Reason} -> 
									{error, {brandid, not_found}, {memberId, not_found}, _Reason};
        _Else -> 
		{error, {brandid, not_found}, {memberId, not_found}, _Else}
  end;
get_loginData(_,_) ->
   {error, user_missing}. 

%% @private
%% @doc Get the CommunityId to BrandId maping from config
%% @end
%-spec get_spark_communityId_brandId_mapping(Host::string()) -> {tuple()} | {error, not_found}.
%get_spark_communityId_brandId_mapping(Host) ->
%    case ejabberd_auth_spark:get_spark_auth_service_config(Host) of
%       {error, REASON} -> {error, REASON}; 	
%       HasValue -> HasValue
%    end. 

%% 
%% @doc Retrieve login Data from Jid
%%      returns the {brandid, integer}, {memberid, integer} or {error, Reason}
%% @end
%-spec get_brandId_from_communityId([CommunityId::integer(),MemberId::integer()], Host::string())-> {error, tuple(), term()} | {ok, {atom(), integer()},  {atom(), integer()}}.
get_brandId_from_communityId([CommunityId, MemberId], Host) when (CommunityId > 0)->
   ?DEBUG("~p CommunityId ~p, MemberID ~p~n", [?CURRENT_FUNCTION_NAME(), CommunityId, MemberId]), 
   Ids = case ejabberd_auth_spark_config:get_spark_communityId_brandId_mapping(Host) of
             {error, Reason} -> {error, Reason};             
             Val -> Val
         end,
   ?DEBUG("~p Config Read BrandI-CommunityId Map ~p~n", [?CURRENT_FUNCTION_NAME(), Ids]),
   Val1 = case Ids of
        {error, Reason1} -> {error, Reason1};
        Val2 -> find_value(CommunityId, Ids)
   end,
   ?DEBUG("~p Get CommunityId ~p~n", [?CURRENT_FUNCTION_NAME(), Val1]),
   BrandId = extract_brandId(Val1),
   ?DEBUG("~p Extracted BrandId from CommunityId ~p~n", [?CURRENT_FUNCTION_NAME(), BrandId]),
   Val3 = case BrandId of
        {ok, {brandid, C}} -> {ok, {brandid, C}, {memberId, MemberId}};
        {error, Reason2} -> {error, {brandid, not_found}, {memberId, not_found}, Reason2}
   end,
   ?DEBUG("~p Get BranId from CommunityId ~p~n", [?CURRENT_FUNCTION_NAME(), Val3]),
   Val3;
get_brandId_from_communityId({error, Reason}, _) -> 
  {error, {brandid, not_found}, {memberid, not_found}, Reason}; 
get_brandId_from_communityId(_,_) -> 
  {error, {brandid, not_found}, {memberid, not_found}, mal_formed}.

%% @private
%% @doc get community / brand id mapping from configure file
%% @end


find_value(Key, List) ->
    Key1 =binary_to_number(Key),
    ?DEBUG("~p Key ~p Integer Key ~p List ~p~n", [?CURRENT_FUNCTION_NAME(), Key, Key1, List]),    
    case lists:keyfind(Key1, 2, List) of
        {Key, Result} -> Result;
        false -> {error, nothing}
    end.

extract_brandId({error, not_found}) -> {error, not_found};
extract_brandId({_A,_B,C}) -> {ok, {brandid, C}};
extract_brandId(_)-> {error, not_found}.

binary_to_number(B) ->
   list_to_number(binary_to_list(B)).

list_to_number(L) when is_list(L) -> 
   try list_to_float(L)
   catch 
        error:badarg -> list_to_integer(L);
        {error, badarg} -> list_to_integer(L);
        _ -> {error, badarg}
   end.

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
