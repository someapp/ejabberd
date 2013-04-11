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
%%      returns the {brandId, integer}, {memberId, integer} or {error, Reason}
%% @end
-spec get_loginData(UserName::string(), Host::string())-> {error, {brandId, atom()}, {memberId, atom()}, atom()}| {ok, {atom(), integer()},  {atom(), integer()}}.
get_loginData("",_) ->
  {error, user_missing}
;
get_loginData(UserName, Host) when (is_list(UserName)) ->
  CommunityAndMemberId = 
		case re:split(UserName,"-") of 
 		     [MemberId, CommunityId] -> [MemberId, CommunityId];
                     {error, Reason} -> {error, Reason};
                     Else -> {error, Else}
  		end,
  ?DEBUG("~p CommunityAndMemberId ~p~n", [?CURRENT_FUNCTION_NAME(), CommunityAndMemberId]),
  Ret = case get_brandId_from_communityId(CommunityAndMemberId,Host) of
        {ok, {brandId, C}, {memberId, Id}} -> 
					     {ok, {brandId, C}, {memberId, Id}};
        {error, {brandId, not_found}, {memberId, not_found}, _Reason} -> 
									{error, {brandId, not_found}, {memberId, not_found}, _Reason};
        _Else -> 
		{error, {brandId, not_found}, {memberId, not_found}, _Else}
  end,
  ?DEBUG("~p Returning CommunityAndMemberId ~p~n", [?CURRENT_FUNCTION_NAME(), Ret]),
  Ret;
get_loginData(_,_) ->
   {error, user_missing}. 

%% 
%% @doc Retrieve login Data from Jid
%%      returns the {brandId, integer}, {memberId, integer} or {error, Reason}
%% @end
%-spec get_brandId_from_communityId([MemberId::integer(),CommunityId::integer()], Host::string())-> {error, tuple(), term()} | {ok, {atom(), integer()},  {atom(), integer()}}.
get_brandId_from_communityId([MemberId, CommunityId], Host) when (CommunityId > 0)->
   ?DEBUG("~p CommunityId ~p, MemberID ~p~n", [?CURRENT_FUNCTION_NAME(), CommunityId, MemberId]), 
   Ids = case ejabberd_auth_spark_config:get_spark_communityId_brandId_mapping(Host) of
             {error, Reason} -> {error, Reason};             
             Val -> Val
         end,
   ?DEBUG("~p Config Read BrandI-CommunityId Map ~p~n", [?CURRENT_FUNCTION_NAME(), Ids]),
   Val1 = case Ids of
        {error, Reason1} -> {error, Reason1};
        _Val2 -> find_value(CommunityId, Ids)
   end,
   MId = binary_to_number(MemberId),
   Val3 = case Val1 of
        {ok, Val2} -> {ok, {brandId, Val2}, {memberId, MId}};
        {error, _Reason} -> {ok, {brandId, not_found}, {memberId, not_found}};
        Else -> {error, Else}
   end,
   ?DEBUG("~p Got BrandId ~p~n", [?CURRENT_FUNCTION_NAME(), Val3]),
   Val3;
get_brandId_from_communityId({error, Reason}, _) -> 
  {error, {brandId, not_found}, {memberId, not_found}, Reason}; 
get_brandId_from_communityId(_,_) -> 
  {error, {brandId, not_found}, {memberId, not_found}, mal_formed}.

%% @private
%% @doc get community / brand id mapping from configure file
%% @end


find_value(Key, List) ->
    Key1 =binary_to_number(Key),
    ?DEBUG("~p Key ~p Integer Key ~p List ~p~n", [?CURRENT_FUNCTION_NAME(), Key, Key1, List]),    
    Ret = case lists:keyfind(Key1, 2, List) of
        {_Type, _Key, Result} -> {ok, Result};
        {Key, Result} -> {ok, Result};
        false -> {error, nothing};
        {error, Reason} -> {error, Reason}
    end,
    ?DEBUG("~p Found Result ~p~n", [?CURRENT_FUNCTION_NAME(), Ret]),
    Ret.

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
        [?assertEqual([{ok, {brandId,}, {memberId, }}],get_loginData("12345-3")),
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
        [?assertEqual([{ok, {brandId,}, {memberd, }}],get_brandId_from_communityId("12345-3")),
         ?assertEqual({error, not_exist}, get_brandId_from_communityId("12345-99")),
         ?assertEqual({error, not_exist}, get_brandId_from_communityId("12345-99")),
         ?assertEqual({error, not_exist}, get_brandId_from_communityId("12345-99")),
         ?assertEqual({error, not_exist}, get_brandId_from_communityId("12345-99")),
        ].

-endif.
