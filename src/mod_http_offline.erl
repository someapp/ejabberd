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

-module(mod_http_offline).

-author("etsang").

%% Every ejabberd module implements the gen_mod behavior
%% The gen_mod behavior requires two functions: start/2 and stop/1
-behaviour(gen_mod).

%% public methods for this module
-export([start/2, stop/1, create_message/3]).

%% included for writing to ejabberd log file
-include("ejabberd.hrl").
%% ejabberd functions for JID manipulation called jlib.
-include("jlib.hrl").


%TODO change it to true at end of cycle
-define(TEST, true).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-define(CURRENT_FUNCTION_NAME(), element(2, element(2, process_info(self(), current_function)))).
-define(AUTHENTICATED, 200).
-define(DefaultType, json)


start(_Host, _Opt) -> 
		?INFO_MSG("mod_http_offline loading", []),
		inets:start(),
		Config = load_config(),
		?INFO_MSG("HTTP client started", []),
		spark_msgarchive_restclient:start(),
		?INFO_MSG("Spark Rest Client started", []),
		%% a start up test to post to a test account
		post_offline_message("testFrom", "testTo", "testBody"),
		ejabberd_hooks:add(offline_message_hook, _Host, ?MODULE, create_message, 50).   


stop (_Host) -> 
		?INFO_MSG("stopping mod_http_offline", []),
		ejabberd_hooks:delete(offline_message_hook, _Host, ?MODULE, create_message, 50).



create_message(_From, _To, Packet) ->
		Type = xml:get_tag_attr_s("type", Packet),
		FromS = xml:get_tag_attr_s("from", Packet),
		ToS = xml:get_tag_attr_s("to", Packet),
		Body = xml:get_path_s(Packet, [{elem, "body"}, cdata]),

		AccessToken xml:get_path(Packet, ["body"], cdata]),

		case Type of
			"chat" -> post_offline_message(FromS, ToS, AccessToken, Body);
			{warn, Warn} -> ?WARN_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Reason]), 
					ok;
			{error, Reason} -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Reason]),
					    ok;
	 		Else -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Else]),
				ok;
		end.



post_offline_message(SenderId, RecipientId, AccessToken, Body) ->
		?INFO_MSG("Posting From ~p To ~p Body ~p~n",[SenderId, RecipientId, Body]),
            	Messages = lists:concat(["From=", SenderId,"&To=", RecipientId,"&Body=", Body]),		
		Response = case post_to_restapi(SenderId, RecipientId, Messages) of
			{ok, _} -> ok;
			{error, Reason} -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Reason]),
					    ok;
			Else -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Else]),
				ok;
		end,	
		Ret = case verifyResponse(Response) of
			{ok, _} -> ?INFO_MSG("post request sent", []),
				   ok;
			{warn, _Warning} - ?WARN_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), _Warning]),
					    ok;
			{error, _R} -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), _Reason]),
					    {error, _Reason};
			_Else -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), _Else]),
				 {error, _Else}
		     end,	
		Ret.


post_to_restapi(SenderId, RecipientId, Body) ->
	[BrandId, SourceMemberId] = getBrandIdSrcMemberId(SenderId), 
	TargetMemberId = getTargetMemberId(RecipientId), 
	SendMissedIMUrl = getSendMissedIMUrl(),
	Access_Token = getClientAccessToken(),
	Url = restc:construct(),
	Response = case restc:request(post, json, Url, [200],[],[""]) of 
			{ok, S} -> ?INFO_MSG("post request sent", []),
				   {ok, S};
			{warn, Warning} - ?WARN_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Warning]),
					  {warn, Warning};
			{error, Reason} -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Reason]),
					    {error, Reason};
			Else -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Else]),
				 {error, Else}
		   end,
	Response.

load_config()->
  {ok, Conf} = file:consult(io:tolist(?Module)++".cfg"),
  {ok, #state{}};
.

getClientAccessToken()->
  
.

getSendMissedIMUrl()->

.


getBrandIdSrcMemberId(SenderId) ->

.

getTargetMemberId(RecipientId) ->


.



%%%%%% EUNIT %%%%%%%%%%%%%%%%%%
-ifdef(TEST).

-endif.
