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

-module(mod_spark_http_offline).

-author("etsang").

%% Every ejabberd module implements the gen_mod behavior
%% The gen_mod behavior requires two functions: start/2 and stop/1
-behavirou(gen_server).
-behaviour(gen_mod).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

%% ejabbberd gen_mod callbacks
-export([
	start/2, 
	stop/1
	]). 

%% Module 
-export([
	create_message/3
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(state, 
	{host, 
	 urls,
	 client_settings,
	 community2brandId,
	 sanity_test_setting
	}).

-record(message, {from, to, type, subject, body, thread}).


%TODO change it to true at end of cycle
-define(TEST, true).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-define(CURRENT_FUNCTION_NAME(), element(2, element(2, process_info(self(), current_function)))).
-define(AUTHENTICATED, 200).
-define(DefaultType, json)


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

%%%%% Rest and RabbitMQClient %%%%%

getSparksOauthAccessToken(State)->
   AccessToken=getVal_for(spark_oauth_access_token, #State.urls),
   AccessToken.

getSparkApiEndpoint(State)->
   ApiUrl = getVal_for(spark_api_endpoint, #State.urls),
   ApiUrl.

getSendMissedIMUrl(State)->
   SendMissedIMUrl = getVal_for(send_missed_im, #State.urls),
   SendMissedIMUrl.

getRabbitMQEndpoint(State)->
   RabbitMQEndpoint = getVal_for(rabbitmq_endpoint, #State.urls),
   RabbitMQEndpoint.

getProfileMemberStatus(State)->
   ProfileMemberStatus = getVal_for(profile_memberstatus, #State.urls),
   ProfileMemberStatus.

getBrandId(SenderId, State) ->
   CommunityIdBrandIdMap = getVal_for(community2brandId, #State.community2brandId),
   Val = case re:split(UserName,"-") of 
 		     		  [CommunityId, MemberId] -> [CommunityId, MemberId];
                     		  {error, Reason} -> {error, Reason};
                     	          Else -> {error, Else}
  			     end,
   
   BrandIdTuple = case Val of
	{error _} -> {error, undefined};
	[CommunityId, MemberId] -> case lists:keyfind(Key, 2, List) of
        				{Key, Result} -> Result;
        			    	false -> {error, nothing}
    				   end;
   end,
   extract_brandId(BrandIdTuple).

getTargetMemberId(RecipientId, State) ->

   MemberId.

%%%%% Rest and RabbitMQClient %%%%%
getRestClientTimeout(State)->
   RestClientTimeout = getVal_for(rest_client_timeout_in_sec, #State.client_settings),
   RestClientTimeout.

getRestRetryAttempt(State)->
   RestClientTimeout = getVal_for(rest_call_retry_attempt, #State.client_settings),
   RestClientTimeout.

getRabbitMQClientTimeout(State)->
   RabbitMQClientTimeout = getVal_for(rabbitmq_client_timeout_in_sec, #State.client_settings),
   RabbitMQClientTimeout.

getRabbitMQClientRetryAttempt(State)->
   RabbitMQClientTimeout = getVal_for(rabbitmq_call_retry_attempt, #State.client_settings),
   RabbitMQClientRetryAttempt.


%%%%% Sanity Test Settings %%%%%
getTestBrandId(State)->
   TestBrandId = getVal_for(testBrandId, #State.sanity_test_setting), 
   TestBrandId.

getTestAppId(State)->
   TestAppId = getVal_for(testBrandId, #State.sanity_test_setting),
   TestAppId.

getTestClientSecret(State)->
   TestClientSecret = getVal_for(testBrandId, #State.sanity_test_setting),
   TestClientSecret.

getTestSourceMemberId(State)->
   TestSourceMemberId = getVal_for(testBrandId, #State.sanity_test_setting),
   TestSourceMemberId.

getTestSourceMemberEmail(State)->
   TestSourceMemberEmail = getVal_for(testBrandId, #State.sanity_test_setting),
   TestSourceMemberEmail.

getTestTargetMemberId(State)->
   TestTargetMemberId = getVal_for(testBrandId, #State.sanity_test_setting),
   TestTargetMemberId.

getTestTargetMemberEmail(State)->
   TestTargetMemberEmail = getVal_for(testBrandId, #State.sanity_test_setting),
   TestTargetMemberEmail.

getTestTargetMemberPassword(State)->
   TestTargetMemberPassword = getVal_for(testBrandId, #State.sanity_test_setting),
   TestTargetMemberPassword.


%%%%%%%%%%%%%%Utility Function %%%%%%%%%%%%%%%%
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

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, _Opts]) ->
    ?INFO_MSG("starting mod_spark_http_offline", []),
    inets:start(),
    ejabberd_hooks:add(offline_message_hook, _Host, ?MODULE, create_message, 50). 
    Urls         = gen_mod:get_module_opt(global, ?MODULE, url, []),
    ClientSetting = gen_mod:get_module_opt(global, ?MODULE, client_settings, undefined),
    Community2BrandId = gen_mod:get_module_opt(global, ?MODULE, community2brandId, undefined),
    SanityTestSetting = gen_mod:get_module_opt(global, ?MODULE, sanity_test_setting, undefined),
    ?INFO_MSG("started mod_spark_http_offline", []),
    {ok, #state{
	 	host = Host, 
		urls = Urls,
		client_setting = ClientSetting,
		community2brandId  = Community2BrandId,
		sanity_test_setting = SanityTestSetting
    }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({post_results, message_hook, Message}, _From, State) ->
    Data = "from="     ++ ejabberd_http:url_encode(Message#message.from) ++
           "&to="      ++ ejabberd_http:url_encode(Message#message.to) ++
           "&type="    ++ ejabberd_http:url_encode(Message#message.type) ++ 
           "&subject=" ++ ejabberd_http:url_encode(Message#message.subject) ++
           "&body="    ++ ejabberd_http:url_encode(Message#message.body) ++
           "&thread="  ++ ejabberd_http:url_encode(Message#message.thread),
    send_data(message_hook, Data, State),
    {reply, ok, State};
handle_call({post_results, Event, User, Server, Resource, Message}, _From, State) ->
    Data = "user="      ++ ejabberd_http:url_encode(User) ++
           "&server="   ++ ejabberd_http:url_encode(Server) ++
           "&resource=" ++ ejabberd_http:url_encode(Resource) ++ 
           "&message="  ++ ejabberd_http:url_encode(Message),
    send_data(Event, Data, State),
    {reply, ok, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.
    
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({http, {RequestId, stream_start, Headers}}, State) ->
    ?DEBUG("http stream_start RequestId: ~p, Headers: ~p",[RequestId, Headers]),
    {noreply, State};
handle_info({http, {RequestId, stream, BinBodyPart}}, State) ->
    ?DEBUG("http stream RequestId: ~p, BinBodyPart: ~p",[RequestId, BinBodyPart]),
    {noreply, State};
handle_info({http, {RequestId, stream_end, Headers}}, State) ->
    ?DEBUG("http stream_end RequestId: ~p, Headers: ~p",[RequestId, Headers]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ?INFO_MSG("stopping mod_spark_http_offline", []),
    Host = State#state.host,
    ejabberd_hooks:delete(offline_message_hook, _Host, ?MODULE, create_message, 50).
    ?INFO_MSG("stopped mod_spark_http_offline", []),
    ok.
    
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).


%%====================================================================
%% gen_mod callbacks
%%====================================================================


start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec =	{
        Proc,
	    {?MODULE, start_link, [Host, Opts]},
	    transient,
	    1000,
	    worker,
	    [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).
    
stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc).


%%%%%% EUNIT %%%%%%%%%%%%%%%%%%
-ifdef(TEST).

-endif.
