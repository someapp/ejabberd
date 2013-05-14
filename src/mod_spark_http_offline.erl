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


%TODO change it to false at end of cycle
-define(TEST, true).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-define(CURRENT_FUNCTION_NAME(), element(2, element(2, process_info(self(), current_function)))).
-define(AUTHENTICATED, 200).
-define(DefaultType, json)


create_message(From, To, Packet) ->
	case parse_message(From, To, Packet) ->
	     ignore -> ok;
	     {error, Error} -> ?ERROR_MSG("Parse Message Failed ~p ~p~n",[?CURRENT_FUNCTION_NAME(), {error, Error}]),
				ok;

	     Message ->
		     post_offline_message(message_hook, From#jid.server, Message), 
		     ok
	end.

%%====================================================================
%% Internal functions
%%====================================================================
post_offline_message(Event, Server, Message) ->
    Proc = gen_mod:get_module_proc(Server, ?MODULE),
    gen_server:call(Proc, {post_to_restapi, Event, Message}).
post_offline_message(Event, User, Server, Resource, Message) ->
    Proc = gen_mod:get_module_proc(Server, ?MODULE),
    gen_server:call(Proc, {post_to_restapi, Event, User, Server, Resource, Message}).


parse_message(From, To, {xmlelement, "message", _, _} = Packet) ->
    %IsJustOffLine = checkTargetMemberStatus(),
    Type    = xml:get_tag_attr_s("type", Packet),
    case Type of
	"chat" ->   Subject = get_tag_from("subject", Packet),
    		    Body    = get_tag_from("body", Packet),
		    Thread  = get_tag_from("thread", Packet),
		    #message{from = jlib:jid_to_string(From), to = jlib:jid_to_string(To), type = Type, subject = Subject, body = Body, thread = Thread};
	_ -> ignore 
   end;
parse_message(_From, _To, _) -> ignore.


post_to_restapi_message(SenderId, RecipientId, AccessToken, Body, State) ->
		?INFO_MSG("Posting From ~p To ~p Body ~p~n",[SenderId, RecipientId, Body]),
						
		Response = case post_to_restapi(BrandId, AccessToken, RecipientId, Messages, State) of
			{ok, _} -> ok;
			{error, Reason} -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Reason]),
					    ok;
			Else -> ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Else]),
				ok;
		end,	
		Ret = case check_sendmissedIM_response(Response) of
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


post_to_restapi(BrandId, AccessToken, RecipientId, Messages, State) ->
	BaseServiceUrl = mod_spark_http_offline_config:getSparkApiEndpoint(State),
	SendMissedIMUrl = mod_spark_http_offline_configgetSendMissedIMUrl(State),
%%%% Somehow get the AccessToken and RecipientId %%%%
	Access_Token = AccessToken,
	RecipientId = RecipientId,
        ResourceEndpoint1 = re:replace(SendMissedIMUrl, "{brandId}", BrandId, [global, {return, list}]),
	Url = restc:construct_url(BaseServiceEndpoint, ResourceEndpoint1,
					[{"access_token", AccessToken},
					 {"RecipientMemberId",RecipientId },
					 {"Messages", Messages}]),

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
handle_call({post_to_restapi, message_hook, Message}, _From, State) ->
    Data = "from="     ++ ejabberd_http:url_encode(Message#message.from) ++
           "&to="      ++ ejabberd_http:url_encode(Message#message.to) ++
           "&type="    ++ ejabberd_http:url_encode(Message#message.type) ++ 
           "&subject=" ++ ejabberd_http:url_encode(Message#message.subject) ++
           "&body="    ++ ejabberd_http:url_encode(Message#message.body) ++
           "&thread="  ++ ejabberd_http:url_encode(Message#message.thread),
    send_data(message_hook, Data, State),
    {reply, ok, State};
handle_call({post_to_restapi, Event, User, Server, Resource, Message}, _From, State) ->
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
