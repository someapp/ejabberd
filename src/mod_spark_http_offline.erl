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
-include("mod_spark_http_offline.hrl").


-record(message, 
	{from, 
	 to, 
	 type, 
	 subject, 
	 body, 
	 thread}).


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


getSenderId(Message) ->
   SenderId = #Message.from,
   SenderId.

getRecipientId(Message) ->
   RecipientId = #message.to,
   RecipientId.

getAccessToken(Message) ->
   SenderId = getSenderId(Message),
   
   AccessToken = #message.
   AccessToken.



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
    Timeout = mod_spark_http_offline_config:getRestClientTimeout(State),
    {reply, ok, State, Timeout};

handle_call({post_to_restapi, message_hook, Message}, _From, State, Timeout) ->
    SenderId = getSenderId(Message),
    RecipientId = getRecipientId(Message),
    AccessToken = getAccessToken(Message),

    post_to_restapi_message:post_to_restapi_message(SenderId, RecipientId, AccessToken, Body, State),
    {reply, ok, State, Timeout};

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
