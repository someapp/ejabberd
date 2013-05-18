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

-behavirou(gen_server).
-behaviour(gen_mod).

%% gen_server callbacks
-export([
    init/1, 
    start_link/2,
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

%% Module export 
-export([
	create_message/3
	]).

-include("../include/ejabberd.hrl").
-include("../include/jlib.hrl").

-include_lib("../include/mod_spark_http_offline.hrl").


-record(message, 
	{from, 
	 to, 
	 type, 
	 subject, 
	 body, 
	 thread,
	 attempt}).

-type message()::#message{}.

-define(PROCNAME, ?MODULE).

%TODO change it to false at end of cycle
-define(TEST, true).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-spec create_message(string(), string(), binary()) -> ok | {error, reason()}.
create_message(From, To, Packet)->
	case parse_message(From, To, Packet) of 
	     ignore -> ok;
	     {error, Error} -> ?ERROR_MSG("Parse Message Failed ~p ~p~n",[?CURRENT_FUNCTION_NAME(), {error, Error}]),
				ok;
	     Message ->
             		post_offline_message(offline_message, From#jid.server, Message#message{attempt = 0}), 
		     	ok
	end.

%%====================================================================
%% Internal functions
%%====================================================================
post_offline_message(Event, Server, Message) ->
    %%Proc = gen_mod:get_module_proc(Server, ?PROCNAME),  
    ServerMsg = {post_to_restapi, Event, Message},
    ?INFO_MSG("Posting to Procname: ~p Server: ~p on Event: ~p with Message: ~p~n", [?PROCNAME, Server, Event, ServerMsg]),
    gen_server:call(?PROCNAME, {post_to_restapi, Event, Message}).

post_to_restapi(Event, Server, Message) ->
    %%Proc = gen_mod:get_module_proc(Server, ?PROCNAME),  
    ServerMsg = {post_to_restapi, Event, Message},
    ?INFO_MSG("Posting to ~p with Message ~p~n", [Server, ServerMsg]),
    gen_server:call(?PROCNAME, {post_to_restapi, Event, Message}).

parse_message(From, To, {xmlelement, "message", _, _} = Packet) ->
    %IsJustOffLine = checkTargetMemberStatus(),
    Type    = xml:get_tag_attr_s("type", Packet),
    case Type of
	"chat" ->   Subject = get_tag_from("subject", Packet),
    		    Body    = get_tag_from("body", Packet),
		    Thread  = get_tag_from("thread", Packet),
		    #message{from = jid_to_string(From), to = jid_to_string(To), type = Type, subject = Subject, body = Body, thread = Thread};
	_ -> ignore 
   end;

parse_message(_From, _To, _) -> 
   ignore.

jid_to_string(Jid) ->
   jlib:jid_to_string(Jid).

getSenderId(Message) ->
   Message#message.from.

getRecipientId(Message) ->
   Message#message.to.

getAccessToken(Message) ->
   SenderId = getSenderId(Message),
   %%%%%% TODO where to get the access token ?????
   AccessToken = message,
   AccessToken.

get_tag_from(Tag, Packet) ->
    case xml:get_subtag(Packet, Tag) of
        false -> 
            "";
        Xml   ->
            xml:get_tag_cdata(Xml)
    end.

isExceedRetryAttempt(Attempt, MaxRetry) when is_integer(Attempt)->
   case Attempt >= MaxRetry of
 	true -> true;
        false -> false
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
    ?INFO_MSG("Initializing mod_spark_http_offline ~n", []),
    app_helper:ensure_app_started(inets),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, create_message, 50), 

    Urls         = gen_mod:get_module_opt(global, ?MODULE, url, []),
    ClientSetting = gen_mod:get_module_opt(global, ?MODULE, client_settings, undefined),
    Community2BrandId = gen_mod:get_module_opt(global, ?MODULE, community2brandId, undefined),
    SanityTestSetting = gen_mod:get_module_opt(global, ?MODULE, sanity_test_setting, undefined),

    ?INFO_MSG("Started mod_spark_http_offline", []),
    State = {ok, #state{
	 	host = Host, 
		urls = Urls,
		client_settings = ClientSetting,
		community2brandId  = Community2BrandId,
		sanity_test_setting = SanityTestSetting
    }},
    ?INFO_MSG("Initialized mod_spark_http_offline ~n", []), 
    State.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------


handle_call({post_to_restapi, offline_message, Message}, _From, State) ->
    ?DEBUG("{post_to_restapi, offline_message, Message}, _From, State ~p~n",[State]),
    Timeout = mod_spark_http_offline_config:getRestClientTimeout(State),
    %% TODO is following okay?
    handle_call({post_to_restapi, message_hook, Message}, _From, State, Timeout);

handle_call({post_to_restapi,UnsupportedEvent , Message}, _From, State) ->
    ?WARNING_MSG("post_to_api Unsupported Event: ~p~n",[UnsupportedEvent]),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_call({post_to_restapi, offline_message, Message}, _From, State, Timeout) ->
    ?DEBUG("{post_to_restapi, offline_message, Message}, _From, State ~p Timeout ~p~n",[State, Timeout]),
    SenderId = getSenderId(Message),
    RecipientId = getRecipientId(Message),
    AccessToken = getAccessToken(Message),
    RestClientRetryAttempt = mod_spark_http_offline_config:getRestClientTimeout(State),
    Ret = case isExceedRetryAttempt(Message#message.attempt, RestClientRetryAttempt ) of
               false -> 
			Attempt1 = Message#message.attempt +1,
    			Message2 = Message#message{attempt=Attempt1},
                        mod_spark_http_offline_restclient:post_to_restapi_message(SenderId, RecipientId, AccessToken, Message2, State),
    		        {reply, ok, State, Timeout};
               true ->  ?INFO_MSG("post_to_api Message ~p is ~p~n",[Message,expired]),
		        {reply, ok, State, Timeout}
	  end,
    Ret;

handle_call({post_to_restapi, UnsupportedEvent, Message}, _From, State, Timeout) ->
    ?WARNING_MSG("post_to_api Unsupported Event: ~p~n",[UnsupportedEvent]),
    {reply, ok, State, Timeout}.
    
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
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, create_message, 50),
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
    ?INFO_MSG("start link mod_spark_http_offline", []),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).


%%====================================================================
%% gen_mod callbacks
%%====================================================================


start(Host, Opts) ->
    ?INFO_MSG("Starting mod_spark_http_offline at ~p~n", [?PROCNAME]),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =	{
        Proc,
	    {?MODULE, start_link, [Host, Opts]},
	    transient,
	    1000,
	    worker,
	    [?MODULE]},
    Ret = supervisor:start_child(ejabberd_sup, ChildSpec),
    ?INFO_MSG("Started mod_spark_http_offline ~p with status ~p~n", [Proc, Ret]),  
    Ret.
    
stop(Host) ->
    ?INFO_MSG("Stopping mod_spark_http_offline", []),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    Ret = supervisor:delete_child(ejabberd_sup, Proc),
    ?INFO_MSG("Started mod_spark_http_offline ~p with status ~p~n", [Proc, Ret]), 
    Ret.


%%%%%% EUNIT %%%%%%%%%%%%%%%%%%
-ifdef(TEST).


mod_spark_http_offline_test_() ->
    { setup,
      fun setup/0,
      fun cleanup/1,
      [

      ]
    }.

setup() ->   
  ok.

cleanup(_Pid) ->
  ok.
-endif.
