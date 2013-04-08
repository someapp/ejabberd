%%%----------------------------------------------------------------------
%%%
%%% @author : Edward Tsang <etsang@spark.net>
%%% @doc Authentication client against spark authentication server
%%% Created : 20 Mar 2013
%%%---------------------------------------------------------------------
%%%
%%% Copyright (c)
%%%----------------------------------------------------------------------
%%% @end

%% @doc ejabberd_auth_spark exposes the public api to hook up wih ejabberd
%%       internally it is using a rest client to authentication
%% @end
-module(ejabberd_auth_spark).
-author('etsang@spark.net').

%% External exports

-export([start/1,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_password/2,
	 get_password_s/2,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 store_type/0,
	 plain_password_required/0
	]).
%TODO change it to true at end of cycle
-define(TEST, true).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("web/ejabberd_http.hrl").
-include("web/ejabberd_web_admin.hrl").

-type method() :: head | get | put | post | trace | options | delete.
-type url() :: binary().
-type reason() :: term().


%%-record(profile, {identity, server, lang, jid}).
%%TODO the following is janky becasue we are not finding ejabbberd_logger_h.beam just do this before fixing the build script
-define(MYDEBUG(Format,Args),io:format("D(~p:~p:~p) : "++Format++"~n",
				       [calendar:local_time(),?MODULE,?LINE]++Args)).

%%TODO READ THIS FROM config

-define(SPARKAUTH_URI, "http://api....").
-define(CURRENT_FUNCTION_NAME(), element(2, element(2, process_info(self(), current_function)))).
-define(AUTHENTICATED, 200).
-define(DefaultType, json).


%%====================================================================
%% API
%%====================================================================
-spec start(Host::string()) -> ok | {error, not_started}.
%% @doc Perform any initialization needed for the module to run
%% @end
start(Host) ->
    ?DEBUG("~p with host: ~p~n", [?CURRENT_FUNCTION_NAME(), Host]),
    application:start(inets),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl).

%% @doc Set user and password onto server. This is not needed; will be done on mainsite
%% @end
-spec set_password(User::string(), Server::string(), Password::string()) -> {error, not_allowed}.
set_password(User, Server, Password) ->
    
    ?ERROR_MSG("~p with user ~p server ~p password ~p~n", [?CURRENT_FUNCTION_NAME(),User, Server, get_password_string(Password)]),
    RETVAL = {error, not_allowed},
    ?ERROR_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Check if the user and password can login in server.
%% @end
-spec check_password(User::string(), Server::string(), Password::string(),Digest::string(), DigestGen::function()) ->
     false.
check_password(User, Server, Password, _Digest, _DigestGen) ->
    ?INFO_MSG("~p with user ~p server ~p password ~p digest ~p digestgen ~p~n", 
	   [?CURRENT_FUNCTION_NAME(), User, Server, get_password_string(Password), _Digest, _DigestGen]),
    RETVAL = check_password(User, Server, Password),
    ?INFO_MSG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Check if the user and password can login in server.
%% @end
-spec check_password(User::string(), Host::string(), Password::string()) -> false .
check_password(User, Host, "") ->
    ?ERROR_MSG("Password is missing ~p with user ~p host ~p password ~p~n", 
	    [?CURRENT_FUNCTION_NAME(), User, Host, ""]),
    false;
check_password(User, Host, Password) when is_list(Password) ->
    ?DEBUG("~p with user ~p host ~p password ~p~n", [?CURRENT_FUNCTION_NAME(), User, Host, get_password_string(Password)]),
    LUser = jlib:nodeprep(User),
    LHost = jlib:nameprep(Host),
    ?INFO_MSG("~p Authenicating user: ~p host ~p~n",[?CURRENT_FUNCTION_NAME(), LUser, LHost]),
    %UserHost = {LUser, LHost},  
    RETVAL =  case authenticate_request(LHost, LUser, Password) of
                   {ok, authenticated} -> true;
                   {ok, Reason} ->  
                              ?INFO_MSG("Authenication failed ~p ~p~n",[?CURRENT_FUNCTION_NAME(), Reason]),false;
                   {error, REASON} -> 
                                    ?INFO_MSG("Authenication failed ~p ~p~n",[?CURRENT_FUNCTION_NAME(), {error, REASON}]),
                                    false;
                   Error -> 
    			            ?ERROR_MSG("Authenication failed ~p ~p~n",[?CURRENT_FUNCTION_NAME(), {error, Error}]),
                                    false 
              end,
    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL;
check_password(User, Host, Password) ->
    ?ERROR_MSG("Parameters missing ~p with user ~p host ~p password ~p~n", 
	    [?CURRENT_FUNCTION_NAME(), User, Host, get_password_string(Password)]),
    false.

%% @doc Try register new user. This is not needed as this will go through website/mobile site
%% @end
-spec try_register(_User::string(), _Server::string(), _Password::string()) -> {error, not_allowed}.
try_register(_User, _Server, _Password) ->
    ?DEBUG("~p with user ~p server ~p password ~p~n", [?CURRENT_FUNCTION_NAME(), _User, _Server, get_password_string(_Password)]),
    RETVAL = {error, not_allowed},
    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Registered users list do not include anonymous users logged. This functionality we don't care.
%% @end
-spec dirty_get_registered_users() -> [].
dirty_get_registered_users() ->
    ?DEBUG("~p~n", [?CURRENT_FUNCTION_NAME()]),
    RETVAL = [],
    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Registered users list do not include anonymous users logged. This function is not allowed, we just don't care.
%% @end
-spec get_vh_registered_users(_Host::string())-> false.
get_vh_registered_users(_Host) ->
    ?DEBUG("~p with host ~p~n", [?CURRENT_FUNCTION_NAME(), _Host]),
    RETVAL = false,
    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Get the password of the user. This function is not allowed, this case taken by mainsite.
%% @end
-spec get_password(_User::string(), _Server::string()) -> false | string().
get_password(_User, _Server) ->
    ?DEBUG("~p with user ~p server ~p~n", [?CURRENT_FUNCTION_NAME(), _User, _Server]),
    RETVAL = false,
    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Get the password of the user. This function is not allowed, this case taken by mainsite.
%% @end
-spec get_password_s(_User::string(), _Server::string()) -> {error, not_allowed}. 
get_password_s(_User, _Server) ->
    ?DEBUG("~p with user ~p server ~p~n", [?CURRENT_FUNCTION_NAME(), _User, _Server]),
    RETVAL = {error, not_allowed},
    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.
   
%% @doc check if user exists 
%% This function checks user existence on database or on other authentication module.
%% We hit our authentication api to check for user existence
%% @end
-spec is_user_exists(_User::string(), _Host::string()) ->true | false | {error, atom()}.
is_user_exists(User, Host) ->
    ?DEBUG("~p with user ~p host ~p~~n", [?CURRENT_FUNCTION_NAME(), User, Host]),
    LUser = jlib:nodeprep(User),
    LHost = jlib:nameprep(Host),
    %UserHost = {LUser, LHost},
    V = get_global_call_parameters(Host),
    {{serviceEndpoint, BaseServiceEndpoint}, {appId, AppId}, {client_secret, ClientSecret}} = V, 
    ResourceEndpoint = ejabberd_auth_spark_config:get_isUserExists_service_endpoint(LHost),
    ?DEBUG("~p Config read with resource_endpoint ResourceEndpoint ~p V ~p~n", [?CURRENT_FUNCTION_NAME(), ResourceEndpoint, V]),
    Val = case spark_parse_loginData:get_loginData(LUser, LHost) of
         {ok, {brandId, BrandId}, {memberId, MemberId}} -> {ok, {brandId, BrandId}, {memberId, MemberId}};  
         {error, _, _, Reason} -> {error, Reason};
         {error, Reason1} -> {error, Reason1};
         _ ->   {error, not_found}                         
    end,
    {Url, Verb1} = case  ResourceEndpoint of 
         {error, _Reason} -> ?ERROR_MSG("Error in calling is user exists Error ~p~n", [?CURRENT_FUNCTION_NAME(), _Reason]), 
			    {error, _Reason};   
         {EndPoint, Verb} -> {EndPoint, Verb};        
	 _ -> {error, not_found}
    end,
   
    Response =  
    case Val of
      {ok, {brandId, BrandId1}, {memberId, MemberId1}} ->
                                 ResourceEndpoint1 = re:replace(Url, "{brandId}", convert_integer_tolist(BrandId1), [global, {return, list}]),
   				 ResourceEndpoint2 = re:replace(ResourceEndpoint1, "{applicationId}", convert_integer_tolist(AppId), [global, {return, list}]),
    				 ResourceEndpoint3 = re:replace(ResourceEndpoint2, "{memberId}", convert_integer_tolist(MemberId1), [global, {return, list}]),
    				_Url = restc:construct_url(BaseServiceEndpoint, ResourceEndpoint3,[{"client_secret", ClientSecret}]),
    				case {Url, Verb1} of
         		             {error, _Reason1} -> {error, _Reason1};
                                     {Url, [Verb2]} ->  post_isUserExists_request(Verb2, json, _Url, [200], []);
         		             _ -> {error, not_found}
    				end;
      {error, _Reason1}  -> {error, _Reason1};
      Else -> {error, Else}
    end,
    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Response]),
    T = case Response of 
	{ok, subscriber} -> true;
        {ok, _ } -> false;
        {error, Reason2} -> ?ERROR_MSG("? Error in checking user password Error ~p~n", [?CURRENT_FUNCTION_NAME(), Reason2]),
		false;
        Else2 -> ?ERROR_MSG("? Error in checking user password Error ~p~n", [?CURRENT_FUNCTION_NAME(), Else2]), 
		false
    end,

    ?DEBUG("~p Return with status ~p~n", [?CURRENT_FUNCTION_NAME(), T]),
    T.     

%% @doc Remove user.This function is not allowed, this case taken by mainsite.
%% @end
-spec remove_user(_User::string(), _Server::string())-> {error, not_allowed}.
remove_user(_User, _Server) ->
    ?DEBUG("~p with user ~p server ~p~n", [?CURRENT_FUNCTION_NAME(), _User, _Server]),
    RETVAL = {error, not_allowed},
    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Try to remove user if the provided password is correct. This function is not allowed.
%% User removal be taken care by main site. 
%% @end
-spec remove_user(_User::string(), _Server::string(), _Password::string()) -> not_allowed.
remove_user(_User, _Server, _Password) ->
    ?DEBUG("~p with user ~p server ~p password ~p~n", [?CURRENT_FUNCTION_NAME(), _User, _Server, _Password]),
    RETVAL = not_allowed,
    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc This is only executed by ejabberd_c2s for non-SASL auth client
%% @end
-spec plain_password_required()-> true.
plain_password_required() ->
    ?DEBUG("~p~n", [?CURRENT_FUNCTION_NAME()]),
    RETVAL = false,
    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.   
   
%% @doc Flag to indicate if using external storage to cache credentials
%% TODO: I do not think we store any password right in external storage? 
%% @end
-spec store_type()-> scram.
store_type() ->
    ?DEBUG("~p store type ~p~n", [?CURRENT_FUNCTION_NAME(),stored_type]),
    RETVAL = scram,
    ?DEBUG("~p with status ~p", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.  

%%====================================================================
%% Internal functions
%%====================================================================
%% @private
%% @doc check for the authentication http post response for Success is true and error term is null
%%      anything else is error and considered authentication error and failed.
%% @end
-spec check_auth_response(AuthStatus::[tuple()]) -> {ok, authenticated} | {error, term()} | term().
check_auth_response(Body) ->
    V1 = case proplists:get_value(<<"data">>,Body) of 
		undefined -> {error, missing_body};
	        List -> List
	end,
    V = case  proplists:get_value(<<"subscriptionStatus">>,V1) of
    	     <<"Member">>-> {ok, subscriber};
	     {error, Reason} -> {error, Reason};
             Else -> {ok, non_subscriber}
       end, 
    V.

check_isUser_response(Body)->
    ?DEBUG("~p Check isUser Response Body ~p~n", [?CURRENT_FUNCTION_NAME(),Body]),

    V1 = case proplists:get_value(<<"data">>,Body) of 
		undefined -> {error, missing_body};
		List -> List
	end,
    M = case proplists:get_value(<<"subscriptionStatus">>, V1) of
	  <<"Member">>-> {ok, subscriber};
	  {error, Reason} -> {error, Reason};
	  Else -> {ok, non_subscriber}
       end,
    ?DEBUG("~p Check subscription status ~p~n", [?CURRENT_FUNCTION_NAME(),M]), 
    M.


convert_integer_tolist(IntegerThing) when is_integer(IntegerThing) ->
   integer_to_list(IntegerThing);
convert_integer_tolist(IntegerThing) ->
   IntegerThing.

%% @private
%% @doc Get Parameters to make restful call
%% @end
get_global_call_parameters(Host)->
    BaseServiceEndpoint = ejabberd_auth_spark_config:get_spark_authservice_endpoint(Host),
    AppId = ejabberd_auth_spark_config:get_spark_application_id(Host),
    ClientSecret = ejabberd_auth_spark_config:get_spark_client_secrete(Host),
    {{serviceEndpoint, BaseServiceEndpoint}, {appId, AppId}, {client_secret, ClientSecret}}.

%% @private
%% @doc authenticate against api server from ejabberd Jid
%% @end
-spec authenticate_request(Host::string(), Email::string(), Password::string()) -> {ok, authenticated} | {error, term()} | term().
authenticate_request(Host, User, Password) ->    
    V = get_global_call_parameters(Host),
    {{serviceEndpoint, BaseServiceEndpoint}, {appId, _AppId}, {client_secret, _ClientSecret}} = V, 
    ResourceEndpoint = ejabberd_auth_spark_config:get_miniProfile_service_endpoint(Host),
    ?DEBUG("~p Config read with resource_endpoint ResourceEndpoint ~p V ~p~n", [?CURRENT_FUNCTION_NAME(), ResourceEndpoint, V]),
    Val = case spark_parse_loginData:get_loginData(User, Host) of
	 {ok, {brandId, BrandId}, {memberId, MemberId}} -> {ok, {brandId, BrandId}, {memberId, MemberId}};
         {error, {brandId, _}, {memberId, _}, Reason} -> {error, Reason};
         {error, _, _, Reason1} -> {error, Reason1};
         {error, Reason2} -> {error, Reason2};
         _ -> {error, not_found}                           
    end,
    ?DEBUG("~p Login Data read with brandId & memeberId ~p~n", 
[?CURRENT_FUNCTION_NAME(), Val]),
    {Url, Verb1}  = case  ResourceEndpoint of 
         {error, _Reason} -> ?ERROR_MSG("Error in calling is user exists Error ~p~n", [?CURRENT_FUNCTION_NAME(), _Reason]), 
			    {error, _Reason}; 	 
	 {EndPoint, Verb} -> {EndPoint, Verb};          
	 _ -> {error, not_found}
    end,
    ?DEBUG("~p Config read Resource Url and Http Verb ~p~n", 
[?CURRENT_FUNCTION_NAME(), {Url, Verb1}]),
    Response =  
    case Val of
      {ok, {brandId, BrandId1}, {memberId, MemberId1}} ->
                                 ResourceEndpoint1 = re:replace(Url, "{brandId}", convert_integer_tolist(BrandId1), [global, {return, list}]),
   				 ResourceEndpoint2 = re:replace(ResourceEndpoint1, "{targetMemberId}", convert_integer_tolist(MemberId1), [global, {return, list}]),
    				 ResourceEndpoint3 = re:replace(ResourceEndpoint2, "{memberId}", convert_integer_tolist(MemberId1), [global, {return, list}]),
	                        ?DEBUG("~p construct rest call ~p ~p password ~p~n", [?CURRENT_FUNCTION_NAME(), BaseServiceEndpoint, ResourceEndpoint3, Password]),
    				_Url = restc:construct_url(BaseServiceEndpoint, ResourceEndpoint3,[{"access_token", Password}]),
                                ?DEBUG("~p Initiate rest call ~p~n", [?CURRENT_FUNCTION_NAME(), _Url]),
    				case {Url, Verb1} of
         		             {error, _Reason1} -> {error, _Reason1};
                                     {Url, [Verb2]} ->  post_authenticate_request(Verb2, json, _Url, [200], []);
         		             _ ->  {error, not_found}
    				end;
      {error, _Reason1}  -> {error, _Reason1};
      Else -> {error, Else}
    end,

    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), Response]),
    Response.


%% @private
%% @doc post the Authentication Http Post request to the api server. Return {ok, authentication}
%% if the http code returns 200 and the Response body constains no error message and the Success Status is true 
%% @end
%% https://api.spark.net/brandId/1003/application/1000/member/133272351/status?client_secret=SXO0NoMjOqPDvPNGmEwZsHxnT5oyXTmYKpBXCx3SJTE1
-spec post_authenticate_request(Method::atom(), Type::atom(), Url::string(), Expect::[atom()], Headers::[term()]) -> {ok, authenticated} | {error, term()} | term().
post_authenticate_request(Method, Type, Url, Expect, Headers) ->
    ?DEBUG("~p Method ~p Type ~p Url ~p Expect ~p Headers ~p~n", [?CURRENT_FUNCTION_NAME(), Method, Type, Url, Expect, Headers]),
    RetValue = 
       case restc:request(Method, Type, Url, Expect, Headers) of
	    {ok, Status, H, B} -> {ok, Status, H, B};
            {error, Status, _H, _B} -> {error, Status, _H, _B}; 
            Status -> Status
       end,

    ?DEBUG("?p RestCall returned with response ~p~n", 
	    [?CURRENT_FUNCTION_NAME(), RetValue]),    
   
    Status1 = case RetValue of
	    {ok, ?AUTHENTICATED, _, ResponseBody} -> check_auth_response(ResponseBody);
            {error, Status,_, _} -> {error, Status};
            ResponseBody -> ?INFO_MSG("?p RestCall response malformed with status ~p~n",
            [?CURRENT_FUNCTION_NAME(), ResponseBody]), 
		     {error, bad_request}
    end,
    case Status1 of 
         {ok, ?AUTHENTICATED, _ServerInfo, ResponseBody1} -> check_auth_response(ResponseBody1);
         {error, Reason} -> {error, Reason};
         Else -> Else
    end.

post_isUserExists_request(Method, Type, Url, Expect, Headers) ->
    ?DEBUG("~p Method ~p Type ~p Url ~p Expect ~p Headers ~p~n", [?CURRENT_FUNCTION_NAME(), Method, Type, Url, Expect, Headers]),
    V = restc:request(Method, Type, Url, Expect, Headers),
    ?DEBUG("~p Restc Request Response ~p~n", [?CURRENT_FUNCTION_NAME(), V]),
    RetVal = 
       case V of
	    {ok, Status, H, B} ->
			        ?DEBUG("~p Check is_usr Request Response ~p~n", [?CURRENT_FUNCTION_NAME(), B]),
				check_isUser_response(B);
			        
            {error, Status, _H, _B} ->     
                                  ?ERROR_MSG("RestCall failed with status ~p ~p~n", [?CURRENT_FUNCTION_NAME(), Status]), 
                                  {error, Status, _H, _B}; 
            Status -> Status
       end,
    ?DEBUG("~p Request Response ~p~n", [?CURRENT_FUNCTION_NAME(), RetVal]), 
    RetVal.
  
  
%% @private
%% @doc get access expiration time
%% @end
%%-spec get_access_expiration()->
   

%% @private
%% @doc get password to be printed to log as ******
%% @end
-spec get_password_string(_Password::string())-> string().
get_password_string("") -> 
    ""; 

get_password_string(_Password) when is_list(_Password) ->
   get_password_string(_Password, true).

get_password_string(_Password, true) when is_list(_Password)->
    "******";
get_password_string(_Password, false) when is_list(_Password)->
    _Password;
get_password_string(_Password, IsDebug) when is_list(_Password) ->
    get_password_string(_Password, false).

string_to_integer(StringValue)->
    case string:to_integer(StringValue) of
    	{Int, _} -> Int;
        Else -> Else
    end.

%%%%%% EUNIT %%%%%%%%%%%%%%%%%%
-ifdef(TEST).

%%get_spark_authservice_endpoint_test()->[?assertEqual({error, endpoint_notfound}, get_spark_authservice_endpoint("BadHost")),
%%					?assertEqual("https://api.spark.test", get_spark_authservice_endpoint("GoodHost"))].

get_password_string_test()-> [?assertEqual("******", get_password_string("Password")),
		       ?assertEqual("******", get_password_string(anyValue))].

store_type_No_Password_Stored_test()-> ?assertEqual(scram, store_type()).

plain_password_always_required_test()-> ?assertEqual(false, plain_password_required()).

dirty_get_registered_users_return_emptylist_test()-> ?assertEqual([], dirty_get_registered_users()).
						     

get_vh_registered_users_return_false_test() -> ?assertEqual(false, get_vh_registered_users("SomeHost")).

remove_user_not_allowed_test()-> [?assertEqual(not_allowed,remove_user("SomeUser","SomeHost","SomePassword")),
				 ?assertEqual(not_allowed,remove_user(anyValue,anyValue,anyValue)),
				 ?assertEqual({error, not_allowed}, remove_user("SomeUser","SomeHost")),
				 ?assertEqual({error, not_allowed}, remove_user(anyValue,anyValue))].

try_register_not_allowed_test()->[?assertEqual({error, not_allowed}, try_register("SomeUser", "SomeServer", "SomePassword")),
			       ?assertEqual({error, not_allowed}, try_register(anyValue,anyValue,anyValue))]. 

set_password_not_allowed_test()->[?assertEqual({error, not_allowed}, set_password("SomeUser", "SomeServer", "SomePassword")),
			       ?assertEqual({error, not_allowed}, set_password(anyValue,anyValue,anyValue))]. 



check_password_return_false_test()->[?assertEqual(false,check_password("SomeUser", "SomeServer", "SomePassword","SomeDigest","DigestGen")),
				     ?assertEqual(false,check_password(anyValue, anyValue, anyValue, anyValue, anyValue))].

check_password_return_test()->[?assertEqual(true,check_password("SomeUser", "SomeHost", "SomePassword")),
			       ?assertEqual(true,check_password(anyValue, anyValue, anyValue))].


get_password_return_false_test() -> [?assertEqual(false, get_password("SomeUser", "SomeServer")),
				     ?assertEqual(false, get_password(anyValue, anyValue)),
				     ?assertEqual({error, not_allowed}, get_password_s("SomeUser", "SomeServer")),
				     ?assertEqual({error, not_allowed}, get_password_s(anyValue, anyValue))].

is_user_exists_test_return_true()-> [?assertEqual(true, is_user_exists("SomeUser", "SomeHost")),
				     ?assertEqual(true, is_user_exists(anyValue, anyValue))].

-endif.

