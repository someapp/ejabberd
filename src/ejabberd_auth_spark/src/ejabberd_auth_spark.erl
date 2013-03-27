%%%----------------------------------------------------------------------
%%%
%%% @author : Edward Tsang <etsang@spark.net>
%%% @doc Authentication client against spark authentication server
%%% Created : 20 Mar 2013
%%%---------------------------------------------------------------------
%%%
%%% Copyright (c)
%%%
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
    
    RETVAL = {error, not_started}, 
    ?DEBUG("Spark authentication with status: ~p~n", [RETVAL]),    
    RETVAL.

%% @doc Set user and password onto server. This is not needed; will be done on mainsite
%% @end
-spec set_password(User::string(), Server::string(), Password::string()) -> {error, not_allowed}.
set_password(User, Server, Password) ->
    %% TODO security issue to log this, doit another way but also enough info for debugging
    ?DEBUG("~p with user ~p server ~p password ~p~n", [?CURRENT_FUNCTION_NAME(),User, Server, get_password_string(Password)]),
    RETVAL = {error, not_allowed},
    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Check if the user and password can login in server.
%% @end
-spec check_password(User::string(), Server::string(), Password::string(),Digest::string(), DigestGen::function()) ->
     false.
check_password(User, Server, Password, _Digest, _DigestGen) ->
    ?DEBUG("~p with user ~p server ~p password ~p digest ~p digestgen ~p~n", 
	   [?CURRENT_FUNCTION_NAME(), User, Server, get_password_string(Password), _Digest, _DigestGen]),
    RETVAL = check_password(User, Server, Password),
    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Check if the user and password can login in server.
%% @end
-spec check_password(User::string(), Host::string(), Password::string()) -> false .
check_password(User, Host, Password) ->
    ?DEBUG("~p with user ~p host ~p password ~p~n", [?CURRENT_FUNCTION_NAME(), User, Host, get_password_string(Password)]),
    RETVAL = 
         case authenticate_request(Host, User, Password)of
              {ok, authenticated} -> true;
              {error, Error} -> 
                        ?INFO("Authenication error ~p ~p~n",[?CURRENT_FUNCTION_NAME(), {error, Error}]),
                        false;
              Error -> ?ERROR("Authentication Call Error ~p ~p~n ", [?CURRENT_FUNCTION_NAME(), {error, Error}]),);
                        false; 
        end;
    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

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
%% This function checks user existence on database or on other authentication module
%% since we don't offer checking user existence on our internal api. We will always return
%% true - which is sad - for ejabberd to go on with its business
%% @end
-spec is_user_exists(_User::string(), _Host::string()) ->true. 
is_user_exists(_User, _Host) ->
    ?DEBUG("~p with user ~p host ~p~~n", [?CURRENT_FUNCTION_NAME(), _User, _Host]),
    RETVAL = {error, not_implemented},
    ?DEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.     

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
    ?DEBUG(" store type ~p~n", [stored_type]),
    RETVAL = scram,
    ?DEBUG("~p with status ~p", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.  

%%====================================================================
%% Internal functions
%%====================================================================
%% @private
%% @doc get the authentication endpoint rest client to talk to
%% @end
get_spark_auth_service_config(Host, TokenName) ->
    case ejabberd_config:get_local_option({TokenName, Host}) of
	undefined -> {error, not_found};
	Val   -> Val
    end.

%% @private
%% @doc get the rest api authentication endpoint from config file 
%% @end
-spec get_spark_authservice_endpoint(Host::string()) -> string() | {error, endpoint_notfound}.
get_spark_authservice_endpoint(Host) ->
    get_spark_auth_service_config(Host, spark_auth_endpoint). 

%% @private
%% @doc get the rest applicationId from config file
-spec get_spark_application_id(Host::string()) -> string() | {error, not_found}.
get_spark_application_id(Host) ->
    get_spark_auth_service_config(Host, spark_application_id). 
   


%% @private
%% @doc get the rest client secrete from config file 
%% @end
-spec get_spark_client_secrete(Host::string()) -> integer() | {error, not_found}.
get_spark_client_secrete(Host) ->
    case get_spark_auth_service_config(Host,spark_client_secrete) of
       {error, REASON} -> {error, REASON}; 	
       HasValue -> string_to_integer(HasValue);
        _ -> 0 %% don't retry by default
    end. 

%% @private
%% @doc get the rest client time out value in seconds from config file 
%% @end
-spec get_rest_client_timeout_in_sec(Host::string()) -> integer() | {error, not_found}.
get_rest_client_timeout_in_sec(Host) ->
    case get_spark_auth_service_config(Host,rest_client_timeout_in_sec) of
       {error, REASON} -> {error, REASON}; 
        HasValue -> string_to_integer(HasValue);
        _ -> 15 %% 15 sec is default, this seems long to me
    end.

%% @private
%% @doc get the rest client call retry attempt from config file
%% @end
-spec get_rest_call_retry_attempt(Host::string()) -> integer() | {error, not_found}.
get_rest_call_retry_attempt(Host) ->
    case get_spark_auth_service_config(Host,rest_call_retry_attempt) of
        {error, REASON} -> {error, REASON}; 
    	HasValue -> string_to_integer(HasValue);
        _ -> 0
    end.

%% @private
%% @doc check for the authentication http post response for Success is true and Error term is null
%%      anything else is error and considered authentication error and failed.
%% @end
-spec check_auth_response(AuthStatus::[tuple()]) -> {ok, authenticated} | {error, term()} | term().
check_auth_response(AuthStatus) ->
    case AuthStatus of
             [{<<"Success">>,true}, 
              _MemberId, 
              _AccessToken, 
              _ExpiresIn, 
              _AccessExpiresTime, 
              _RefreshToken, 
              _RefreshTokenExpiresTime, 
              {<<"Error">>,null},
              _IsPayingMember] -> {ok, authenticated};
             {error, Reason} -> {error, Reason};
             Error -> Error
     end.	

%% @private
%% @doc authenticate against api server from ejabberd Jid
%% @end
-spec authenticate_request(Host::string(), Email::string(), Password::string()) -> {ok, authenticated} | {error, term() | term().
authenticate_request(Host, Email, Password) ->
    Host = 
    ServiceEndpoint = get_spark_authservice_endpoint(Host),
    AppId = get_spark_application_id(Host),
    ClientSecret = get_spark_client_secrete(Host);
    BrandId = get_spark_brandId(Host),
    Resource = io_lib:format("brandid/~p/oauth2/accesstoken/application/~p",[BrandId,AppId]),
    Url = restc:construct_url(ServiceEndpoint, Resource,["client_secret", ClientSecrete], {"Email", Email}, {"Password", PAssword}),   
    post_authenticate_request(post, json, Url, [200], [], [""]);

%% @private
%% @doc post the Authentication Http Post request to the api server. Return {ok, authentication}
%% if the http code returns 200 and the Response body constains no Error message and the Success Status is true 
%% @end
-spec check_auth_response(term()) -> {ok, authenticated} | {error, term()} | term().
post_authenticate_request(Method, Type, Url, Expect, Headers, Body) ->
    PostToUrl = restc:construct_url("https://api.spark.net","brandid/1003/oauth2/accesstoken/application/1000",[{"client_secret","SXO0NoMjOqPDvPNGmEwZsHxnT5oyXTmYKpBXCx3SJTE1"}, {"Email","rrobles01@spark.net"}, {"Password","1234"}]). 

    RetValue = 
       case restc:request(Method, Type, Url, [?AUTHENTICATED], Headers, Body) of
	    {ok, Status, H, B} -> {ok, Status, H, B};
            {error, Status, H, B} -> {error, Status, H, B}; 
            Error -> Error;
       end;
    AuthStatus = 
       case RetValue of
	    {ok, ?AUTHENTICATED, _ServerInfo, ResponseBody} -> {ok, ?AUTHENTICATED, _ServerInfo, ResponseBody};
            Error -> Error
        end;
    
    check_auth_response(AuthStatus). 


%% @private
%% @doc get password to be printed to log as ******
%% @end
-spec get_password_string(_Password::string())-> string().
get_password_string(_Password)->
    "******".
string_to_integer(StringValue)->
    case string:to_integer(StringValue) of
    	{Int, _} -> Int;
        Else -> Else
    end.

%%%%%% EUNIT %%%%%%%%%%%%%%%%%%
-ifdef(TEST).

get_spark_authservice_endpoint_test()->[?assertEqual({error, endpoint_notfound}, get_spark_authservice_endpoint("BadHost")),
					?assertEqual("https://api.spark.test", get_spark_authservice_endpoint("GoodHost"))].

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

