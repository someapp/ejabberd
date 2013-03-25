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

%% @doc ejabberd_auth_spark exposes the public api to hook up wih ejabberd
%% @doc internally it is using a rest client to authentication
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


%%====================================================================
%% API
%%====================================================================
-spec start(Host::string()) -> ok | {error, not_started}.
%% @doc Perform any initialization needed for the module to run
start(Host) ->
    ?MYDEBUG("~p with host: ~p~n", [?CURRENT_FUNCTION_NAME(), Host]),
    
    RETVAL = {error, not_started}, 
    ?MYDEBUG("Spark authentication with status: ~p~n", [RETVAL]),    
    RETVAL.

%% @doc Set user and password onto server. This is not needed; will be done on mainsite
-spec set_password(User::string(), Server::string(), Password::string()) -> {error, not_allowed}.
set_password(User, Server, Password) ->
    %% TODO security issue to log this, doit another way but also enough info for debugging
    ?MYDEBUG("~p with user ~p server ~p password ~p~n", [?CURRENT_FUNCTION_NAME(),User, Server, get_password_string(Password)]),
    RETVAL = {error, not_allowed},
    ?MYDEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Check if the user and password can login in server.
%% @spec (User::string(), Server::string(), Password::string(),
%%        Digest::string(), DigestGen::function()) ->
%%     true | false
-spec check_password(User::string(), Server::string(), Password::string(),Digest::string(), DigestGen::function()) ->
     false.
check_password(User, Server, Password, _Digest, _DigestGen) ->
    ?MYDEBUG("~p with user ~p server ~p password ~p digest ~p digestgen ~p~n", 
	   [?CURRENT_FUNCTION_NAME(), User, Server, get_password_string(Password), _Digest, _DigestGen]),
    RETVAL = check_password(User, Server, Password),
    ?MYDEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Check if the user and password can login in server.
-spec check_password(User::string(), Host::string(), Password::string()) -> false .
check_password(User, Host, Password) ->
    ?MYDEBUG("~p with user ~p host ~p password ~p~n", [?CURRENT_FUNCTION_NAME(), User, Host, get_password_string(Password)]),
    RETVAL = false,
    ?MYDEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Try register new user. This is not needed as this will go through website/mobile site
-spec try_register(_User::string(), _Server::string(), _Password::string()) -> {error, not_allowed}.
try_register(_User, _Server, _Password) ->
    ?MYDEBUG("~p with user ~p server ~p password ~p~n", [?CURRENT_FUNCTION_NAME(), _User, _Server, get_password_string(_Password)]),
    RETVAL = {error, not_allowed},
    ?MYDEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Registered users list do not include anonymous users logged. This functionality we don't care.
-spec dirty_get_registered_users() -> [].
dirty_get_registered_users() ->
    ?MYDEBUG("~p~n", [?CURRENT_FUNCTION_NAME()]),
    RETVAL = [],
    ?MYDEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Registered users list do not include anonymous users logged. This function is not allowed, we just don't care.
-spec get_vh_registered_users(_Host::string())-> false.
get_vh_registered_users(_Host) ->
    ?MYDEBUG("~p with host ~p~n", [?CURRENT_FUNCTION_NAME(), _Host]),
    RETVAL = false,
    ?MYDEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Get the password of the user. This function is not allowed, this case taken by mainsite.
-spec get_password(_User::string(), _Server::string()) -> false | string().
get_password(_User, _Server) ->
    ?MYDEBUG("~p with user ~p server ~p~n", [?CURRENT_FUNCTION_NAME(), _User, _Server]),
    RETVAL = false,
    ?MYDEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Get the password of the user. This function is not allowed, this case taken by mainsite.
-spec get_password_s(_User::string(), _Server::string()) -> {error, not_allowed}. 
get_password_s(_User, _Server) ->
    ?MYDEBUG("~p with user ~p server ~p~n", [?CURRENT_FUNCTION_NAME(), _User, _Server]),
    RETVAL = {error, not_allowed},
    ?MYDEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.
   
%% @doc check if user exists 
%% @doc This function checks user existence on database or on other authentication module
%% @doc since we don't offer checking user existence on our internal api. We will always return
%% @doc true - which is sad - for ejabberd to go on with its business
-spec is_user_exists(_User::string(), _Host::string()) ->true. 
is_user_exists(_User, _Host) ->
    ?MYDEBUG("~p with user ~p host ~p~~n", [?CURRENT_FUNCTION_NAME(), _User, _Host]),
    RETVAL = {error, not_implemented},
    ?MYDEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.     

%% @doc Remove user.This function is not allowed, this case taken by mainsite.
-spec remove_user(_User::string(), _Server::string())-> {error, not_allowed}.
remove_user(_User, _Server) ->
    ?MYDEBUG("~p with user ~p server ~p~n", [?CURRENT_FUNCTION_NAME(), _User, _Server]),
    RETVAL = {error, not_allowed},
    ?MYDEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc Try to remove user if the provided password is correct. This function is not allowed.
%% @doc User removal be taken care by main site. 
-spec remove_user(_User::string(), _Server::string(), _Password::string()) -> not_allowed.
remove_user(_User, _Server, _Password) ->
    ?MYDEBUG("~p with user ~p server ~p password ~p~n", [?CURRENT_FUNCTION_NAME(), _User, _Server, _Password]),
    RETVAL = not_allowed,
    ?MYDEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.

%% @doc This is only executed by ejabberd_c2s for non-SASL auth client
-spec plain_password_required()-> true.
plain_password_required() ->
    ?MYDEBUG("~p~n", [?CURRENT_FUNCTION_NAME()]),
    RETVAL = false,
    ?MYDEBUG("~p with status ~p~n", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.   
   
%% @doc Flag to indicate if using external storage to cache credentials
%% @doc TODO: I do not think we store any password right in external storage? 
%%-spec store_type()-> scram | external | plain.
-spec store_type()-> scram.
store_type() ->
    ?MYDEBUG(" store type ~p~n", [stored_type]),
    RETVAL = scram,
    ?MYDEBUG("~p with status ~p", [?CURRENT_FUNCTION_NAME(), RETVAL]),
    RETVAL.  

%%====================================================================
%% Internal functions
%%====================================================================
%% @private
%% doc get the authentication endpoint rest client to talk to
get_spark_auth_service_config(Host, TokenName) ->
    case ejabberd_config:get_local_option({TokenName, Host}) of
	undefined -> {error, not_found};
	Val   -> Val
    end.

-spec get_spark_authservice_endpoint(Host::string()) -> string() | {error, endpoint_notfound}.
get_spark_authservice_endpoint(Host) ->
    get_spark_auth_service_config(Host, spark_auth_endpoint). 
  
-spec get_spark_application_id(Host::string()) -> string() | {error, not_found}.
get_spark_application_id(Host) ->
    get_spark_auth_service_config(Host, spark_application_id). 
   
-spec get_spark_client_secrete(Host::string()) -> integer() | {error, not_found}.
get_spark_client_secrete(Host) ->
    case get_spark_auth_service_config(Host,spark_client_secrete) of
       {error, REASON} -> {error, REASON}; 	
       HasValue -> string_to_integer(HasValue);
        _ -> 0 %% don't retry by default
    end. 
 
-spec get_rest_client_timeout_in_sec(Host::string()) -> integer() | {error, not_found}.
get_rest_client_timeout_in_sec(Host) ->
    case get_spark_auth_service_config(Host,rest_client_timeout_in_sec) of
       {error, REASON} -> {error, REASON}; 
        HasValue -> string_to_integer(HasValue);
        _ -> 15 %% 15 sec is default, this seems long to me
    end.
-spec get_rest_call_retry_attempt(Host::string()) -> integer() | {error, not_found}.
get_rest_call_retry_attempt(Host) ->
    case get_spark_auth_service_config(Host,rest_call_retry_attempt) of
        {error, REASON} -> {error, REASON}; 
    	HasValue -> string_to_integer(HasValue);
        _ -> 0
    end.

%% @private
%% doc get password to be printed to log as ******
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

