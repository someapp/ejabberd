-type host()::string().
-type method()::head | get | put | post | trace | options | delete.
-type user()::string().
-type url()::string().
-type access_token()::string().
-type reason()::term().
-type result()::tuple().
-type reply()::{ok, result()} | {error, reason()}.
-type configKey()::atom().
-type configValue()::atom() | integer() | string() | term()| tuple().

-define(CURRENT_FUNCTION_NAME(), element(2, element(2, process_info(self(), current_function)))).
-define(AUTHENTICATED, 200).
-define(DefaultType, json).

-record(state, 
	{host, 	
	 urls,	
	 client_settings, 
	 community2brandId, 
	 sanity_test_setting
	}).

-type configSetting() :: #state{}.

-export_type([
	host/0,
	method/0,
	user/0,
	url/0,
	access_token/0,
	reason/0,
	result/0,
	reply/0,
	configKey/0,
	configValue/0,
	configSetting/0
	]).
