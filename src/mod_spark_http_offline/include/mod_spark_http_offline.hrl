-type host()::string().
-type method()::head | get | put | post | trace | options | delete.
-type user()::string().
-type url()::string().
-type access_token()::string().
-type memberId()::string().
-type reason()::term().
-type result()::tuple().
-type reply()::{ok, result()} | {error, reason()}.
-type configKey()::atom().
-type configValue()::atom() | integer() | string() | term()| tuple().
-type configSettingTuple()::{configKey(), integer(), integer()}.

-define(CURRENT_FUNCTION_NAME(), element(2, element(2, process_info(self(), current_function)))).
-define(AUTHENTICATED, 200).
-define(DefaultType, json).

-record(state, 
	{host	::host(), 	
	 urls	::[{configKey(),url}],	
	 client_settings ::[{configKey(),integer()}], 
	 community2brandId ::[{configKey(),configSettingTuple()}], 
	 sanity_test_setting :: [{configKey(), configValue()}]
	}).

-type configSetting() :: #state{}.

-export_type([
	host/0,
	method/0,
	user/0,
	url/0,
	memberId/0,
	access_token/0,
	reason/0,
	result/0,
	reply/0,
	configKey/0,
	configValue/0,
	configSetting/0,
	configSettingTuple/0
	]).
