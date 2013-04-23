-include ("mod_spark_msgarchive_version.hrl").
-type host()::string().
-type method() :: head | get | put | post | trace | options | delete.
-type user()::string().
-type url() :: string().
-type accessToken() :: string().
-type reason() :: term().
-type result() :: tuple().
-type reply()::{ok, result()} | {error, reason()}.
-type configKey() ::atom().
-type configValue() ::atom() | integer() | string() | term()| tuple().
-record (mod_spark_msgarchive_config, {conf:: [atom(), term()]} ).
-define(CURRENT_FUNCTION_NAME(), element(2, element(2, process_info(self(), current_function)))).

%% TODO move the following elsewhere not related to ejabberd function
%%{spark_auth_endpoint, "https://api.spark.net/v2"}.
%% {spark_application_id, "1054"}. TODO: i don;tknow why applicaitonid 1054 notwork
%%{spark_client_secrete,"nZGVVfj8dfaBPKsx_dmcRXQml8o5N-iivf5lBkrAmLQ1"}
%%{spark_application_id, "1000"}.
%%{spark_client_secrete,"SXO0NoMjOqPDvPNGmEwZsHxnT5oyXTmYKpBXCx3SJTE1"}.
%%{rest_client_timeout_in_sec, 15}.
%%{rest_call_retry_attempt, 0}.
%% TODO Community to brand id mapping. move this elsewhere
%%{community2brandId, [{spark, 1, 1001}, 
%%                     {jdate, 3, 1003}, 
%%                     {cupid, 10, 1015}, 
%%                     {bbw, 23, 90410}, 
%%                     {blacksingle, 24, 90510}
%%                    ]}.
%% TODO OMg bbw is bbwpersonalsplus:23


%% TODO the following shall be moved elswhere
%%{auth_profile_miniProfile, 
%%{"/brandId/{brandId}/profile/miniProfile/{targetMemberId}", [get]}}.
%%{profile_memberstatus, 
%%{"/brandId/{brandId}/application/{applicationId}/member/{memberId}/status", [get]}}.
