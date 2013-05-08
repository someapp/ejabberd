
-module(mod_spark_msgarhive_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), 
	{I, 
		{I, start_link, []}, 
		permanent, 5000, Type, [I]
	}).

-define(RestartStrategy, one_for_one).
-define(MaximumRetry, 5).
-define(RestartIntervalInSec, 10).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    start_link([]).

start_link(Args)->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).    

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
    


    Mod_Spark_MsgArchive_Config = [],
    Mod_Spark_RestClient = [],
    Mod_Spark_MsfArchieve = [],
    {ok, { {?RestartStrategy, ?MaximumRetry, ?RestartIntervalInSec}, 
	   []} }.



