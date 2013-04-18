%%%----------------------------------------------------------------------
%%%
%%% @author : Edward Tsang <etsang@spark.net>
%%% @doc Configuration file reader for mod_spark_msgarchive_config and saves 
%%% 	 result in memory
%%% Created : 18 Apr 2013
%%%---------------------------------------------------------------------
%%%
%%% Copyright (c)
%%%----------------------------------------------------------------------
%%% @end

%% @doc This 
%%
%%       internally it is using a rest client to authentication
%% @end
-module(mod_spark_msgarchive_config).
-author('etsang@spark.net').
-behaviour(gen_server).

-define(DefaultMsgMaxSizeMb, 5).
-define(DefaultThrottleIntervalSec, 5).
-define(DefaultLogMaxSizeMb, 20).

-include("ejabberd.hrl").
-include("mod_spark_msgarchive.hrl").

-define(?MODULE,SERVER).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

%%  gen_server callbacks
%%
-export([init/1, handle_cast/2, handle_info/2, terminate/1]).
-export([handle_call/3, code_change/3]).

%% External exports
%% 
-export([load_config/0, reload_config/0]). 

%% @doc
%%
%% @end
init(Args)->
  .

%% @doc
%% Get the spark rest endpoint of sending missed IM 
%% @end
handle_call(send_missedIM_endpoint, )->
 .


%% @doc
%% Get the message queue endpoint of published missed IM to
%% @end
handle_call(rabbimq_endpoint, )->
 .

%% @doc
%% Get the max size of messages to send/push
%% @end
handle_call(message_throttle_size, )->
 .

%% @doc
%% Get the log files directory
%% @end
handle_call(mod_spark_achive_log_dir, )->
 .



%% @doc
%%
%% @end
handle_cast() ->
 .

%% @doc
%%
%% @end
handle_info() ->


 .

%% @doc
%%
%% @end
terminate()->
 .


%% @doc
%% 
%% @end
code_change()->
 .


%%%%%%%%%%%% Private %%%%%%%%%%%%%%

%% @private
%%
%% 

%% @private
%%
%%

%% @private
%%
%%

%% @private
%%
%%

%% @private
%%
%%


%% @private
%%
%%





%%%%%% EUNIT %%%%%%%%%%%%%%%%%%
-ifdef(TEST).



-endif.
