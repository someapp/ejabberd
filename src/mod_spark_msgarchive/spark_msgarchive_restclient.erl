
%%%----------------------------------------------------------------------
%%%
%%% @author : Edward Tsang <etsang@spark.net>
%%% @doc Rest client to send missed IM messages to Spark Rest API
%%% Created : 24 Apr 2013
%%%---------------------------------------------------------------------
%%%
%%% Copyright (c)
%%%----------------------------------------------------------------------
%%% @end

%% @doc spark_msgarchive_restclient takes in IM messages to Spark Rest API
%% @end
-module(spark_msgarchive_restclient).
-author('etsang@spark.net').
-behaviour(gen_server).

-include("include/mod_spark_msgarchive.hrl").
-include("include/mod_spark_msgarchive_version.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% server messages
-export([sendMissedMessages/1]). 
%%, checkMessageSendStatus/1]).

-define(SERVER, ?MODULE).
-record(sendMissedIM, 
	{	
	  brandId::string(),
	  access_token::string(),
	  recipientId::string(),
 	  messages::[tuple()] 		
	}).

%%@doc Send Offline IM messages 
%%
%%@end
-spec sendMissedMessages(Messages::[{string(), string()}]) -> {ok, posted_api_ok}| {error, reason()}.
sendMissedMessages(Messages) -> 
  gen_server:call()
 .

%%checkMessageSendStatus(Token) ->
%% .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([])->
  %%load the config default
  %%check config
  %%check dependence loaded ????
  %%log or send a notification saying it has started and of what state
  ;

init(Args)->
  %%load the config default
  %%check config
  %%check dependence loaded ????
  %%log or send a notification saying it has started and of what state  
  .



%handle_call()-> ;

handle_cast/2, 

handle_info/2,

terminate/2, 

code_change/3



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


