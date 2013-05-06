
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
sendMissedMessages(Pid, Messages) -> 
  State = #sendMissedIM
  gen_server:call(Pid, {sendMissedMessages, #sendMissedIM)
 .

%%checkMessageSendStatus(Token) ->
%% .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start()->
  gen_server:start().

stop(Module)->
  gen_server:stop(Module, stop);

stop()->
   stop(?Module).

state




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



handle_call({sendMissedMessages, #sendMissedIM}, _From, )-> 

;

handle_cast{{sendMissedMessagesAsync, #sendMissedIM}, } ->


. 

handle_info(_Info, State)->
  ?INFO_MSG("~p Terminiate ~p with reason ~p~n", [?CURRENT_FUNCTION_NAME(), ?Module, _Reason]), 
  {noreply, State}.

terminate(_Reason, _State)->
  ?INFO_MSG("~p Terminiate ~p with reason ~p~n", [?CURRENT_FUNCTION_NAME(), ?Module, _Reason]),  
  ok. 

code_change(OldVsn, _State, _Extra)->
  ?INFO_MSG("~p Code Change from version ~p~n", [?CURRENT_FUNCTION_NAME(), OldVsn]),
  {ok, _State}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


