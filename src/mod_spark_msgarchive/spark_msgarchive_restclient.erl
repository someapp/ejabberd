
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

%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%http://api.stgv3.spark.net/v2/brandId/1003/instantmessenger/missedIM?access_token=1/DNezb4VlVSRIyJ9JKrY36Df1a/S/+buAaZhRB51gzv4=&RecipientMemberId=1234&Messages=First
-define(SERVER, ?MODULE).
-record(sendMissedIMMsg, 
	{	
	   access_token
           RecipientMemberId=
           Messages  		
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call()-> ;



