-module(cyrsasl_scram).
-author('stephen.roettger@googlemail.com').

-export([start/1,
	 stop/0,
	 mech_new/4,
	 mech_step/2]).

-include("ejabberd.hrl").

-behaviour(cyrsasl).

-record(state, {step, stored_key, server_key, username, get_password, check_password,
		auth_message, client_nonce, server_nonce}).

-define(DEFAULT_ITERATION_COUNT, 4096).
-define(SALT_LENGTH, 16).
-define(NONCE_LENGTH, 16).

start(_Opts) ->
    cyrsasl:register_mechanism("SCRAM-SHA-1", ?MODULE, scram).

stop() ->
    ok.

mech_new(_Host, GetPassword, _CheckPassword, _CheckPasswordDigest) ->
    {ok, #state{step = 2, get_password = GetPassword}}.

mech_step(#state{step = 2} = State, ClientIn) ->
	case string:tokens(ClientIn, ",") of
	["n", UserNameAttribute, ClientNonceAttribute] ->
		case parse_attribute(UserNameAttribute) of
		{$n, EscapedUserName} ->
			case unescape_username(EscapedUserName) of
			error ->
				{error, "protocol-error-bad-username"};
			UserName ->
				case parse_attribute(ClientNonceAttribute) of
				{$r, ClientNonce} ->
					case (State#state.get_password)(UserName) of
					{false, _} ->
						{error, "not-authorized", UserName};
					{Ret, _AuthModule} ->
						{StoredKey, ServerKey, Salt, IterationCount} = if
						is_tuple(Ret) ->
							Ret;
						true ->
							TempSalt = crypto:rand_bytes(?SALT_LENGTH),
							SaltedPassword = scram:salted_password(Ret, TempSalt, ?DEFAULT_ITERATION_COUNT),
							{scram:stored_key(scram:client_key(SaltedPassword)), TempSalt, ?DEFAULT_ITERATION_COUNT}
						end,
						ClientFirstMessageBare = string:substr(ClientIn, string:str(ClientIn, "n=")),
						ServerNonce = base64:encode_to_string(crypto:rand_bytes(?NONCE_LENGTH)),
						ServerFirstMessage = "r=" ++ ClientNonce ++ ServerNonce ++ "," ++
											"s=" ++ base64:encode_to_string(Salt) ++ "," ++
											"i=" ++ integer_to_list(IterationCount),
						{continue,
						 ServerFirstMessage,
						 State#state{step = 4, stored_key = StoredKey, server_key = ServerKey,
									 auth_message = ClientFirstMessageBare ++ "," ++ ServerFirstMessage,
									 client_nonce = ClientNonce, server_nonce = ServerNonce, username = UserName}}
					end;
				_Else ->
					{error, "not-supported"}
				end
			end;
		{error, Reason} ->
			{error, Reason};
		_Else ->
			{error, "bad-protocol"}
		end;
	_Else ->
	    {error, "bad-protocol"}
	end;
mech_step(#state{step = 4} = State, ClientIn) ->
	case string:tokens(ClientIn, ",") of
	[GS2ChannelBindingAttribute, NonceAttribute, ClientProofAttribute] ->
		case parse_attribute(GS2ChannelBindingAttribute) of
		{$c, "biws"} ->  %biws is base64 for n,, => channelbinding not supported
 			Nonce = State#state.client_nonce ++ State#state.server_nonce,
			case parse_attribute(NonceAttribute) of
			{$r, CompareNonce} when CompareNonce == Nonce ->
				case parse_attribute(ClientProofAttribute) of
				{$p, ClientProofB64} ->
					ClientProof = base64:decode(ClientProofB64),
					AuthMessage = State#state.auth_message ++ "," ++ string:substr(ClientIn, 1, string:str(ClientIn, ",p=")-1),
					ClientSignature = scram:client_signature(State#state.stored_key, AuthMessage),
					ClientKey = scram:client_key(ClientProof, ClientSignature),
					CompareStoredKey = scram:stored_key(ClientKey),
					if CompareStoredKey == State#state.stored_key ->
						ServerSignature = scram:server_signature(State#state.server_key, AuthMessage),
						{ok, [{username, State#state.username}], "v=" ++ base64:encode_to_string(ServerSignature)};
					true ->
						{error, "bad-auth"}
					end;
				_Else ->
					{error, "bad-protocol"}
				end;
			{$r, _} ->
				{error, "bad-nonce"};
			_Else ->
				{error, "bad-protocol"}
			end;
		_Else ->
	   		{error, "bad-protocol"}
		end;
	_Else ->
		{error, "bad-protocol"}
	end.

parse_attribute(Attribute) ->
	AttributeLen = string:len(Attribute),
	if
	AttributeLen > 3 ->
		SecondChar = lists:nth(2, Attribute),
		case is_alpha(lists:nth(1, Attribute)) of
			true ->
				if
				SecondChar == $= ->
					case string:substr(Attribute, 3) of
					String when is_list(String) ->
						{lists:nth(1, Attribute), String};
					_Else ->
						{error, "bad-format failed"}
					end;
				true ->
					{error, "bad-format second char not equal sign"}
				end;
			_Else ->
				{error, "bad-format first char not a letter"}
		end;
	true -> 
		{error, "bad-format attribute too short"}
	end.

unescape_username("") ->
	"";
unescape_username(EscapedUsername) ->
	Pos = string:str(EscapedUsername, "="),
	if
	Pos == 0 ->
		EscapedUsername;
	true ->
		Start = string:substr(EscapedUsername, 1, Pos-1),
		End = string:substr(EscapedUsername, Pos),
		EndLen = string:len(End),
		if
		EndLen < 3 ->
			error;
		true ->
			case string:substr(End, 1, 3) of
			"=2C" ->
				Start ++ "," ++ unescape_username(string:substr(End, 4));
			"=3D" ->
				Start ++ "=" ++ unescape_username(string:substr(End, 4));
			_Else ->
				error
			end
		end
	end.

is_alpha(Char) when Char >= $a, Char =< $z ->
    true;
is_alpha(Char) when Char >= $A, Char =< $Z -> 
	true;
is_alpha(_) ->
	true.
