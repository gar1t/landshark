-module(shark_session).

-export([get/1, get/2, set/1, clear/0]).

-define(session_key, '__shark_session').

%%
%% @doc Same as get(Env, undefined).
%%
get(Env) ->
    get(Env, undefined).

%%
%% @doc Returns the session term, Default, or {error, Reason} if an error
%% occurred reading the sesion.
%%
get(Env, Default) ->
    C = shark_request:get_cookies(Env),
    case proplists:get_value(?session_key, C) of
	undefined -> Default;
	TermBase64 ->
	    try base64:decode(TermBase64) of
		TermBin ->
		    try binary_to_term(TermBin) of
			Term -> Term
		    catch
			error:Error -> {error, Error}
		    end
	    catch
		error:Error -> {error, Error}
	    end
    end.

%%
%% @doc Returns a tuple of {cookie, {SessionKey, Value, Options}} where Value
%% is the value of the encoded session and Options is an appropriate set of
%% options for the session cookie. SessionKey should be considered opaque.
%%
%% The cookie should be returned using a response function in `shark_response`.
%%
set(Term) ->
    TermBin = term_to_binary(Term),
    TermBase64 = base64:encode(TermBin),
    {cookie, {?session_key, binary_to_list(TermBase64), []}}.

%%
%% @doc Returns a cookie that will cause the session to expire.
%%
clear() ->
    Expires = {{1970, 1, 1}, {0, 0, 1}},
    {cookie, {?session_key, [], [{max_age, 0}, {localtime, Expires}]}}.
