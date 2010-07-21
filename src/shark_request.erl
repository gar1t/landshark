-module(shark_request).

-export([get_cookies/1]).

get_cookies(Env) ->
    case proplists:get_value(http_cookie, Env) of
	undefined ->
	    [];
	Cookie ->
	    unescape_cookie(mochiweb_cookies:parse_cookie(Cookie))
    end.

unescape_cookie(Cookie) ->
    unescape_cookie(Cookie, []).

unescape_cookie([], Acc) ->
    lists:reverse(Acc);
unescape_cookie([{Name, Value}|Rest], Acc) ->
    Unescaped = shark_util:unescape({cookie, Value}),
    unescape_cookie(Rest, [{list_to_atom(Name), Unescaped}|Acc]).
