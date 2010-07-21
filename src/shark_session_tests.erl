-module(shark_session_tests).

-include_lib("eunit/include/eunit.hrl").

cookie_to_env({cookie, {Name, Value, _Options}}) ->
    ValueEscaped = shark_util:escape({cookie, Value}),
    Header = lists:concat([Name, "=", ValueEscaped]),
    io:format("~p", [Header]),
    [{http_cookie, Header}].

session_test() ->
    C = shark_session:set("foo"),
    ?assertMatch({cookie, {_, "g2sAA2Zvbw==", []}}, C),
    S = shark_session:get(cookie_to_env(C)),
    ?assertEqual("foo", S).

default_session_test() ->
    ?assertEqual(foo, shark_session:get([], foo)).

%%
%% Clearing the session is a matter of expiring the session cookie. We do this
%% by setting the expires date to Jan 1, 1970 00:00:01.
%%
clear_session_test() ->
    ExpireDate = {{1970,1,1}, {0,0,1}},
    ?assertMatch({cookie, {_,[], [{max_age, 0}, {localtime, ExpireDate}]}},
		 shark_session:clear()).
