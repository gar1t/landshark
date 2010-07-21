-module(shark_util_tests).

-include_lib("eunit/include/eunit.hrl").

%%
%% @doc Cookie values need to be escaped. This test confirms that the
%% "safe chars" (as per the JavaScript `escape` function) for cookies remain
%% unescaped. All other characters are escaped.
%%
escape_cookie_test() ->
    Safe = "az09@-.*+/_",
    ?assertEqual(Safe, shark_util:escape({cookie, Safe})),
    SampleUnsafe = "~_!_#_$_%_^_&_(_)_=_{_}_[_]",
    SampleEscaped = "%7E_%21_%23_%24_%25_%5E_%26_%28_%29_%3D_%7B_%7D_%5B_%5D",
    ?assertEqual(SampleEscaped, shark_util:escape({cookie, SampleUnsafe})),

    % going backwards
    ?assertEqual(SampleUnsafe, shark_util:unescape({cookie, SampleEscaped})).
