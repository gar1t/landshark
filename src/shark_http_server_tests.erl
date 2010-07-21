-module(shark_http_server_tests).

-include_lib("eunit/include/eunit.hrl").

%%
%% @doc HTTP headers are converted to atoms in the environ that start with
%% 'http_'. There are two special headers: content-type and content-length,
%% which are stored as the atoms content_type and content_length respectively.
%%
http_request_headers_test() ->
    % headers may contain string names or atoms
    Raw = [{"Foo", "fooval"},
	   {"Foo-Bar", "foobarval"},
	   {'content-Length', "10"},
	   {bar, "barval"},
	   {"Content-Type", "ct"}],
    Expected = [{http_foo,"fooval"},
		{http_foo_bar,"foobarval"},
		{content_length,"10"},
		{http_bar,"barval"},
		{content_type,"ct"}],
    Http = shark_http_server:request_headers(Raw),
    ?assertEqual(Expected, Http).
