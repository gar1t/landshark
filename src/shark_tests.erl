-module(shark_tests).

-export([test/0]).

test() ->
    eunit:test({inparallel, [shark_html_tests,
			     shark_http_server_tests,
			     shark_param_tests,
			     shark_response_tests,
			     shark_route_tests,
			     shark_session_tests,
			     shark_util_tests,
			     shark_form_tests]}).
