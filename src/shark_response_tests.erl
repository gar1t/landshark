-module(shark_response_tests).

-include_lib("eunit/include/eunit.hrl").

%%
%% @doc When html content is specified without additional properties, the
%% "text/html" content type header is provided in the respnse.
%%
default_html_props_test() ->
    {Status, Headers, Body} = shark_response:ok({html, "Hello"}),
    ?assertEqual(200, Status),
    ?assertEqual([{"Content-Type","text/html"}], Headers),
    ?assertEqual(<<"Hello">>, list_to_binary([Body])).

%%
%% @doc To set a cookies in a response, specify one or more cookie properties.
%%
%%
cookie_prop_test() ->
    Props = [{cookie, {"foo", "Valueoffoo"}}],
    {_Status, Headers, _Body} = shark_response:ok({html, ""}, Props),
    ?assertEqual([{"Set-Cookie",
		   "foo=Valueoffoo; Version=1"}], Headers).

see_other_test() ->
    ?assertEqual({303,[{"Location","/"}],[]}, shark_response:see_other("/")),
    ?assertEqual({303, [{"Location","/"},{"Set-Cookie","foo=Foo; Version=1"}],
		  []},
		 shark_response:see_other("/", [{cookie, {"foo", "Foo"}}])).

html_slots_test() ->
    Page = [{html,
	     [{'div', [{class, header}], {slot, header, "Default Header"}},
	      {'div', [{class, body}], {slot, body, "Default Body"}},
	      {'div', [{class, footer}], {slot, footer, {p, "Copyright"}}}]}],
    ExpDefault =
	"<html>\n"
	"<div class=\"header\">\n"
	"Default Header</div>\n"
	"<div class=\"body\">\n"
	"Default Body</div>\n"
	"<div class=\"footer\">\n"
	"<p>Copyright</p>\n"
	"</div>\n"
	"</html>\n",
    {200, _, Default} = shark_response:ok({slots_html, Page}),
    io:format("~p~n~n", [list_to_binary(Default)]),
    ?assertEqual(list_to_binary(ExpDefault), list_to_binary(Default)),

    Fills = [{replace, header, "Custom Header"},
	     {append, footer, {p, "ACME Corp"}},
	     {replace, body, "My Page"}],
    ExpFilled =
	"<html>\n"
	"<div class=\"header\">\n"
	"Custom Header</div>\n"
	"<div class=\"body\">\n"
	"My Page</div>\n"
	"<div class=\"footer\">\n"
	"<p>Copyright</p>\n"
	"<p>ACME Corp</p>\n"
	"</div>\n"
	"</html>\n",
    {200, _, Filled} = shark_response:ok({slots_html, Page ++ Fills}),
    io:format("~p~n~n", [list_to_binary(Filled)]),
    ?assertEqual(list_to_binary(ExpFilled), list_to_binary(Filled)),    

    ok.
	     
