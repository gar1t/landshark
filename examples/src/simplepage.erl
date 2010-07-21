-module(simplepage).

-export([app/1]).

page() ->
    {html,
     [{head, {title, "Simple Page"}},
      {body,
       [{h1, "Welcome"},
	{p, "This is a simple page that illustrates Landhshark's compact "
	    "page rendering syntax."},
	{p, ["It's ", {em, "really"}, " easy to add HTML markup using Erlang "
	    "tuples."]},
	{p, "Adding images is easy too!"},
	{p, {img, [{src, "http://erlang.org/images/erlang.gif"}]}}
       ]}
      ]}.

app(_Env) ->
    shark_response:ok({html, page()}).

