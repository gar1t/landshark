-module(pageconstruct).

-export([app/1, start_link/1]).

-define(grids_css, "http://yui.yahooapis.com/2.7.0/build/grids/grids-min.css").

template(Header, Body, Footer) ->
    {'div', [{id, doc}],
     [{'div', [{id, hd}], Header},
      {'div', [{id, bd}], Body},
      {'div', [{id, ft}], Footer}
      ]}.

header() ->
    {h1, "Page Header"}.

body() ->
    {p, "This page is contructed using a container and three pieces of page "
        "content: a header (above), body content (the sentence you're "
        "reading, and a footer (below)."}.

footer() ->
    {p, "Page Footer"}.

page() ->
    {html,
     [{head,
       [{title, "Constructed Page"},
	{link, [{rel, stylesheet}, {type, "text/css"}, {href, ?grids_css}]}
       ]},
      {body, template(header(), body(), footer())}
      ]}.

app(_Env) ->
    shark_response:ok({html, page()}).

start_link(Port) ->
    shark_http_server:start_link(Port, {?MODULE, app}).
