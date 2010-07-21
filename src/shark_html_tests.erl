-module(shark_html_tests).

-include_lib("eunit/include/eunit.hrl").

%%
%% @doc Helper to render HTML as a binary.
%%
render(Content) ->
    list_to_binary([shark_html:render(Content)]).

%%
%% @doc Helper to return a quoted attribute value as a binary.
%%
quote_attr(Value) ->
    list_to_binary(shark_html:quote_attr(Value)).

%%
%% @doc Tests quoting of attribute values.
%%
%% Attribute values are always double quoted. Characters that are escaped are
%% (exclusively) > < & "
%%
quote_attr_test() ->
    ?assertEqual(<<"\"\"">>, quote_attr("")),
    ?assertEqual(<<"\"foo\"">>, quote_attr("foo")),
    ?assertEqual(<<"\"M &amp; M\"">>, quote_attr("M & M")),
    ?assertEqual(<<"\"1 &lt; 2\"">>, quote_attr("1 < 2")),
    ?assertEqual(<<"\"2 &gt; 1\"">>, quote_attr("2 > 1")),
    ?assertEqual(<<"\"&quot;Hello&quot;\"">>, quote_attr("\"Hello\"")),
    ?assertEqual(<<"\"a&lt;&gt;&amp;&quot;z\"">>, quote_attr("a<>&\"z")).

%%
%% @doc Empty content returns an empty binary string.
%%
render_empty_test() ->
    ?assertEqual(<<"">>, render("")),
    ?assertEqual(<<"">>, render([])).

%%
%% @doc Atoms are converted to string binaries.
%%
render_atom_test() ->
    ?assertEqual(<<"foo">>, render(foo)).

%%
%% @doc Strings are converted to binaries using UTF-8 encoding.
%%
render_string_test() ->
    ?assertEqual(<<"foo">>, render("foo")).

%%
%% @doc Binaries are rendered without modification.
%%
render_binary_test() ->
    ?assertEqual(<<"foo">>, render(<<"foo">>)).

%%
%% @doc A basic form of content is a tag with string content.
%%
render_basic_tag_test() ->
    C = {p, "Hello"},
    Html = "<p>Hello</p>\n",
    ?assertEqual(list_to_binary(Html), render(C)).

%%
%% @doc Content may be specified as a list of content.
%%
render_list_of_tags_test() ->
    C = [{p, "hi"}, {"p", "there"}],
    Html =
	"<p>hi</p>\n"
	"<p>there</p>\n",
    ?assertEqual(list_to_binary(Html), render(C)).

%%
%% @doc A tag may contain content that is specified as a list of content.
%%
render_tag_with_list_content_test() ->
    C = {'div',
	 [{p, "Par 1"},
	  {p, "Par 2"}]},
    Html =
	"<div>\n"
	"<p>Par 1</p>\n"
	"<p>Par 2</p>\n"
	"</div>\n",
    ?assertEqual(list_to_binary(Html), render(C)).

%%
%% @doc Empty tags don't have closing tags in HTML. Use the empty atom to
%% designate a tag as being empty.
%%
render_empty_tag_test() ->
    C1= {br, empty},
    Html = "<br>\n",
    ?assertEqual(list_to_binary(Html), render(C1)),
    C2= {br},
    ?assertEqual(list_to_binary(Html), render(C2)),
    ok.

%%
%% @doc Empty tags may have attributes.
%%
render_empty_tag_with_attrs_test() ->
    C1 = {img, [{src, "hello.gif"}], empty},
    Html = "<img src=\"hello.gif\">",
    ?assertEqual(list_to_binary(Html), render(C1)),
    C2 = {img, [{src, "hello.gif"}]},
    ?assertEqual(list_to_binary(Html), render(C2)),
    ok.

%%
%% @doc Here's a tag with attributes and string content.
%%
render_tag_with_attrs_test() ->
    C = {p, [{id, "greeting"}, {class, "cheery"}], "Hello"},
    Html =
	"<p id=\"greeting\" class=\"cheery\">Hello</p>\n",
    ?assertEqual(list_to_binary(Html), render(C)).

%%
%% @doc Attribte names and values may be specified as strings as well as
%% atoms.
%%
render_string_attr_names_test() ->
    C = {some_tag, [{atom_val, "A"}, {"string_val", "s"}], []},
    Html = "<some_tag atom_val=\"A\" string_val=\"s\"></some_tag>\n",
    ?assertEqual(list_to_binary(Html), render(C)),
    C2 = {some_tag, [{atom_val, 'A'}, {"string_val", s}], []},
    ?assertEqual(list_to_binary(Html), render(C2)).

%%
%% @doc Tag names may be strings as well as atoms.
%%
render_string_tag_test() ->
    ?assertEqual(<<"<foo>\n">>, render({"foo", empty})).

%%
%% @doc A lone tag is also supported.
%%
render_lone_tag_test() ->
    ?assertEqual(<<"<foo></foo>\n">>, render({foo})).

%%
%% @doc Some tags are known to be empty and have special support.
%%
render_known_empty_tags_test() ->
    ?assertEqual(<<"<img src=\"pic.gif\">">>,
		 render({img, [{src, "pic.gif"}]})),
    ?assertEqual(<<"<br>\n">>, render({br})).

%%
%% @doc A simple HTML page.
%%
render_simple_html_page_test() ->
    C = {html,
	 [{head,
	   [{title, "Simple Page"}]},
	  {body,
	   [{h1, "Welcome"},
	    {p, [{class, "desc"}], "A very simple page."},
	    {img, [{src, "sample.gif"}, {id, "sample_pic"}]}]}
	  ]},
    Html =
	"<html>\n"
	"<head>\n"
	"<title>Simple Page</title>\n"
	"</head>\n"
	"<body>\n"
	"<h1>Welcome</h1>\n"
	"<p class=\"desc\">A very simple page.</p>\n"
        "<img src=\"sample.gif\" id=\"sample_pic\">"
	"</body>\n"
	"</html>\n",
    ?assertEqual(list_to_binary(Html), render(C)).

%%
%% @doc Tests some tag inlining.
%%
render_inline_tags_test() ->
    C = {p, ["This is an ", {em, "HTML snippet"}, "."]},
    Html = "<p>This is an <em>HTML snippet</em>.</p>\n",
    ?assertEqual(list_to_binary(Html), render(C)).

%%
%% @doc Tests a basic templat with content.
%%
render_template_with_content_test() ->
    Header = {h1, "Page Header"},
    Body = {p, "Page Body"},
    Footer = {p, "Page Footer"},
    Template = {'div', [{id, doc}],
		[{'div', [{id, hd}], Header},
		 {'div', [{id, bd}], Body},
		 {'div', [{id, ft}], Footer}
		]},
    Page = {html,
	    [{head, {title, "Constructed Page"}},
	     {body, Template}
	    ]},
    Html = "<html>\n"
	   "<head>\n"
	   "<title>Constructed Page</title>\n"
	   "</head>\n"
	   "<body>\n"
	   "<div id=\"doc\">\n"
	   "<div id=\"hd\">\n"
	   "<h1>Page Header</h1>\n"
	   "</div>\n"
	   "<div id=\"bd\">\n"
	   "<p>Page Body</p>\n"
	   "</div>\n"
	   "<div id=\"ft\">\n"
	   "<p>Page Footer</p>\n"
	   "</div>\n"
	   "</div>\n"
	   "</body>\n"
	   "</html>\n",
    io:format(render(Page), []),
    ?assertEqual(list_to_binary(Html), render(Page)).

%%
%% @doc Lists of "other" content (atom, string, binary) are supported.
%%
render_lists_other_test() ->
    ?assertEqual(<<"abc">>, render(["a", b, <<"c">>])),
    ?assertEqual(<<"abc">>, render([a, "b", <<"c">>])),
    ?assertEqual(<<"abc">>, render([<<"a">>, b, "c"])),
    ?assertEqual(<<"abc">>, render(["a", <<"b">>, c])),
    ?assertEqual(<<"abc">>, render([a, <<"b">>, "c"])),
    ?assertEqual(<<"abc">>, render([<<"a">>, "b", c])).

%%
%% @doc Attributes whose values = 'undefined' are not rendered.
%%
undefined_attr_test() ->
    ?assertEqual(<<"<foo baz=\"defined\"></foo>\n">>,
		 render({foo, [{bar, undefined}, {baz, defined}], []})),
    ?assertEqual(<<"<foo bar=\"defined\"></foo>\n">>,
		 render({foo, [{bar, defined}, {baz, undefined}], []})),
    ?assertEqual(<<"<foo></foo>\n">>,
		 render({foo, [{bar, undefined}, {baz, undefined}], []})).

%%
%% @doc If a single atom is provided as an attribute, it is
%% rendered without a value. {Atom, true} is treated the same way.
%%
atom_attr_test() ->
    ?assertEqual(<<"<foo bar baz=\"bam\"></foo>\n">>,
		 render({foo, [bar, {baz, bam}], []})),
    ?assertEqual(<<"<foo bar baz=\"bam\"></foo>\n">>,
		 render({foo, [{bar, true}, {baz, bam}], []})).
