-module(shark_form_tests).

-include_lib("eunit/include/eunit.hrl").

validate_field(Field, Env) ->
    shark_form:validate_field(Field, Env).

render(Form) ->
    render(Form, []).

render(Form, Env) ->
    Rendered = shark_form:render(Form, Env),
    list_to_binary([shark_html:render(Rendered)]).

render_display(Form, Env) ->
    Rendered = shark_form:render_display(Form, Env),
    list_to_binary([shark_html:render(Rendered)]).

render_with_errors(Form, Errors) ->
    Rendered = shark_form:render_with_errors(Form, Errors, []),
    list_to_binary([shark_html:render(Rendered)]).

render_input_widget(Field) ->
    render_input_widget(Field, []).

render_input_widget(Field, Env) ->
    Rendered = shark_form:render_input_widget(Field, Env),
    list_to_binary([shark_html:render(Rendered)]).

render_action(Action) ->
    render_action(Action, []).

render_action(Action, Env) ->
    Rendered = shark_form:render_action(Action, Env),
    list_to_binary([shark_html:render(Rendered)]).

empty_form_test() ->
    Html = "<form method=\"post\" action=\"?\">\n</form>\n",
    F = [],
    io:format("~p~n", [render(F)]),
    ?assertEqual(list_to_binary(Html), render(F)).

basic_form_test() ->
    Html =
	"<form method=\"post\" action=\"/handle\" id=\"myform\" "
	"class=\"myform\">\n"
	"<h1>My Form</h1>\n"
	"<p>My sample form.</p>\n"
	"<fieldset>\n"
	"<ol>\n"
	"<li><label>Field 1</label>\n"
	"<input name=\"field1\" type=\"text\" value=\"Hello\">\n"
	"</li>\n"
	"<li><label>Field2</label>\n"
	"<input name=\"field2\" type=\"text\">\n"
	"<span class=\"error\">A value is required</span></li>\n"
	"</ol>\n"
	"</fieldset>\n"
	"<p>It's nice</p>\n"
	"<div class=\"myactions\">\n"
	"<input name=\"go\" type=\"submit\" value=\"Go\">\n"
	"<input class=\"button\" name=\"stop\" type=\"submit\" "
	"value=\"Dont Go\">\n</div>\n</form>\n",
    F = [{title, "My Form"},
	 {header, {p, "My sample form."}},
	 {footer, {p, "It's nice"}},
	 {action_path, "/handle"},
	 {css_class, "myform"},
	 {id, "myform"},
	 {fields,
	  [{field1, text, [{default, "Hello"}, {label, "Field 1"}]},
	   {field2, text, [{error, "A value is required"}]}
	  ]},
	 {actions_css_class, myactions},
	 {actions, [{go, []},
                    {stop, [{label, "Dont Go"}, {css_class, button}]}]}
	],
    io:format("~p~n", [render(F)]),
    ?assertEqual(list_to_binary(Html), render(F)).

field_name_to_label_test() ->
    Fmt = fun(Val) -> shark_form:field_name_to_label(Val) end,
    ?assertEqual("Foo", Fmt(foo)),
    ?assertEqual("Foo", Fmt("foo")),
    ?assertEqual("Foo Bar", Fmt(foo_bar)),
    ?assertEqual("Foo Bar", Fmt("foo_bar")).

text_input_basic_test() ->
    Html = "<input name=\"foo\" type=\"text\">\n",
    F = {foo, text, []},
    ?assertEqual(list_to_binary(Html), render_input_widget(F)).

text_input_full_test() ->
    Html =
	"<input id=\"foo\" name=\"foo\" type=\"text\" value=\"Foo\" readonly "
	"maxlength=\"10\">\n",
    Field = {foo, text,
	     [{id, foo},
	      {maxlength, 10},
	      {default, "Foo"},
	      readonly]},
    io:format("~p~n", [render_input_widget(Field)]),
    ?assertEqual(list_to_binary(Html), render_input_widget(Field)),
    HtmlEnv =
	"<input id=\"foo\" name=\"foo\" type=\"text\" value=\"Foo from Form\" "
	"readonly maxlength=\"10\">\n",
    Env = [{params, [{"foo", "Foo from Form"}]}],
    io:format("~p~n", [render_input_widget(Field, Env)]),
    ?assertEqual(list_to_binary(HtmlEnv), render_input_widget(Field, Env)).

action_test() ->
    ?assertEqual(<<"<input name=\"foo\" type=\"submit\" value=\"Foo\">\n">>,
		 render_action({foo, []})),
    ?assertEqual(<<"<input class=\"button\" id=\"foo\" name=\"foo\" "
		  "type=\"submit\" value=\"Click Me\">\n">>,
		 render_action({foo, [{label, "Click Me"},
                                      {css_class, button},
                                      {id, foo}]})).

%%
%% @doc A required field is contained in an li element with a CSS class of
%% "required".
%%
required_field_test() ->
    Form = [{fields, [{foo, text, [required]}]}],
    Html =
	"<form method=\"post\" action=\"?\">\n"
	"<fieldset>\n"
	"<ol>\n"
	"<li class=\"required\"><label>Foo</label>\n"
	"<input name=\"foo\" type=\"text\">\n"
	"</li>\n"
	"</ol>\n"
	"</fieldset>\n"
	"</form>\n",
    io:format("~p", [render(Form)]),
    ?assertEqual(list_to_binary(Html), render(Form)).

validate_text_test() ->
    Form = [{fields, [{foo, text, [required]}, {bar, text}]}],
    Env = [{params, [{"foo", ""}, {"bar", ""}]}],
    {errors, Errors} = shark_form:validate(Env, Form),
    ?assertEqual([{foo, {missing_required, "Required"}}], Errors),

    Html =
	"<form method=\"post\" action=\"?\">\n"
	"<fieldset>\n"
	"<ol>\n"
	"<li class=\"required\"><label>Foo</label>\n"
	"<input name=\"foo\" type=\"text\">\n"
	"<span class=\"error\">Required</span>"
	"</li>\n"
	"<li><label>Bar</label>\n"
	"<input name=\"bar\" type=\"text\">\n"
	"</li>\n"
	"</ol>\n"
	"</fieldset>\n"
	"</form>\n",
    io:format("~p~n", [render_with_errors(Form, Errors)]),
    ?assertEqual(list_to_binary(Html), render_with_errors(Form, Errors)),

    Env2 = [{params, [{"foo", "hi"}, {"bar", ""}]}],
    ?assertEqual({ok, [{foo,"hi"}, {bar,[]}]},
		 shark_form:validate(Env2, Form)).

%%
%% @doc Text block fields are rendered using textareas.
%%
textblock_test() ->
    Field1 = {foo, textblock, [{css_class, desc}, {id, desc}]},
    Env1 = [],
    Exp1 = "<textarea name=\"foo\" id=\"desc\" class=\"desc\"></textarea>\n",
    io:format("~p~n", [render_input_widget(Field1, Env1)]),
    ?assertEqual(list_to_binary(Exp1), render_input_widget(Field1, Env1)),

    Field2 = {foo, textblock, [{css_class, desc}, {id, desc}]},
    Env2 = [{params, [{"foo", "Hello there"}]}],
    Exp2 =
	"<textarea name=\"foo\" id=\"desc\" class=\"desc\">"
	"Hello there</textarea>\n",
    io:format("~p~n", [render_input_widget(Field2, Env2)]),
    ?assertEqual(list_to_binary(Exp2), render_input_widget(Field2, Env2)),

    ok.

%%
%% @doc Hidden fields are rendered outside the fieldset list.
%%
hidden_field_test() ->
    Form = [{fields, [{foo, integer, [hidden]}, {bar, integer}]}],
    Html =
	"<form method=\"post\" action=\"?\">\n"
	"<fieldset>\n"
	"<ol>\n"
	"<li><label>Bar</label>\n"
	"<input name=\"bar\" type=\"text\">\n"
	"</li>\n"
	"</ol>\n"
	"<input name=\"foo\" type=\"hidden\">\n"
	"</fieldset>\n"
	"</form>\n",
    io:format("~p~n", [render(Form)]),
    ?assertEqual(list_to_binary(Html), render(Form)).

%%
%% @doc Integer fields are validated and converted as integer types.
%%
integer_field_test() ->
    Form = [{fields, [{foo, integer, [required]},
		      {bar, integer, [hidden]},
		      {baz, integer, [{default, 10}]}]}],
    Env = [{params, [{"foo", ""}, {"bar", "not a number"}]}],
    {errors, Errors} = shark_form:validate(Env, Form),
    ?assertEqual([{foo, {missing_required, "Required"}},
		  {bar, {bad_integer, "Bad integer"}}], Errors),

    Html =
	"<form method=\"post\" action=\"?\">\n"
	"<fieldset>\n"
	"<ol>\n"
	"<li class=\"required\"><label>Foo</label>\n"
	"<input name=\"foo\" type=\"text\">\n"
	"<span class=\"error\">Required</span></li>\n"
	"<li><label>Baz</label>\n"
	"<input name=\"baz\" type=\"text\" value=\"10\">\n</li>\n"
	"</ol>\n"
	"<input name=\"bar\" type=\"hidden\">\n"
	"</fieldset>\n"
	"</form>\n",
    io:format("~p~n", [render_with_errors(Form, Errors)]),
    ?assertEqual(list_to_binary(Html), render_with_errors(Form, Errors)),

    Env2 = [{params, [{"foo", "5"}, {"bar", ""}]}],
    ?assertEqual({ok, [{foo, 5}, {bar, undefined}, {baz, 10}]},
		 shark_form:validate(Env2, Form)).

%%
%% @doc Tests text field validation.
%%
validate_text_field_test() ->
    Env = fun (Name, Value) -> [{params, [{Name, Value}]}] end,

    % Default field, empty env - no value
    ?assertEqual({ok, undefined}, validate_field({foo, text, []}, [])),

    % Default field, simple value
    ?assertEqual({ok, "Foo"},
		 validate_field({foo, text, []}, Env("foo", "Foo"))),

    % Required field, simple value
    ?assertEqual({ok, "Foo"},
		 validate_field({foo, text, [required]}, Env("foo", "Foo"))),

    % Required field, empty value
    ?assertEqual({error, {missing_required, "Required"}},
		 validate_field({foo, text, [required]}, Env("foo", ""))),

    % Required field, no value
    ?assertEqual({error, {missing_required, "Required"}},
		 validate_field({foo, text, [required]}, [])),

    % Max limit, too long value
    ?assertEqual({error, {too_long, "Too long"}},
		 validate_field({foo, text, [{max, 3}]}, Env("foo", "1234"))),

    % Min limit, too short value
    ?assertEqual({error, {too_short, "Too short"}},
		 validate_field({foo, text, [{min, 3}]}, Env("foo", "12"))),

    % Min limit + nostrip, okay value
    ?assertEqual({ok, "12  "},
		 validate_field({foo, text, [nostrip, {min, 3}]},
                                Env("foo", "12  "))),

    % Min limit, too short value when stripped
    ?assertEqual({error, {too_short, "Too short"}},
		 validate_field({foo, text, [{min, 3}]},
				Env("foo", "12  "))),
    ok.

%%
%% @doc Text fields are stripped by default. You need to specify nostrip to
%% preserve leading and trailing space.
%%
strip_test() ->
    Env = fun (Name, Value) -> [{params, [{Name, Value}]}] end,
    ?assertEqual({ok, "abc"}, validate_field({foo, text},
                                             Env("foo", " abc "))),
    ?assertEqual({ok, "abc"}, validate_field({foo, password},
                                             Env("foo", " abc "))),
    ?assertEqual({ok, "abc"}, validate_field({foo, textblock},
                                             Env("foo", " abc "))),
    ?assertEqual({ok, " abc "}, validate_field({foo, text, [nostrip]},
                                               Env("foo", " abc "))),
    ok.

%%
%% @doc Test of integer validation.
%%
validate_integer_field_test() ->
    Env = fun (Name, Value) -> [{params, [{Name, Value}]}] end,

    % Default field, empty env - no value
    ?assertEqual({ok, undefined},
		 validate_field({foo, integer, []}, [])),

    % Default field, int value
    ?assertEqual({ok, 999},
		 validate_field({foo, integer, []}, Env("foo", "999"))),

    % Default field, bad int
    ?assertEqual({error, {bad_integer, "Bad integer"}},
		 validate_field({foo, integer, []}, Env("foo", "badnum"))),

    % Required field, empty value
    ?assertEqual({error, {missing_required, "Required"}},
		 validate_field({foo, integer, [required]}, Env("foo", ""))),

    % Required field, missing value
    ?assertEqual({error, {missing_required, "Required"}},
		 validate_field({foo, integer, [required]}, [])),

    % Required field, bad int
    ?assertEqual({error, {bad_integer, "Bad integer"}},
		 validate_field({foo, integer, [required]},
				Env("foo", "badnum"))),

    ok.

%%
%% @doc Test custom validators.
%%
custom_validate_test() ->
    ValidateFoo = fun ({Name, text, _Props}, Env) ->
			  case shark_param:get(Name, Env) of
			      undefined -> {error, {noval, "No val"}};
			      "" -> {error, {empty, "Empty"}};
			      Val ->
				  case string:substr(Val, 1, 3) of
				      "Foo" -> {ok, Val};
				      _ -> {error, {notfoo, "Not foo"}}
				  end
			  end
		  end,
    Form = [{fields, [{foo, text, [{validate, ValidateFoo}]}]}],
    ?assertEqual({errors, [{foo, {noval, "No val"}}]},
		 shark_form:validate([{params, []}], Form)),
    ?assertEqual({errors, [{foo, {empty, "Empty"}}]},
		 shark_form:validate([{params, [{"foo", ""}]}], Form)),
    ?assertEqual({errors, [{foo, {notfoo, "Not foo"}}]},
		 shark_form:validate([{params, [{"foo", "bar"}]}], Form)),
    ?assertEqual({ok, [{foo, "Foo Bar"}]},
		 shark_form:validate([{params, [{"foo", "Foo Bar"}]}], Form)),
    ok.

%%
%% @doc Widget can be rendered using a function provided in the field
%% properties.
%%
custom_render_test() ->
    Render = fun({Name, text, _Props}, Env) ->
		     Val = shark_param:get(Name, Env, ""),
		     {input, [{value, Val}, {class, "custom"}]}
	     end,
    Form = [{fields, [{foo, text, [{input_widget, Render}]}]}],
    Env = [{params, [{"foo", "Foo"}]}],
    Html =
	"<form method=\"post\" action=\"?\">\n"
	"<fieldset>\n"
	"<ol>\n"
	"<li><label>Foo</label>\n"
	"<input value=\"Foo\" class=\"custom\">\n"
	"</li>\n"
	"</ol>\n"
	"</fieldset>\n"
	"</form>\n",
    io:format("~p", [render(Form, Env)]),
    ?assertEqual(list_to_binary(Html), render(Form, Env)),
    ok.

display_form_test() ->
    % Here's a copy of an ealier form. Any legal form definition can be used
    % in a call to shark_form:render_viewonly, though some of the form
    % properties are ignored.
    Form = [{title, "My Form"},
            {header, {p, "My sample form."}},
            {footer, {p, "It's nice"}},
            {action_path, "/handle"},
            {css_class, "myform"},
            {id, "myform"},
            {fields,
             [{field1, text, [{default, "Hello"}, {label, "Field 1"}]},
              {field2, text, [{error, "A value is required"}]}
             ]},
            {actions_css_class, myactions},
            {actions, [{go, []},
                       {stop, [{label, "Dont Go"}, {css_class, button}]}]}
           ],
    Env = [{params, [{"field2", "Val of field2"}]}],
    Html =
        "<div id=\"myform\" class=\"myform\">\n"
        "<h1>My Form</h1>\n"
        "<p>My sample form.</p>\n"
        "<ol>\n"
        "<li><span>Field 1</span><div>\n"
        "Hello</div>\n"
        "</li>\n"
        "<li><span>Field2</span><div>\n"
        "Val of field2</div>\n"
        "<span class=\"error\">A value is required</span></li>\n"
        "</ol>\n"
        "<p>It's nice</p>\n"
        "</div>\n",
    io:format("~p", [render_display(Form, Env)]),
    ?assertEqual(list_to_binary(Html), render_display(Form, Env)),

    ok.

%%
%% @doc Field hints are rendered as label title attributes.
%%
form_hints_test() ->
    Form = [{fields, [{foo, text, [required, {hint, "Foo field"}]},
		      {bar, integer, [{hint, "A bar field"}]}]}],
    Env = [{params, [{"foo", "Foo"}, {"bar", "Bar"}]}],

    Html =
        "<form method=\"post\" action=\"?\">\n"
        "<fieldset>\n"
        "<ol>\n"
        "<li class=\"required\"><label title=\"Foo field\">Foo</label>\n"
        "<input name=\"foo\" type=\"text\" value=\"Foo\">\n"
        "</li>\n"
        "<li><label title=\"A bar field\">Bar</label>\n"
        "<input name=\"bar\" type=\"text\" value=\"Bar\">\n"
        "</li>\n"
        "</ol>\n"
        "</fieldset>\n"
        "</form>\n",
    io:format("~p~n", [render(Form, Env)]),
    ?assertEqual(list_to_binary(Html), render(Form, Env)),

    ok.

%%
%% @doc You can apply field values to a form, which results in the field
%% default values being updated to reflect the new values.
%%
apply_field_values_test() ->
    Form = [{fields, [{foo, text},
                      {bar, integer, [{default, 123}]},
                      {baz, text, [{default, "Baz"}]}]}],
    Values = [{foo, "Foo"}, {bar, 456}],
    UpdatedForm = shark_form:apply_field_values(Values, Form),
    io:format("~p~n", [UpdatedForm]),
    ?assertEqual([{fields,
                   [{foo, text, [{default, "Foo"}]},
                    {bar, integer, [{default, 456}]},
                    {baz, text, [{default, "Baz"}]}]}],
                   UpdatedForm),
    ok.
