-module(shark_form).

-export([process/2,
         validate/2,
         validate_field/2,
         render/2,
         render_display/2,
         render_with_msg/3,
         render_with_errors/3,
         render_with_errors/4,
         apply_field_values/2,
         get_field/2]).

%%% Internal API
-export([render_input_widget/2, render_action/2, field_name_to_label/1,
	 form_action/2]).

%%
%% @doc Validates the form data provided in Env. Returns {errors, Errors,
%% RenderedForm} if there were errors or {ok, Data} if all of the form fields
%% were validated and converted into their native values.
%%
validate(Env, Form) ->
    Fields = proplists:get_value(fields, Form, []),
    {Validated, Errors} = validate_fields(Fields, Env, [], []),
    case Errors of
	[] -> {ok, Validated};
	Errors -> {errors, Errors}
    end.

%%
%% @doc Validates a list of fields, returning a tuple of Validated and Errors.
%%
validate_fields([], _Env, Validated, Errors) ->
    {lists:reverse(Validated), lists:reverse(Errors)};
validate_fields([Field|Rest], Env, Validated, Errors) ->
    {Name, Type, Props} = normalize_field(Field),
    Validate = get_validate_function({Name, Type, Props}),
    case Validate({Name, Type, Props}, Env) of
	{ok, Value} ->
	    validate_fields(Rest, Env, [{Name, Value}|Validated], Errors);
	{error, Error} ->
	    validate_fields(Rest, Env, Validated, [{Name, Error}|Errors])
    end.

%%
%% @doc Returns a function that can be used to validate the specified field.
%% If the field provides a 'validate' property function, it is returned,
%% otherwise valdiate_field/2 (default validation) is returned.
%%
get_validate_function({_Name, _Type, Props}) ->
    case proplists:get_value(validate, Props) of
	undefined -> fun validate_field/2;
	Fun -> Fun
    end.

%%
%% @doc Validates a field. Returns {ok, Value} if the field is validated.
%% Value is a converted value based on the environ params and is field type
%% specific. If the field cannot be validated, returns {error, Error}, where
%% Error is conventionally a tuple of {ErrorNo, ErrorMsg}.
%%
validate_field({Name, Type}, Env) ->
    validate_field({Name, Type, []}, Env);

validate_field({_Name, _Type, _Props}=Field, Env) ->
    case convert_field_value(Field, Env) of
	{error, Error} -> {error, Error};
	{ok, Value} ->
	    Checks = get_field_checks(Field),
	    case check_field_value(Value, Field, Env, Checks) of
		{error, Error} -> {error, Error};
		{ok, Value} ->
		    case Value of
			undefined ->
			    Default = get_field_value(Field, Env),
			    {ok, Default};
			Value ->
			    {ok, Value}
		    end
	    end
    end.

%%
%% @doc Converts inputs from Env to a field value. Returns {ok, Value} if the
%% value was converted or {error, Error} if there was a problem.
%%
convert_field_value({Name, text, Props}, Env) ->
    case shark_param:get(Name, Env) of
	undefined -> {ok, undefined};
	Value ->
	    case proplists:get_bool(nostrip, Props) of
		false -> {ok, string:strip(Value)};
		true -> {ok, Value}
	    end
    end;

convert_field_value({Name, password, Props}, Env) ->
    convert_field_value({Name, text, Props}, Env);

convert_field_value({Name, textblock, Props}, Env) ->
    convert_field_value({Name, text, Props}, Env);

convert_field_value({Name, integer, _Props}, Env) ->
    case string:strip(shark_param:get(Name, Env, "")) of
	"" -> {ok, undefined};
	ValueStr ->
	    case string:to_integer(ValueStr) of
		{ValueInt, []} -> {ok, ValueInt};
		{_, _} -> {error, {bad_integer, "Bad integer"}}
	    end
    end.

%%
%% @doc Returns a list of functions that should perform checks on a converted
%% field value. The functions should be of arity 3: (ConvertedValue, Field,
%% Env) and return {ok, Value} if the value is ok or {error, Error} if the
%% check failed.
%%
get_field_checks({_Name, text, _Props}) ->
    [fun check_required/3,
     fun check_text_max/3,
     fun check_text_min/3];

get_field_checks({Name, password, Props}) ->
    get_field_checks({Name, text, Props});

get_field_checks({Name, textblock, Props}) ->
    get_field_checks({Name, text, Props});

get_field_checks({_Name, integer, _Props}) ->
    [fun check_required/3].

%%
%% @doc Checks a field value returning {ok, Value} of the field value is
%% okay or {error, Error} if it is not.
%%
check_field_value(Value, _Field, _Env, []) ->
    {ok, Value};

check_field_value(Value, Field, Env, [Check|Rest]) ->
    case Check(Value, Field, Env) of
	{error, Error} -> {error, Error};
	{ok, Value} -> check_field_value(Value, Field, Env, Rest)
    end.

%%
%% @doc Checks the required status of field input. Returns {error, {
%% missing_required, Msg}} if a required field value isn't specified -- i.e.
%% Value is either undefined or an empty string.
%%
check_required(Value, {_Name, _Type, Props}, _Env) ->
    case proplists:get_bool(required, Props) of
	false -> {ok, Value};
	true ->
	    if
		Value == undefined orelse Value == "" ->
		    {error, {missing_required, "Required"}};
		true -> {ok, Value}
	    end
    end.

%%
%% @doc Checks whether or not a field value is too long. Returns {error,
%% {too_long, Msg}} if it is too long.
%%
check_text_max(Value, {_Name, _Type, Props}, _Env) ->
    case proplists:get_value(max, Props) of
	undefined -> {ok, Value};
	Max ->
	    Len = string:len(Value),
	    if
		Len > Max -> {error, {too_long, "Too long"}};
		true -> {ok, Value}
	    end
    end.

%%
%% @doc Checks whether or not a field value is too short. Returns {error,
%% {too_short, Msg}} if it is too long.
%%
check_text_min(Value, {_Name, _Type, Props}, _Env) ->
    case proplists:get_value(min, Props) of
	undefined -> {ok, Value};
	Min ->
	    Len = string:len(Value),
	    if
		Len < Min -> {error, {too_short, "Too short"}};
		true -> {ok, Value}
	    end
    end.

%%
%% @doc Returns either {form, RenderedForm} or (action, Action} where Action is
%% action performed by the user as per the form's actions list.
%%
process(Env, Form) ->
    case form_action(Env, proplists:get_value(actions, Form)) of
	none -> {form, render(Form, Env)};
	Action -> {action, Action}
    end.

%%
%% @doc Returns Action if one of the specified action values is in Env or
%% none if there is no action.
%%
form_action(_Env, undefined) -> none;
form_action(_Env, []) -> none;
form_action(Env, [{ActionName, _Props}|Rest]) ->
    form_action(Env, [ActionName|Rest]);
form_action(Env, [ActionName|Rest]) ->
    case shark_param:get(ActionName, Env) of
	undefined -> form_action(Env, Rest);
	_ -> ActionName
    end.

%%
%% @doc Returns structured content for the specified form.
%%
render(Form, Env) ->
    Method = proplists:get_value(method, Form, "post"),
    ActionPath = proplists:get_value(action_path, Form, "?"),
    CssClass = proplists:get_value(css_class, Form),
    Id = proplists:get_value(id, Form),
    % TODO: enctype
    {form, [{method, Method}, {action, ActionPath}, {id, Id},
	    {class, CssClass}],
     [render_title(Form, Env),
      render_header(Form, Env),
      render_input_fields(Form, Env),
      render_footer(Form, Env),
      render_actions(Form, Env)]}.

%%
%% @doc Returns structured content for a view only (non-editable) form.
%%
%% This is not a "form" in HTML and so does not use several form attributes
%% including method, action_path, and actions.
%%
%% XXX - this name is confusing. render_non_editable would be clearer I think.
%%
render_display(Form, Env) ->
    CssClass = proplists:get_value(css_class, Form),
    Id = proplists:get_value(id, Form),
    {'div', [{id, Id}, {class, CssClass}],
     [render_title(Form, Env),
      render_header(Form, Env),
      render_display_fields(Form, Env),
      render_footer(Form, Env)]}.

%%
%% @doc Renders the form with the specified message appended to the form
%% header.
%%
render_with_msg(Form, Msg, Env) ->
    Header = proplists:get_value(header, Form, []),
    MsgContent = case Msg of
                     {error, Content} ->
                         {p, [{class, "msg error"}], Content};
                     {msg, Content} ->
                         {p, [{class, "msg"}], Content};
                     Content ->
                         {p, [{class, "msg"}], Content}
                 end,
    render([{header, [Header|MsgContent]}|Form], Env).

%%
%% @doc Renders the form with the specified Errors. Errors is a
%%
render_with_errors(Form, Errors, Env) ->
    FormWithErrors = annotate_form_with_errors(Form, Errors),
    render(FormWithErrors, Env).

%%
%% @doc Same as render_with_errors but includes an error message in the form
%% header.
%%
render_with_errors(Form, Msg, Errors, Env) ->
    Header = proplists:get_value(header, Form, []),
    MsgContent = {p, [{class, "msg error"}], Msg},
    render_with_errors([{header, [Header|MsgContent]}|Form],
                       Errors, Env).

%%
%% @doc Renders the form title.
%%
render_title(Form, _Env) ->
    case proplists:get_value(title, Form) of
	undefined -> [];
	Title -> {h1, Title}
    end.

%%
%% @doc Renders the form header.
%%
render_header(Form, _Env) ->
    proplists:get_value(header, Form, []).

%%
%% @doc Renders the form footer.
%%
render_footer(Form, _Env) ->
    proplists:get_value(footer, Form, []).

%%
%% @doc Renders a form's fields.
%%
render_input_fields(Form, Env) ->
    case proplists:get_value(fields, Form) of
	undefined -> [];
	Fields ->
	    NormFields = lists:map(fun normalize_field/1, Fields),
	    VisibleFields = [render_input_field(Field, Env) ||
				Field <- NormFields, not is_hidden(Field)],
	    HiddenFields = [render_hidden_input_field(Field, Env) ||
			       Field <- NormFields, is_hidden(Field)],
	    {fieldset, [{ol, VisibleFields}|HiddenFields]}
    end.

%%
%% @doc Renders display only (non-editable) fields.
%%
render_display_fields(Form, Env) ->
    case proplists:get_value(fields, Form) of
	undefined -> [];
	Fields ->
	    NormFields = lists:map(fun normalize_field/1, Fields),
	    VisibleFields = [render_display_field(Field, Env) ||
				Field <- NormFields, not is_hidden(Field)],
	    {ol, VisibleFields}
    end.

%%
%% @doc Returns true if the field is hidden, false if not.
%%
is_hidden({_Name, _Type, Props}) ->
    proplists:get_bool(hidden, Props).

%%
%% @doc Renders a visible (not hidden) field.
%%
%% Visible fields are rendered with labels and appropriate descriptions.
%%
render_input_field(Field, Env) ->
    Attrs = field_container_attrs(Field, Env),
    {_Name, _Type, Props} = Field,
    Widget = case proplists:get_value(input_widget, Props) of
		 undefined -> render_input_widget(Field, Env);
                 {M, F} -> M:F(Field, Env);
		 F -> F(Field, Env)
	     end,
    Label = render_label(Field, Env),
    Annotations = render_annotations(Field, Env),
    {li, Attrs, [Label, Widget] ++ Annotations}.

%%
%% @doc Renders a hidden field.
%%
%% Hidden fields are rendered without labels.
%%
render_hidden_input_field(Field, Env) ->
    {_Name, _Type, Props} = Field,
    case proplists:get_value(render_widget, Props) of
	undefined -> render_hidden_widget(Field, Env);
	RenderWidget -> RenderWidget(Field, Env)
    end.

%%
%% @doc Renders a display (non-editable) field.
%%
render_display_field(Field, Env) ->
    Attrs = field_container_attrs(Field, Env),
    {_Name, _Type, Props} = Field,
    Widget = case proplists:get_value(render_widget, Props) of
		 undefined -> render_display_widget(Field, Env);
		 RenderWidget -> RenderWidget(Field, Env)
	     end,
    Label = render_display_label(Field, Env),
    Annotations = render_annotations(Field, Env),
    {li, Attrs, [Label, Widget] ++ Annotations}.

%%
%% @doc Returns attributes for a field container.
%%
%% Required fields are wrapped in a container with a class=required.
%%
field_container_attrs({_Name, _Type, Props}, _Env) ->
    A1 = case proplists:get_value(required, Props) of
             true -> [{class, required}];
             _ -> []
         end,
    A2 = case proplists:get_value(container_id, Props) of
             undefined -> A1;
             Id -> [{id, Id}|A1]
         end,
    A2.

%%
%% @doc Renders a field label.
%%
render_label({Name, _Type, Props}, _Env) ->
    Label = case proplists:get_value(label, Props) of
		undefined -> field_name_to_label(Name);
		L -> L
	    end,
    case proplists:get_value(hint, Props) of
	undefined -> {label, Label};
	Hint -> {label, [{title, Hint}], Label}
    end.

%%
%% @doc Renders a field label for a display form. Display forms aren't
%% actually forms so we don't use proper 'label' HTML elements for this.
%%
render_display_label({Name, _Type, Props}, _Env) ->
    Label = case proplists:get_value(label, Props) of
		undefined -> field_name_to_label(Name);
		L -> L
	    end,
    case proplists:get_value(hint, Props) of
	undefined -> {span, Label};
	Hint -> {span, [{title, Hint}], Label}
    end.

%%
%% @doc Renders a field's input widget.
%%
render_input_widget({Name, text, Props}, Env) ->
    Attrs = base_text_attrs({Name, text, Props}, Env),
    {input, lists:reverse(Attrs)};

render_input_widget({Name, textblock, Props}=Field, Env) ->
    FormValue = case shark_param:get(Name, Env) of
	undefined -> get_field_value(Field, Env, "");
	Val -> Val
    end,
    {textarea, [{name, Name}|base_attrs(Props)], FormValue};

render_input_widget({Name, password, Props}, Env) ->
    Attrs = base_text_attrs({Name, password, Props}, Env),
    {input, lists:reverse(Attrs)};

render_input_widget({Name, integer, Props}, Env) ->
    Attrs = base_text_attrs({Name, text, Props}, Env),
    {input, lists:reverse(Attrs)}.

%%
%% @doc Provides attributes applicable to a text widget.
%%
base_text_attrs({Name, Type, Props}, Env) ->
    MaxLength = proplists:get_value(maxlength, Props),
    ReadOnly = proplists:get_value(readonly, Props),
    BaseAttrs = base_input_attrs({Name, Type, Props}, Env),
    [{maxlength, MaxLength}, {readonly, ReadOnly}|BaseAttrs].

%%
%% @doc Provides attributes for input widgets.
%%
base_input_attrs({Name, Type, Props}, Env) ->
    FormValue = input_form_value({Name, Type, Props}, Env),
    [{value, FormValue}, {type, Type}, {name, Name}|base_attrs(Props)].

%%
%% @doc Provides attributes for all widgets (id and class).
%%
base_attrs(Props) ->
    Id = proplists:get_value(id, Props),
    CssClass = proplists:get_value(css_class, Props),
    [{id, Id}, {class, CssClass}].

%%
%% @doc Returns a form value for a field.
%%
input_form_value({Name, text, Props}=Field, Env) ->
    case shark_param:get(Name, Env) of
	undefined -> get_field_value(Field, Env);
	FormValue ->
	    case proplists:get_bool(nostrip, Props) of
		false -> string:strip(FormValue);
		true -> FormValue
	    end
    end;

input_form_value({Name, password, Props}, Env) ->
    input_form_value({Name, text, Props}, Env);

input_form_value({Name, _Type, _Props}=Field, Env) ->
    case shark_param:get(Name, Env) of
	undefined -> get_field_value(Field, Env);
	FormValue -> FormValue
    end.

%%
%% @doc Renders a display (non-editable) widget.
%%
render_display_widget({Name, _Type, Props}=Field, Env) ->
    Value = case shark_param:get(Name, Env) of
                undefined -> get_field_value(Field, Env);
                FormValue -> FormValue
            end,
    {'div', base_attrs(Props), Value}.

%%
%% @doc Renders a hidden widget.
%%
render_hidden_widget({Name, Type, Props}, Env) ->
    FormValue = input_form_value({Name, Type, Props}, Env),
    Attrs = [{value, FormValue}, {type, hidden}, {name, Name}|
             base_attrs(Props)],
    {input, lists:reverse(Attrs)}.

%%
%% Applies note and error annotations to a field.
%%
render_annotations({_Name, _Type, Props}, _Env) ->
    append_annotation(note, Props, append_annotation(error, Props, [])).

%%
%% @doc Applies an annotation to a field if it exists.
%%
%% Annotations are used to present field notations and errors.
%%
append_annotation(Type, Props, List) ->
    case proplists:get_value(Type, Props) of
	undefined -> List;
	Msg ->
	    Annotation = {span, [{class, Type}], Msg},
	    [Annotation|List]
    end.

%%
%% @doc Renders form actions.
%%
render_actions(Form, Env) ->
    case proplists:get_value(actions, Form) of
	undefined -> [];
	Actions ->
	    CssClass = proplists:get_value(actions_css_class, Form, "actions"),
	    Rendered = lists:map(
                         fun(A) -> render_action(normalize_action(A), Env) end,
                         Actions),
	    {'div', [{class, CssClass}], Rendered}
    end.

%%
%% @doc Renders a form action.
%%
render_action({Name, Props}, _Env) ->
    Label = case proplists:get_value(label, Props) of
		undefined -> field_name_to_label(Name);
		L -> L
	    end,
    Type = case proplists:get_bool(button, Props) of
               true -> button;
               false -> submit
           end,
    Attrs = [{value, Label},
	     {type, Type},
	     {name, Name}|base_attrs(Props)],
    {input, lists:reverse(Attrs)}.

%%
%% @doc Converts a field name to a default label.
%%
%% The default label conists of one or more capitalized words separated by
%% spaces. Underscore characters are used to break field names into words.
%%
field_name_to_label(Name) when is_atom(Name) ->
    field_name_to_label(atom_to_list(Name));

field_name_to_label(Name) when is_list(Name) ->
    Words = shark_util:capitalize_words(string:tokens(Name, "_")),
    string:join(Words, " ").

%%
%% @doc Annotates a form with errors.
%%
annotate_form_with_errors(Form, Errors) ->
    case proplists:get_value(fields, Form) of
	undefined -> Form;
	Fields ->
	    AnnotatedFields = annotate_fields_with_errors(Fields, Errors),
	    [{fields, AnnotatedFields}|Form]
    end.

%%
%% @doc Annotates a list of fields with errors.
%%
annotate_fields_with_errors(Fields, Errors) ->
    Annotate =
	fun(Field) ->
		{Name, _Type, _Props} = normalize_field(Field),
		annotate_field(Field, error, proplists:get_value(Name, Errors))
	end,
    lists:map(Annotate, Fields).

%%
%% @doc Annotates a field.
%%
annotate_field(Field, _Tag, undefined) ->
    Field;

annotate_field({Name, Type}, Tag, Annotation) ->
    annotate_field({Name, Type, []}, Tag, Annotation);

annotate_field({Name, Type, Props}, Tag, {_Id, Msg}) ->
    {Name, Type, [{Tag, Msg}|Props]}.

%%
%% @doc Normalizes a field to always be {Name, Type, Properties}.
%%
normalize_field({Name, Type}) ->
    {Name, Type, []};

normalize_field({Name, Type, Props}) ->
    {Name, Type, Props}.

%%
%% @doc Normalizes an action to always be {Name, Properties}.
%%
normalize_action({Name, Props}) ->
    {Name, Props};

normalize_action(Name) ->
    {Name, []}.

%%
%% @doc Returns Form modified with appropriate default field values using
%% Values. This can be used to "populate" a form with field values. Field
%% values should be in a the type that corresponds to the field type.
%%
apply_field_values(Values, Form) ->
    Fields = lists:map(fun normalize_field/1,
                       proplists:get_value(fields, Form)),
    UpdatedFields = apply_field_values(Fields, Values, []),
    [{fields, UpdatedFields}|proplists:delete(fields, Form)].

apply_field_values([], _Values, Acc) ->
    lists:reverse(Acc);
apply_field_values([{Name, Type, Props}|Rest], Values, Acc) ->
    NewProps = case proplists:get_value(Name, Values) of
                   undefined -> Props;
                   Value -> [{default, Value}|proplists:delete(default, Props)]
               end,
    apply_field_values(Rest, Values, [{Name, Type, NewProps}|Acc]).

%%
%% @doc Returns the specified field from Form or undefined if the field doesn't
%% exist.
%%
get_field(Name, Form) ->
    find_field(Name, proplists:get_value(fields, Form, [])).

find_field(_Name, []) ->
    undefined;
find_field(Name, [{Name, _Type}=Field|_T]) ->
    Field;
find_field(Name, [{Name, _Type, _Props}=Field|_T]) ->
    Field;
find_field(Name, [_H|T]) ->
    find_field(Name, T).

%%
%% @doc Returns the value associated with a field. This looks in Env for
%% field_values and returns a value for the field if provided. If no such env
%% value exists, returns the default value in the field properties.
%%
get_field_value(Field, Env) ->
    get_field_value(Field, Env, undefined).

get_field_value({Name, Type}, Env, Default) ->
    get_field_value({Name, Type, []}, Env, Default);
get_field_value({Name, _Type, Props}, Env, Default) ->
    case get_field_env_value(Name, Env) of
        undefined -> proplists:get_value(default, Props, Default);
        Val -> Val
    end.

get_field_env_value(Name, Env) ->
    case proplists:get_value(field_values, Env) of
        undefined -> undefined;
        Vals -> proplists:get_value(Name, Vals)
    end.
