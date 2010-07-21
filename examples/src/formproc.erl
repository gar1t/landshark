-module(formproc).

-export([app/1]).

-define(form, ([{fields, [{name, text}]},
	        {actions, [submit]}])).

app(Env) ->
    case shark_form:process(Env, ?form) of
	{form, Form} ->
            shark_response:ok({html, Form});
	{action, submit} ->
            case shark_param:get(name, Env) of
                "" -> shark_response:ok(
                        {html, ["What's your name?",
                                shark_form:render(?form, Env)]});
                Name -> shark_response:ok(
                          {html, ["Hello ", Name]})
            end 
    end.
