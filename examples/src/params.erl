%%%
%%% @doc This application illustrates how GET and POST parameters work
%%% together. Use a URL query syntax to specify GET parameters and the page
%%% form to specify POST parameters.
%%%
%%% The "All parameters" section in the page shows the combined list of
%%% parameters. When using ``shark_params:get``, the first value matching the
%%% specified key is returned. Note that POST parameters are listed before GET
%%% parameters.
%%%
-module(params).

-export([app/1]).

%%
%% Renders params using an unordered list.
%%
params([]) ->
    {blockquote, [{em, "None"}]};
params(Params) ->
    {ul, param_items(Params, [])}.

%%
%% Renders a single parameter using a list item.
%%
param_items([], Acc) ->
    lists:reverse(Acc);
param_items([{Key, Value}|Rest], Acc) ->
    param_items(Rest, [{li, [Key, " = ", Value]}|Acc]).

%%
%% Renders a form that can be used to submit values "foo" and "bar".
%%
form(Env) ->
    Action = proplists:get_value(request_uri, Env),
    Foo = shark_param:get(foo, Env, ""),
    Bar = shark_param:get(bar, Env, ""),
    {form, [{method, 'POST'}, {action, Action}],
     [{p, ["foo ", {input, [{name, foo}, {type, text}, {value, Foo}]}]},
      {p, ["bar ", {input, [{name, bar}, {type, text}, {value, Bar}]}]},
      case Action of
	  "/" -> "";
	  _ -> {p, ["Form will be submitted to ", {code, Action}, " ",
		    {a, [{href, "/"}], "(reset)"}]}
      end,
      {p, {input, [{type, submit}]}}
     ]}.

%%
%% Renders the page.
%%
page(Env) ->
    {html,
     [{p, "All params:"},
      params(proplists:get_value(params, Env, [])),
      {p, "GET params:"},
      params(proplists:get_value(params_get, Env, [])),
      {p, "POST params:"},
      params(proplists:get_value(params_post, Env, [])),
      form(Env)
     ]}.

app(Env) ->
    shark_response:ok({html, page(Env)}).

