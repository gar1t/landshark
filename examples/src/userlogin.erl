-module(userlogin).

-export([app/1]).

%%
%% @doc Page content describing the users's status.
%%
user_status(undefined) ->
    ["You are not logged in. ",
     {a, [{href, "/login"}], "Click here to log in"}];
user_status(Login) ->
    ["You are logged in as ", Login, ". ",
     {a, [{href, "/logout"}], "Click here to log out."}].

%%
%% @doc Handles "/login" paths. This is a classic "controller", which processes
%% the incoming request to determine how to respond. If the request is a 'GET',
%% displays the page. If it's a 'POST' it logs the user in provided the request
%% is valid.
%%
login(Env) ->
    case proplists:get_value(request_method, Env) of
	'GET' -> login_view("");
	'POST' ->
	    case string:strip(shark_param:get(login, Env, "")) of
		"" -> login_view("Specify a value");
		Login -> login_update(Login)
	    end
    end.

%%
%% @doc Renders the login page. Msg is displayed as a message to the user
%% at the top of the page and can be used for validation errors or other
%% instructions.
%%
login_view(Msg) ->
    P = {form, [{method, 'POST'}],
	 [{p, Msg},
	  "Log in: ", {input, [{name, login}]}, {input, [{value, "Login"},
							 {type, submit}]}
	 ]},
    shark_response:ok({html, P}).

%%
%% @doc Performs a login of Login. Subsequent requests can access the logged in
%% user via the login property of the session.
%%
%% Once the user is logged in, redirects to the root page "/".
%%
login_update(Login) ->
    Session = shark_session:set([{login, Login}]),
    shark_response:see_other("/", [Session]).

%%
%% @doc Logs the user out by clearing the session.
%%
%% Redirects to the root page after the user is logged out.
%%
logout(_Env) ->
    Session = shark_session:clear(),
    shark_response:see_other("/", [Session]).

%%
%% @doc Renders the root page by displaying the user status.
%%
root(Env) ->
    Session = shark_session:get(Env, []),
    Login = proplists:get_value(login, Session),
    shark_response:ok({html, user_status(Login)}).

%%
%% @doc Main application, which dispatches to the appropriate handler based
%% on the request path.
%%
app(Env) ->
    shark_route:dispatch(Env, [{"/login", fun login/1},
			       {"/logout", fun logout/1},
			       {"/", fun root/1}]).
