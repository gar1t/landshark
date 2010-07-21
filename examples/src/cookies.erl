-module(cookies).

-export([app/1]).

app(Env) ->
    % Get the current time in seconds.
    Now = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),

    % Get the cookies from Env.
    Cookies = shark_request:get_cookies(Env),

    % Get the last visit time from the cookie.
    LastVisit = case proplists:get_value(lastvisit, Cookies) of
		    undefined ->
			none;
		    Date ->
			case string:to_integer(Date) of
			    {error, _} ->
				none;
			    {I, _} ->
				I
			end
		end,

    % Create an appropriate message for the user.
    Msg = case LastVisit of
	      none ->
		  "Welcome!";
	      _ ->
		  io_lib:format("Welcome back! We saw you ~p second(s) ago.",
				[Now - LastVisit])
	  end,

    % Create a cookie for the next visit, set to expire in 60 seconds.
    LastVisitCookie = {cookie, {lastvisit, lists:concat([Now]),
				[{max_age, 60}]}},

    % Response with our message and new cookie.
    shark_response:ok({html, Msg}, [LastVisitCookie]).

