-module(routes).

-export([app/1]).

users(_) -> shark_response:ok({html, "users handler"}).
events(_) -> shark_response:ok({html, "events handler"}).
index(_) -> shark_response:ok({html, "index handler"}).
other(_) -> shark_response:ok({html, "other handler"}).

app(Env) ->
    shark_route:dispatch(Env, [{"/users", fun users/1},
                               {"/events", fun events/1},
                               {"//", fun index/1},
                               {"/", fun other/1}]).
