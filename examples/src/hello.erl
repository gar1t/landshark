-module(hello).

-export([app/1]).

app(_Env) ->
    {200, [{"Content-Type", "text/plain"}],
     "You're that clever shark, aren't you?"}.

