-module(environ).

-export([app/1]).

app(Env) ->
    {200, [{content_type, "text/plain"}], io_lib:format("~p~n", [Env])}.
