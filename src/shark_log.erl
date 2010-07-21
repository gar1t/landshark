-module(shark_log).

-import(proplists, [get_value/2]).

-export([log/3]).

log(Env, Resp, StartTime) ->
    Millis = millis(erlang:now()) - millis(StartTime),
    io:format("[~s] ~s ~s ~.2f ms~n",
              [httpd_util:rfc1123_date(),
               get_value(request_method, Env),
               get_value(request_uri, Env),
               Millis]),
    Resp.

millis({Mega, Sec, Micro}) ->
    Mega * 1000000000 + Sec * 1000 + Micro / 1000.
