-module(run_sample).

-export([start/1]).

start([Mod]) ->
    start([8080, Mod]);
start([Port, Mod]) ->
    start([Port, Mod, app]);
start([Port, Mod, Fun]) ->
    case Mod of
        dummy ->
            io:format("use \"make MOD=<module name> run\"~n"),
            init:stop();
        _ ->
            {ok, Pid} = shark_http_server:start_link(Port, {Mod, Fun}),
            unlink(Pid),
            io:format("~nRunning sample app ~p:~p/1 on port ~p (pid ~p)~n"
                      "Type Ctrl-C Ctrl-C to exit~n~n",
                      [Mod, Fun, Port, Pid])
    end.
