-module(shark_static).

-export([from_env/2,
         from_env/3,
         from_path/2,
         from_path/3,
         expires_after/1]).

relative_path("/"++Path) ->
    Path;
relative_path(Path) ->
    Path.

full_path(undefined, _Path) ->
    undefined;
full_path(DocRoot, Path) ->
    filename:join(DocRoot, relative_path(Path)).

from_path(Path, DocRoot) ->
    from_path(Path, DocRoot, []).

from_path(Path, DocRoot, Props) ->
    {filepath, full_path(DocRoot, Path), Props}.

from_env(Env, DocRoot) ->
    from_env(Env, DocRoot, []).

from_env(Env, DocRoot, Props) ->
    PathInfo = proplists:get_value(path_info, Env),
    {filepath, full_path(DocRoot, PathInfo), Props}.

expires_after(AfterSeconds) ->
    {Mega, Sec, Us} = erlang:now(),
    httpd_util:rfc1123_date(calendar:now_to_universal_time(
                              {Mega, Sec + AfterSeconds, Us})).
