-module(shark_route).

-export([dispatch/2]).

%%% private functions
-export([find_app/2]).
-export([path_matches/2, starts_with/2]).
-export([ensure_trailing/1, remove_trailing/1, ensure_leading/1]).

%%
%% @doc Returns a tuple of {true, {ScriptName, PathInfo}} if Path matches the
%% specified prefix or false if there is no match.
%%
path_matches("/", "//") ->
    {true, {"", "/"}};
path_matches(Path, Prefix) ->
    NormPath = ensure_trailing(Path),
    NormPrefix = ensure_trailing(Prefix),
    case starts_with(NormPath, NormPrefix) of
	{true, Rest} ->
	    {true, {remove_trailing(NormPrefix),
		    ensure_leading(remove_trailing(Rest))}};
	false ->
	    false
    end.

%%
%% @doc If Path doesn't end in "/", appends "/".
%%
ensure_trailing(Path) ->
    ensure_trailing(Path, []).

ensure_trailing([], []) ->
    "/";
ensure_trailing([], Acc) ->
    lists:reverse(Acc);
ensure_trailing([H|[]], Acc) when H =/= $/ ->
    ensure_trailing([], [$/, H|Acc]);
ensure_trailing([H|T], Acc) ->
    ensure_trailing(T, [H|Acc]).

%%
%% @doc If Path ends in "/", removes it.
%%
remove_trailing(Path) ->
    remove_trailing(Path, []).

remove_trailing([], Acc) ->
    lists:reverse(Acc);
remove_trailing([H|[]], Acc) when H =:= $/ ->
    remove_trailing([], Acc);
remove_trailing([H|T], Acc) ->
    remove_trailing(T, [H|Acc]).

%%
%% @doc If Path doesn't start with "/", prepends "/".
%%
ensure_leading([]) ->
    "/";
ensure_leading("/"++Path) ->
    "/"++Path;
ensure_leading(Path) ->
    "/"++Path.

%%
%% @doc Returns {true, Rest} if Arg1 starts with Arg2, where Rest is the
%% remaining portion of Arg1.
%%
starts_with([H|T1], [H|T2]) ->
    starts_with(T1, T2);
starts_with(T, []) ->
    {true, T};
starts_with(_,_) ->
    false.

%%
%% @doc Returns {App, AppEnv} for that app that should handle Env or notfound
%% if there is no such app.
%%
find_app(Env, Routes) ->
    find_app(proplists:get_value(path_info, Env), Routes, Env).

find_app(_Path, [], _Env) ->
    notfound;
find_app(Path, [{Prefix, App}|Rest], Env) ->
    case path_matches(Path, Prefix) of
	{true, {ScriptName, AppPathInfo}} ->
	    % update Env with script_name and path_info for the app
	    AppEnv = [{script_name, ScriptName}, {path_info, AppPathInfo}|Env],
	    {App, AppEnv};
	false ->
	    find_app(Path, Rest, Env)
    end.

dispatch(Env, Routes) ->
    case find_app(Env, Routes) of
	notfound -> {404, [], "Not Found"};
	{App, AppEnv} -> App(AppEnv)
    end.
