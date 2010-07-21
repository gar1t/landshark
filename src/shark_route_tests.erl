-module(shark_route_tests).

-include_lib("eunit/include/eunit.hrl").

ensure_trailing_test() ->
    ?assertEqual("/", shark_route:ensure_trailing("")),
    ?assertEqual("/", shark_route:ensure_trailing("/")),
    ?assertEqual("foo/", shark_route:ensure_trailing("foo")),
    ?assertEqual("/foo/", shark_route:ensure_trailing("/foo")),
    ?assertEqual("/foo/", shark_route:ensure_trailing("/foo/")).

remove_trailing_test() ->
    ?assertEqual("", shark_route:remove_trailing("")),
    ?assertEqual("", shark_route:remove_trailing("/")),
    ?assertEqual("foo", shark_route:remove_trailing("foo")),
    ?assertEqual("/foo", shark_route:remove_trailing("/foo")),
    ?assertEqual("foo", shark_route:remove_trailing("foo/")),
    ?assertEqual("/foo", shark_route:remove_trailing("/foo/")).

ensure_leading_test() ->
    ?assertEqual("/", shark_route:ensure_leading("")),
    ?assertEqual("/", shark_route:ensure_leading("/")),
    ?assertEqual("/foo", shark_route:ensure_leading("foo")),
    ?assertEqual("/foo", shark_route:ensure_leading("/foo")),
    ?assertEqual("/foo/", shark_route:ensure_leading("foo/")),
    ?assertEqual("/foo/", shark_route:ensure_leading("/foo/")).
    
starts_with_test() ->
    ?assertEqual({true, []}, shark_route:starts_with("", "")),
    ?assertEqual({true, []}, shark_route:starts_with("/", "/")),
    ?assertEqual({true, []}, shark_route:starts_with("/foo", "/foo")),
    ?assertEqual({true, "/"}, shark_route:starts_with("/foo/", "/foo")),
    ?assertEqual({true, "/"}, shark_route:starts_with("/", "")),
    ?assertEqual(false, shark_route:starts_with("/bar", "/foo")),
    ?assertEqual({true, "bar"}, shark_route:starts_with("/foobar", "/foo")),
    ?assertEqual(false, shark_route:starts_with("/a", "/aa")).

path_matches_test() ->
    ?assertEqual({true, {[], "/"}},
		 shark_route:path_matches("/", "/")),
    ?assertEqual({true, {[], "/foo"}},
		 shark_route:path_matches("/foo", "/")),
    ?assertEqual({true, {"/foo", "/"}},
		 shark_route:path_matches("/foo", "/foo")),
    ?assertEqual({true, {"/foo", "/bar"}},
		 shark_route:path_matches("/foo/bar", "/foo")),
    ?assertEqual({true, {"/foo", "/bar"}},
		 shark_route:path_matches("/foo/bar/", "/foo/")),
    ?assertEqual(false, shark_route:path_matches("/foobar", "/foo")),
    ?assertEqual(false, shark_route:path_matches("/bar/", "/foo/")).    

find_app(Path, Routes) ->
    Env = [{path_info, Path}],
    case shark_route:find_app(Env, Routes) of
	{App, AppEnv} ->
	    {App, proplists:get_value(script_name, AppEnv),
	     proplists:get_value(path_info, AppEnv)};
	notfound ->
	    notfound
    end.

find_app_test() ->
    Routes = [{"/bar", bar},
	      {"/foobar", foobar},
	      {"//", index},
	      {"/index.html", index},
	      {"/", default}],
    ?assertEqual({index, [], "/"}, find_app("/", Routes)),
    ?assertEqual({index, "/index.html", "/"}, find_app("/index.html", Routes)),
    ?assertEqual({bar, "/bar", "/"}, find_app("/bar", Routes)),
    ?assertEqual({bar, "/bar", "/"}, find_app("/bar/", Routes)),
    ?assertEqual({bar, "/bar", "/foo"}, find_app("/bar/foo", Routes)),
    ?assertEqual({default, [], "/foo"}, find_app("/foo", Routes)),
    ?assertEqual({foobar, "/foobar", "/"}, find_app("/foobar", Routes)),
    ?assertEqual({default, [], "/img/hi.png"},
		 find_app("/img/hi.png", Routes)).    
