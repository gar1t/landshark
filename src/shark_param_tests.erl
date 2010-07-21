-module(shark_param_tests).

-include_lib("eunit/include/eunit.hrl").

empty_params_test() ->
    ?assertEqual(undefined, shark_param:get(foo, [])),
    ?assertEqual(undefined, shark_param:get(foo, [{params, []}])),
    ?assertEqual(undefined, shark_param:get("foo", [{params, []}])),
    ?assertEqual([], shark_param:get_all(foo, [{params, []}])),
    ?assertEqual(foo, shark_param:get("foo", [], foo)).

params_test() ->
    Params = [{"foo", "Foo1"}, {"bar", "Bar"}, {"foo", "Foo2"}],
    Env = [{params, Params}],
    ?assertEqual("Foo1", shark_param:get(foo, Env)),
    ?assertEqual("Foo1", shark_param:get("foo", Env)),
    ?assertEqual(["Foo1", "Foo2"], shark_param:get_all(foo, Env)),
    ?assertEqual(baz, shark_param:get("baz", Env, baz)).

get_params_test() ->
    GetParams = [{"foo", "Foo1"}, {"bar", "Bar"}, {"foo", "Foo2"}],
    Env = [{params_get, GetParams}],
    ?assertEqual("Foo1", shark_param:get({'GET', foo}, Env)),
    ?assertEqual("Foo1", shark_param:get({'GET', "foo"}, Env)),
    ?assertEqual(["Foo1", "Foo2"], shark_param:get_all({'GET', foo}, Env)),
    ?assertEqual(undefined, shark_param:get({'POST', "foo"}, Env)),
    ?assertEqual(baz, shark_param:get({'GET', "baz"}, Env, baz)).

post_params_test() ->
    PostParams = [{"foo", "Foo1"}, {"bar", "Bar"}, {"foo", "Foo2"}],
    Env = [{params_post, PostParams}],
    ?assertEqual("Foo1", shark_param:get({'POST', foo}, Env)),
    ?assertEqual("Foo1", shark_param:get({'POST', "foo"}, Env)),
    ?assertEqual(["Foo1", "Foo2"], shark_param:get_all({'POST', foo}, Env)),
    ?assertEqual(undefined, shark_param:get({'GET', "foo"}, Env)),
    ?assertEqual(baz, shark_param:get({'POST', "baz"}, Env, baz)).

set_params_test() ->
    Env0 = [],
    Env1 = shark_param:set(foo, "Foo", Env0),
    ?assertEqual([{params, [{"foo", "Foo"}]}], Env1),
    ?assertEqual("Foo", shark_param:get(foo, Env1)),
    Env2 = shark_param:set({'GET', bar}, "Bar", Env1),
    ?assertEqual([{params_get,[{"bar","Bar"}]},
                  {params,[{"foo","Foo"}]}], Env2),
    ?assertEqual(undefined, shark_param:get(bar, Env2)),
    ?assertEqual("Bar", shark_param:get({'GET', bar}, Env2)),
    Env3 = shark_param:set({'POST', baz}, "Baz", Env2),
    ?assertEqual([{params_post,[{"baz","Baz"}]},
                  {params_get,[{"bar","Bar"}]},
                  {params,[{"foo","Foo"}]}], Env3),
    ?assertEqual("Baz", shark_param:get({'POST', baz}, Env3)),

    % As the params lists keep getting copied over, you can override existing
    % values.
    Env4 = shark_param:set("foo", "Foo2", Env1),
    ?assertEqual([{params,[{"foo","Foo2"},{"foo","Foo"}]},
                  {params,[{"foo","Foo"}]}], Env4),
    ?assertEqual("Foo2", shark_param:get("foo", Env4)),

    ok.
    
