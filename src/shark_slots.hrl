-include_lib("eunit/include/eunit.hrl").

cjoin_test() ->

    % joining to empty
    ?assertEqual(empty, cjoin(empty, empty)),
    ?assertEqual([], cjoin("", empty)),
    ?assertEqual(123, cjoin(123, empty)),
    ?assertEqual({foo, bar}, cjoin({foo, bar}, empty)),
    
    % joining non lists
    ?assertEqual([1, 2], cjoin(1, 2)),
    ?assertEqual(["foo", "bar"], cjoin("foo", "bar")),
    ?assertEqual([{foo, bar}, {bar, foo}], cjoin({foo, bar}, {bar, foo})),

    % joining to a list
    ?assertEqual([[], []], cjoin([], [])),
    ?assertEqual([foo, []], cjoin(foo, [])),
    ?assertEqual([foo, [bar]], cjoin(foo, [bar])),
    ?assertEqual([[foo], [bar]], cjoin([foo], [bar])),
    ?assertEqual([[], [bar]], cjoin([], [bar])),
    
    ok.

ccons_test() ->

    ?assertEqual([a], ccons(a, [])),
    ?assertEqual([a, b], ccons(a, [b])),
    ?assertEqual([a, b, c], ccons(a, [b, c])),

    ?assertEqual([], ccons(empty, [])),
    ?assertEqual([a], ccons(empty, [a])),

    ok.

cappend_test() ->
    ?assertEqual([a], cappend(a, empty)),
    ?assertEqual([b, a], cappend(a, b)),
    ?assertEqual([b, c, a], cappend(a, [b, c])),
    ?assertEqual([c, a, b], cappend([a, b], c)),
    ?assertEqual([c, d, a, b], cappend([a, b], [c, d])),
    ?assertEqual(["b", a], cappend(a, "b")),
    ok.

cprepend_test() ->
    ?assertEqual([a], cprepend(a, empty)),
    ?assertEqual([a, b], cprepend(a, b)),
    ?assertEqual([a, b, c], cprepend(a, [b, c])),
    ?assertEqual([a, b, c], cprepend([a, b], c)),
    ?assertEqual([a, b, c, d], cprepend([a, b], [c, d])),
    ?assertEqual([a, "b"], cprepend(a, "b")),
    ok.

no_slots_split_test() ->
    D = dict:new(),

    % empty list
    ?assertEqual({[], D}, split("")),

    % single atom
    ?assertEqual({foo, D}, split(foo)),

    % list of atoms
    ?assertEqual({[a, b, c], D}, split([a, b, c])),

    % list of strings
    ?assertEqual({["a", "b", "c"], D}, split(["a", "b", "c"])),

    % single tuples
    ?assertEqual({{a}, D}, split({a})),
    ?assertEqual({{a, b}, D}, split({a, b})),
    ?assertEqual({{a, b, c}, D}, split({a, b, c})),
    ?assertEqual({{a, b, c, d}, D}, split({a, b, c, d})),

    % lists of tuples
    ?assertEqual({[{a}], D}, split([{a}])),
    ?assertEqual({[{a, b}], D}, split([{a, b}])),
    ?assertEqual({[{a, b, c}], D}, split([{a, b, c}])),
    ?assertEqual({[{a, b, c, d}], D}, split([{a, b, c, d}])),

    ?assertEqual({[{a}, {b}], D}, split([{a}, {b}])),
    ?assertEqual({[{a}, {b, c}, {d, e}], D}, split([{a}, {b, c}, {d, e}])),

    % common html content patterns
    ?assertEqual({{p, [], "hello"}, D}, split({p, [], "hello"})),
    ?assertEqual({{p, [{class, "msg"}], "hello"}, D},
                 split({p, [{class, "msg"}], "hello"})),
    ?assertEqual({{p, [{class, "msg"}], {a, [{href, "/"}], "hello"}}, D},
                 split({p, [{class, "msg"}], {a, [{href, "/"}], "hello"}})),

    ok.

ops_split_test() ->
    
    {C1, Ops1} = split({append, s1, foo}),
    ?assertEqual(empty, C1),
    ?assertEqual([{s1, [{append, foo}]}], dict:to_list(Ops1)),

    {C2, Ops2} = split([{slot, s1}, {append, s1, foo}]),
    ?assertEqual([{slot, s1}], C2),
    ?assertEqual([{s1, [{append, foo}]}], dict:to_list(Ops2)),

    % ops are stored in reverse order, but applied in order
    {C3, Ops3} = split([foo, {append, s1, bar}, {prepend, s1, baz}]),
    ?assertEqual([foo], C3),
    ?assertEqual([{s1, [{prepend, baz}, {append, bar}]}],
                 dict:to_list(Ops3)),

    ok.

no_ops_fill_test() ->
    D = dict:new(),

    ?assertEqual(empty, fill({slot, foo}, D)),
    ?assertEqual("Foo", fill({slot, foo, "Foo"}, D)),
    ?assertEqual({foo, "Foo"}, fill({slot, foo, {foo, "Foo"}}, D)),

    ?assertEqual([], fill([{slot, foo}], D)),
    ?assertEqual([bar], fill([{slot, foo, bar}], D)),
    ?assertEqual([{bar, foo}], fill([{slot, foo}, {bar, foo}], D)),

    ?assertEqual({foo, empty}, fill({foo, {slot, s1}}, D)),
    ?assertEqual({foo, bar, empty}, fill({foo, bar, {slot, s1}}, D)),

    ?assertEqual({html, [{head,"title"}, {body,"hello"}]},
                 fill({html, [{head, {slot, title, "title"}},
                              {body, "hello"}]}, D)),
    ok.

replace_test() ->

    % no applicable slot
    ?assertEqual(empty, fill({replace, s1, ""})),
    ?assertEqual([foo], fill([foo, {replace, s1, ""}])),
    ?assertEqual([s1], fill([{slot, s1, s1}, {replace, s2, foo}])),

    % simple case
    ?assertEqual([foo], fill([{slot, s1}, {replace, s1, foo}])),
    ?assertEqual([foo], fill([{slot, s1, s1}, {replace, s1, foo}])),

    % typical html case
    C1 = [{html, [{head, {title, {slot, title, "Default Title"}}},
                  {body, "hello"}]},
          {replace, title, "My Page"}],
    F1 = fill(C1),
    io:format("~p", [F1]),
    ?assertEqual([{html, [{head, {title, "My Page"}}, {body, "hello"}]}], F1),
    
    ok.

append_test() ->

    % no applicable slot
    ?assertEqual(empty, fill({append, s1, ""})),
    ?assertEqual([s1], fill([{slot, s1, s1}, {append, s2, foo}])),

    % simple case
    ?assertEqual([[s1, foo]], fill([{slot, s1, s1}, {append, s1, foo}])),
    ?assertEqual([[s1, foo]], fill([{slot, s1, [s1]}, {append, s1, foo}])),

    % append to empty slot
    ?assertEqual([[foo]], fill([{slot, s1}, {append, s1, foo}])),

    % typical html case
    C1 = [{p, "header"},
          {ol, {slot, items, [{li, "item1"}, {li, "item2"}]}},
          {p, "footer"},
          {append, items, [{li, "item3"}, {li, "item4"}]}],
    F1 = fill(C1),
    io:format("~p", [F1]),
    ?assertEqual([{p, "header"},
                  {ol, [{li,"item1"},
                        {li,"item2"},
                        {li,"item3"},
                        {li,"item4"}]},
                  {p,"footer"}], F1),
    
    ok.

prepend_test() ->

    % no applicable slot
    ?assertEqual(empty, fill({prepend, s1, ""})),
    ?assertEqual([s1], fill([{slot, s1, s1}, {prepend, s2, foo}])),

    % simple case
    ?assertEqual([[foo, s1]], fill([{slot, s1, s1}, {prepend, s1, foo}])),
    ?assertEqual([[foo, s1]], fill([{slot, s1, [s1]}, {prepend, s1, foo}])),

    % typical html case
    C1 = [{p, "header"},
          {ol, {slot, items, [{li, "item3"}, {li, "item4"}]}},
          {p, "footer"},
          {prepend, items, [{li, "item1"}, {li, "item2"}]}],
    F1 = fill(C1),
    io:format("~p", [F1]),
    ?assertEqual([{p, "header"},
                  {ol, [{li,"item1"},
                        {li,"item2"},
                        {li,"item3"},
                        {li,"item4"}]},
                  {p,"footer"}], F1),
    
    ok.

multi_op_test() ->

    Ops1 = [{append, s1, a},
            {append, s1, b},
            {prepend, s1, c},
            {append, s2, d},
            {replace, s2, e}],

    ?assertEqual([], fill(Ops1)),
    ?assertEqual([foo], fill([foo|Ops1])),

    % op order is preserved
    ?assertEqual([[c, a, b]], fill([{slot, s1}|Ops1])),
    ?assertEqual([[c, default, a, b]], fill([{slot, s1, default}|Ops1])),
    ?assertEqual([e], fill([{slot, s2}|Ops1])),
    ?assertEqual([[c, a, b], e, default],
                 fill([{slot, s1}, {slot, s2}, {slot, s3, default}|Ops1])),

    % ops in their own list leave an orphaned list - this is okay - we don't
    % want to put too much effort into "cleaning" the filled output
    ?assertEqual([foo, []], fill([{slot, s1}, [{replace, s1, foo}]])),

    % html example
    C1 = {html,
          [{head,
            [{title, {slot, title, "Colors Template"}},
             {slot, css},
             {slot, js}]},
           {body,
            {p, "My favorite colors:",
             {ul, [{class, "colors"}], {slot, colors}}}}]},
    
    P1 = [C1, [{replace, title, "My Colors"},
               {append, css, {link, [{rel, "styles"}, {href, "a.css"}]}},
               {append, colors, {li, "Green"}},
               {append, colors, {li, "Blue"}},
               {prepend, colors, {li, "Red"}}]],
    F1 = fill(P1),
    io:format("~p", [F1]),
    ?assertEqual([{html,
                   [{head,
                     [{title, "My Colors"},
                      [{link,[{rel,"styles"},{href,"a.css"}]}]]},
                    {body,{p,"My favorite colors:",
                           {ul,[{class,"colors"}],
                            [{li,"Red"},{li,"Green"},{li,"Blue"}]}}}]},
                  []], F1),
    
    ok.

slots_in_slots_test() ->
    C1 = {slot, a, [foo, {slot, b, bar}, baz]},
    ?assertEqual([foo, bar, baz], fill(C1)),
    ?assertEqual([[foo, bar2, baz]], fill([C1, {replace, b, bar2}])),
    ?assertEqual([[foo, bar2, baz, bam]],
                 fill([C1, {replace, b, bar2}, {append, a, bam}])),
    ok.

html_test() ->
    C1 = {p, ["Click ", {a, [{href, "/link"}], "here"}, " when done"]},
    ?assertEqual(C1, shark_slots:fill(C1)),
    ok.
