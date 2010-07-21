-module(slots).

-export([app/1]).

page() ->
    {html,
     [{head, {title, {slot, title, "Default Title"}}},
     {body,
      [{'div', [{class, hd}], {slot, header, "Default Header"}},
       {'div', [{class, bd}], {slot, body, "Default Body"}},
       {'div', [{class, ft}], {slot, footer, "Default Footer"}}
      ]},
      {replace, title, "Updated Title"},
      {replace, body, "Updated Body"}]}.

app(_Env) ->
    shark_response:ok({slots_html, page()}).
