-module(shark_response).

-export([ok/1,
         ok/2,
         see_other/1,
         see_other/2,
         unauthorized/0,
         not_found/0,
         internal_error/1,
         bad_request/1]).

-define(mime(Type), shark_mime:mime_type(Type)).

default_props({html, _Content}) ->
    [{content_type, html}];
default_props({slots_html, _Content}) ->
    [{content_type, html}];
default_props({json, _Content}) ->
    [{content_type, json}];
default_props({plain, _Content}) ->
    [{content_type, plain}].

headers(Props) ->
    headers(Props, []).

headers([], Acc) ->
    Acc;

headers([{content_type, Type}|T], Acc) when is_atom(Type) ->
    headers([{content_type, ?mime(Type)}|T], Acc);
headers([{content_type, Type}|T], Acc)  ->
    headers(T, [{"Content-Type", Type}|Acc]);
headers([{cookie, {Name, Value}}|T], Acc) ->
    headers([{cookie, {Name, Value, []}}|T], Acc);
headers([{cookie, {Name, Value, Options}}|T], Acc) ->
    Escaped = shark_util:escape({cookie, Value}),
    headers(T, [mochiweb_cookies:cookie(Name, Escaped, Options)|Acc]);

headers([{Name, Value}|T], Acc) ->
    headers(T, [{Name, Value}|Acc]).

body({html, Content}) ->
    shark_html:render(Content);
body({slots_html, Content}) ->
    FilledContent = shark_slots:fill(Content),
    shark_html:render(FilledContent);
body({_Other, Content}) ->
    Content.

ok(Body) ->
    ok(Body, default_props(Body)).

ok(Body, Props) ->
    {200, headers(Props), body(Body)}.

see_other(Path) ->
    see_other(Path, []).

see_other(Path, Props) ->
    {303, [{"Location", Path}|headers(Props)], []}.

bad_request(Details) ->
    error_logger:error_report(Details),
    {400, [], "Bad Request"}.

unauthorized() ->
    {401, [], "Unauthorized"}.

not_found() ->
    {404, [], "Not found"}.

internal_error(Details) ->
    error_logger:error_report(Details),
    {500, [], "Internal Server Error"}.
