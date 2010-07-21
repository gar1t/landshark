-module(shark_html).

-export([render/1, quote_attr/1]).

-define(lt, <<"<">>).
-define(ltslash, <<"</">>).
-define(gt, <<">">>).
-define(lf, <<"\n">>).
-define(eq, <<"=">>).
-define(quot, <<"\"">>).
-define(space, <<" ">>).

-define(is_empty(Tag), Tag =:= img orelse
	               Tag =:= input orelse
                       Tag =:= br orelse
                       Tag =:= hr orelse
                       Tag =:= meta orelse
                       Tag =:= link).

-define(is_other(V), is_atom(V) orelse is_binary(V) orelse is_list(V)).

%%
%% @doc Tries to convert a value to a binary.
%%
to_binary(B) when is_binary(B) -> B;
to_binary(A) when is_atom(A)   -> to_binary(atom_to_list(A));
to_binary(L) when is_list(L)   -> unicode:characters_to_binary(L).

%%
%% @doc Returns the LF binary for the specified opening tag.
%%
%% This is to provide some line feeds for typical block tags.
%%
open_tag_lf(html)     -> ?lf;
open_tag_lf(head)     -> ?lf;
open_tag_lf(body)     -> ?lf;
open_tag_lf('div')    -> ?lf;
open_tag_lf(ol)       -> ?lf;
open_tag_lf(ul)       -> ?lf;
open_tag_lf(form)     -> ?lf;
open_tag_lf(fieldset) -> ?lf;
open_tag_lf(table)    -> ?lf;
open_tag_lf(_)        -> <<>>.

%%
%% @doc Returns the LF binary for the specified closing tag.
%%
%% This is to render inline tags such as <img>, <em>, etc.
%%
close_tag_lf(a)       -> <<>>;
close_tag_lf(abbr)    -> <<>>;
close_tag_lf(acronym) -> <<>>;
close_tag_lf(b)       -> <<>>;
close_tag_lf(big)     -> <<>>;
close_tag_lf(del)     -> <<>>;
close_tag_lf(em)      -> <<>>;
close_tag_lf(i)       -> <<>>;
close_tag_lf(img)     -> <<>>;
close_tag_lf(ins)     -> <<>>;
close_tag_lf(kbd)     -> <<>>;
close_tag_lf(span)    -> <<>>;
close_tag_lf(strike)  -> <<>>;
close_tag_lf(strong)  -> <<>>;
close_tag_lf(sub)     -> <<>>;
close_tag_lf(sup)     -> <<>>;
close_tag_lf(u)       -> <<>>;
close_tag_lf(var)     -> <<>>;
close_tag_lf(_)       -> ?lf.

%%
%% @doc Renders the specified content.
%%

%% Special tag indicating nothing.

render(empty) ->
    [];

%% Explicit empty tags - use an atom to mark the content.

render({Tag, empty}) ->
    [?lt, to_binary(Tag), ?gt, close_tag_lf(Tag)];

render({Tag, Attrs, empty}) ->
    [?lt, to_binary(Tag), render_attrs(Attrs), ?gt, close_tag_lf(Tag)];

%% Special empty tags.

render({Tag, Attrs}) when ?is_empty(Tag) ->
    render({Tag, Attrs, empty});

render({Tag}) when ?is_empty(Tag) ->
    render({Tag, [], empty});

render(Tag) when ?is_empty(Tag) ->
    render({Tag, [], empty});

%% Main implementation of tuples: Tag, Attrs, Content

render({Tag}) ->
    render({Tag, []});

render({Tag, Content}) ->
    TagAsBinary = to_binary(Tag),
    [?lt, TagAsBinary, ?gt, open_tag_lf(Tag), render(Content), ?ltslash,
     TagAsBinary, ?gt, close_tag_lf(Tag)];
    
render({Tag, Attrs, Content}) ->
    TagAsBinary = to_binary(Tag),
    [?lt, TagAsBinary, render_attrs(Attrs), ?gt, open_tag_lf(Tag),
     render(Content), ?ltslash, TagAsBinary, ?gt, close_tag_lf(Tag)];

%% Implementation of non-string lists

render([{Tag, Content}|[]]) ->
    render({Tag, Content});

render([{Tag, Content}|Rest]) ->
    [render({Tag, Content}), render(Rest)];
    
render([{Tag, Attrs, Content}|[]]) ->
    render({Tag, Attrs, Content});

render([{Tag, Attrs, Content}|Rest]) ->
    [render({Tag, Attrs, Content}), render(Rest)];

render([Other|[]]) when ?is_other(Other) ->
    render(Other);

render([Other|Rest]) when ?is_other(Other) ->
    [render(Other), render(Rest)];

%% Other: atoms, strings, and binary

render(Other) ->
    to_binary(Other).

%%
%% @doc Renders zero of more attributes.
%%
render_attrs(Attrs) ->
    render_attrs(proplists:unfold(Attrs), []).

render_attrs([], Acc) ->
    lists:reverse(Acc);
render_attrs([{_Name, undefined}|Rest], Acc) ->
    render_attrs(Rest, Acc);
render_attrs([{Name, true}|Rest], Acc) ->
    render_attrs(Rest, [to_binary(Name), ?space|Acc]);
render_attrs([{Name, Value}|Rest], Acc) ->
    render_attrs(Rest, [quote_attr(Value), ?eq, to_binary(Name), ?space|Acc]).

%%
%% @doc Quotes an attribute value.
%%
quote_attr(Value) when is_atom(Value) ->
    quote_attr(atom_to_list(Value));
quote_attr(Value) when is_integer(Value) ->
    quote_attr(integer_to_list(Value));
quote_attr(Value) ->
    quote_attr(Value, []).

%%
%% @doc Quotes a value and performs the substitution of &, <, >, and ".
%%
quote_attr([], Acc) ->
    [?quot, to_binary(lists:reverse(Acc)), ?quot];
quote_attr([$&|Rest], Acc) ->
    quote_attr(Rest, ["&amp;"|Acc]);
quote_attr([$<|Rest], Acc) ->
    quote_attr(Rest, ["&lt;"|Acc]);
quote_attr([$>|Rest], Acc) ->
    quote_attr(Rest, ["&gt;"|Acc]);
quote_attr([$"|Rest], Acc) ->
    quote_attr(Rest, ["&quot;"|Acc]);
quote_attr([C|Rest], Acc) ->
    quote_attr(Rest, [C|Acc]).
