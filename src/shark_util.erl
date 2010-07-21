-module(shark_util).

-export([escape/1, unescape/1, capitalize_words/1]).

-define(alphanum(C), ((C >= $a andalso C =< $z) orelse
                      (C >= $A andalso C =< $Z) orelse
                      (C >= $0 andalso C =< $9))).

-define(cookie_safe(C), ?alphanum(C) orelse (C =:= $_ orelse C =:= $. orelse
                                             C =:= $- orelse C =:= $/ orelse
                                             C =:= $@ orelse C =:= $* orelse
                                             C =:= $+)).

%%
%% @doc Escaped a string values.
%%
%% The input requires a tuple that includes a token identifying the type of
%% string to escape.
%% @end
%% @spec escape({cookie, S}) -> string().
%%
escape({cookie, S}) ->
    escape({cookie, S}, []).

escape({_, []}, Acc) ->
    lists:reverse(Acc);

%% Safe chars are passed through.
escape({cookie, [C|Rest]}, Acc) when ?cookie_safe(C) ->
    escape({cookie, Rest}, [C|Acc]);

%% Everything else is escaped.
escape({Type, [C|Rest]}, Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    escape({Type, Rest}, [hexdigit(Lo), hexdigit(Hi), $%|Acc]).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

%%
%% @doc Unescapes a value. Uses the same pattern as escape.
%%
unescape({_, S}) ->
    mochiweb_util:unquote(S).

capitalize_words(Words) ->
    lists:map(fun capitalize_word/1, Words).

capitalize_word([]) ->
    [];
capitalize_word([First|Rest]) ->
    [string:to_upper(First)|Rest].
