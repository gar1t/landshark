-module(shark_js).

-export([args/1]).

args(Args) ->
    S = lists:flatten(mochijson:encode({array, Args})),
    lists:sublist(S, 2, length(S) - 2).

-ifdef(TEST).
-include("shark_js.hrl").
-endif.

