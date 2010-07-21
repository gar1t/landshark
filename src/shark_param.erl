-module(shark_param).

-export([get/2, get/3, get_all/2, set/3]).

get(Key, Env) ->
    get(Key, Env, undefined).

get({'GET', Key}, Env, Default) ->
    get(params_get, Key, Env, Default);
get({'POST', Key}, Env, Default) ->
    get(params_post, Key, Env, Default);
get(Key, Env, Default) ->
    get(params, Key, Env, Default).

get(Src, Key, Env, Default) ->
    Params = proplists:get_value(Src, Env, []),
    proplists:get_value(key_to_string(Key), Params, Default).

get_all({'GET', Key}, Env) ->
    get_all(params_get, Key, Env);
get_all({'POST', Key}, Env) ->
    get_all(params_post, Key, Env);
get_all(Key, Env) ->
    get_all(params, Key, Env).

get_all(Src, Key, Env) ->
    Params = proplists:get_value(Src, Env, []),
    proplists:get_all_values(key_to_string(Key), Params).

set({'GET', Key}, Value, Env) ->
    set(params_get, Key, Value, Env);
set({'POST', Key}, Value, Env) ->
    set(params_post, Key, Value, Env);
set(Key, Value, Env) ->
    set(params, Key, Value, Env).

set(Src, Key, Value, Env) ->
    Params = proplists:get_value(Src, Env, []),
    [{Src, [{key_to_string(Key), Value}|Params]}|Env].

key_to_string(Key) when is_atom(Key) ->
    atom_to_list(Key);
key_to_string(Key) ->
    Key.
