-module(shark_slots).

-export([fill/1]).

-define(slot_op(Op), Op =:= replace orelse
                     Op =:= append orelse
                     Op =:= prepend).

%%
%% @doc Fills any slots specified in Content with any applicable slot
%% operations.
%%
fill(Content) ->
    {C, Ops} = split(Content),
    fill(C, Ops).

%%
%% @doc Returns {C2, Ops} where C2 is C with all slot operations (e.g. append,
%% prepend, etc.) removed and Ops is a dict of slot operations keyed by slot
%% ID.
%%
split(C) ->
    split(C, dict:new(), empty).

split({Op, Id, C}, Ops, Out) when ?slot_op(Op) ->
    {Out, add_slot_op(Id, {Op, C}, Ops)};

split({T, C}, Ops, Out) ->
    {C2, Ops2} = split(C, Ops, empty),
    {cjoin({T, C2}, Out), Ops2};

split({T, A, C}, Ops, Out) ->
    {C2, Ops2} = split(C, Ops, empty),
    {cjoin({T, A, C2}, Out), Ops2};

split([H|T], Ops, Out) when not is_integer(H) ->
    {OutT, Ops2} = split(T, Ops, Out),
    {OutH, Ops3} = split(H, Ops2, empty),
    {ccons(OutH, OutT), Ops3};

split(Other, Ops, Out) ->
    {cjoin(Other, Out), Ops}.

%%
%% @doc Adds slot operation {Op, Content} to a list of ops keyed by Id. Returns
%% the modified ops dict.
%%
add_slot_op(Id, {Op, Content}, Ops) ->
    case dict:is_key(Id, Ops) of
        true -> dict:append(Id, {Op, Content}, Ops);
        false -> dict:store(Id, [{Op, Content}], Ops)
    end.

%%
%% @doc Fills content C using the operations in Ops.
%%
fill(C, Ops) ->
    fill(C, Ops, empty).

fill({slot, Id}, Ops, Out) ->
    fill({slot, Id, empty}, Ops, Out);

fill({slot, Id, Default}, Ops, Out) ->
    case dict:find(Id, Ops) of
	error ->
	    fill(Default, Ops, Out);
	{ok, SlotOps} ->
	    cjoin(apply_ops(lists:reverse(SlotOps), Default, Ops), Out)
    end;

fill({T, C}, Ops, Out) ->
    cjoin({T, fill(C, Ops, empty)}, Out);

fill({T, A, C}, Ops, Out) ->
    cjoin({T, A, fill(C, Ops, empty)}, Out);

fill([H|T], Ops, Out) when not is_integer(H) ->
    ccons(fill(H, Ops, empty), fill(T, Ops, Out));

fill(Other, _Ops, Out) ->
    cjoin(Other, Out).

%%
%% @doc Applies operations SlotOps to content Slot. Ops is provided for filling
%% slots in default content.
%%
apply_ops([], Slot, _Ops) ->
    Slot;

apply_ops([{replace, C}|T], _Slot, Ops) ->
    C2 = fill(C, Ops, empty),
    apply_ops(T, C2, Ops);

apply_ops([{append, C}|T], Slot, Ops) ->
    C2 = fill(C, Ops, empty),
    Slot2 = fill(Slot, Ops, empty),
    apply_ops(T, cappend(C2, Slot2), Ops);

apply_ops([{prepend, C}|T], Slot, Ops) ->
    C2 = fill(C, Ops, empty),
    Slot2 = fill(Slot, Ops, empty),
    apply_ops(T, cprepend(C2, Slot2), Ops).

%%
%% @doc Joins two pieces of content in a list. If either part is empty, the
%% other is returned.
%%
cjoin(empty, C)  -> C;
cjoin(C, empty)  -> C;
cjoin(C1, C2)    -> [C1, C2].

%%
%% @doc Joins via cons H and a list T. If H is empty, returns T. T must be a
%% list.
%%
ccons(empty, T)  -> T;
ccons(H, T)      -> [H|T].

%%
%% @doc Appends C1 to C2. If C2 is empty, returns C1.
%%
cappend(C1, C2) ->
    lists:append(to_list(C2), to_list(C1)).

%%
%% @doc Prepends C1 to C2. If C2 is empty, returns C1.
%%
cprepend(C1, C2) ->
    lists:append(to_list(C1), to_list(C2)).
    
to_list(empty) -> [];
to_list([H|_T]=C) when not is_integer(H) -> C;
to_list(C) -> [C].

-ifdef(TEST).
-include("shark_slots.hrl").
-endif.
