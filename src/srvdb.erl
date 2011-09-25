-module(srvdb).
-export([init/0]).
-export([init_tbl/1, store/2, lookup/3, lookup/2]).
-export([op_eq/1, op_neq/1]).
-export([op_gr/1, op_greq/1]).
-export([op_le/1, op_leq/1]).
-export([op_inrange/2, op_notinrange/2]).
-export([op_nop/0, is_op_nop/1]).
-export([op_and/2, op_or/2, op_not/1]).


init() -> ok.

init_tbl(Tbl) ->
	TN = get_tbl_name(Tbl),
	case Tbl of
		goodies -> ets:new(TN, [bag, public, named_table]);
		categories -> ets:new(TN, [set, public, named_table]);
		config -> ets:new(TN, [set, public, named_table])
	end.

store(T, D) ->
	ets:insert(get_tbl_name(T), D).

lookup(Tbl, FiltFunc, FiltData) ->
	TN = get_tbl_name(Tbl),
	Res = case Tbl of
		goodies -> ets:match(TN, {'$0','$1','$2','$3','$4','$5','$6'});
		categories -> ets:match(TN, {'$0', '$1','$2','$3','$4'});
		config -> ets:match(TN, {'$0','$1'})
	end,
	filter_out(Res, FiltFunc, FiltData, []).

lookup(Tbl, FiltData) ->
	lookup(Tbl, fun(_,_) -> true end, FiltData).

op_eq(V1) -> {{eq, false}, V1, 0}.
op_neq(V1) -> op_not(op_eq(V1)).
op_gr(V1) -> {{gr, false}, V1, 0}.
op_greq(V1) -> op_or(op_gr(V1), op_eq(V1)).
op_leq(V1) -> op_not(op_gr(V1)).
op_le(V1) -> op_not(op_greq(V1)).
op_or(V1,V2) -> {{l1_or, false}, V1, V2}.
op_and(V1,V2) -> {{l1_and, false}, V1, V2}.
op_inrange(V1, V2) -> {{inrange, false}, V1, V2}.
%op_inrange(V1, V2) -> op_and(op_greq(V1), op_leq(V2)).
op_notinrange(V1, V2) -> op_not(op_inrange(V1, V2)).
op_nop() -> {{nop, false}, 0, 0}.
is_op_nop({{nop,_}, _, _}) -> true;
is_op_nop(_) -> false.
op_not({{Op, C}, V1, V2}) -> {{Op, not C}, V1, V2}.


get_tbl_name(Tbl) ->
	Tbl.

do_op({Op, Neg}, R1, R2, R3) ->
	Res =case Op of
		nop -> true;
		eq -> R1 == R2;
		gr -> R1 > R2;
		inrange -> hlp:in_range(R1, R2, R3);
		l1_and ->
			{O1_l1, R2_l1, R3_l1} = R2,
			{O1_l2, R2_l2, R3_l2} = R3,
			do_op(O1_l1, R1, R2_l1, R3_l1) andalso
			do_op(O1_l2, R1, R2_l2, R3_l2);
		l1_or ->
			{O1_l1, R2_l1, R3_l1} = R2,
			{O1_l2, R2_l2, R3_l2} = R3,
			do_op(O1_l1, R1, R2_l1, R3_l1) orelse
			do_op(O1_l2, R1, R2_l2, R3_l2)

	end,
	case Neg of
		true -> not Res;
		false -> Res
	end.

run_known_filters([],_) -> true;
run_known_filters(_,[]) -> true;
run_known_filters([LH|LT],[RH|RT]) ->
	{Action, V1, V2} = RH,
	do_op(Action, LH, V1, V2) andalso run_known_filters(LT,RT).


shortcut_filter(Tmp) ->
	lists:reverse(lists:dropwhile(fun(X) -> is_op_nop(X) end, lists:reverse(Tmp))).

filter_out([], _, _, Acc) ->
	Acc;
filter_out([H|T], FiltFunc, FiltData, Acc) ->
	ShFi = shortcut_filter(FiltData),
	NewAcc = case run_known_filters(H, ShFi) andalso FiltFunc(H, ShFi) of
		true -> [H|Acc];
		false -> Acc
	end,
	filter_out(T, FiltFunc, ShFi, NewAcc).

