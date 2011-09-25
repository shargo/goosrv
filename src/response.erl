-module(response).
-export([format/2]).
-export([reply/1]).


format_single([], Rest) -> {"", Rest};
format_single([[[L1|L2]|H1]|T1], [H2|T2]) ->
	{Nested, Rest} = format_single(H1, [H2|T2]),
	{Outer, Ret} =  format_single(T1, Rest),
	{[L1|L2] ++ ": {" ++ Nested ++ "}" ++ field_delim(T1) ++ Outer, Ret};
format_single([H1|T1], [H2|T2]) ->
	{Cont, Rest} = format_single(T1, T2),
	{H1 ++ io_lib:format(": ~p", [H2]) ++ field_delim(T1) ++ Cont, Rest}.


format_hlp(_, [], Acc, Num) -> {Acc, Num};
format_hlp(L, [H|T], Acc, Num) ->
	{Single, _} =  format_single(L, H),
	format_hlp(L, T, lists:flatten("{" ++ Single ++ "}" ++ record_delim(Num) ++ Acc), Num + 1).

reply(L) -> io_lib:format("{reply: ~p}", [L]).

format([H|T], Data) ->
	{Res, Num} = format_hlp(T, Data, "]}", 0),
	"{" ++ H ++ ", num: " ++ io_lib:format("~p, data: [", [Num]) ++ Res.



field_delim([]) -> "";
field_delim([_|_]) -> ", ".

record_delim(0) -> "";
record_delim(_) -> ", ".
