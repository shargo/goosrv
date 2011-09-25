-module(hlp).
-export([a2i/1, a2f/1, in_range/3]).

a2i(S) -> {I, _} = string:to_integer(S), case I of error -> 0; _ -> I end.
a2f(S) -> {F, _} = string:to_float(S), case F of error -> a2i(S); _ -> F end.

in_range(X,Min,Max) -> not((Min > X) orelse (X > Max)).
