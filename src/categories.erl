-module(categories).
-export([init/0]).
-export([save/1, lookup/1]).
-export([set_version/1, version/0]).
-export([get_maxsub/1]).
-export([empty/0]).
-export([set_constraint/3]).
-export([array_data/1]).
-export([array_labels/0]).

init() ->
	srvdb:init_tbl(categories),
	set_version(0).

save({Ind, Parent, Val, Ver, MaxSub}) ->
	srvdb:store(categories,{Ind, Parent, Ver, Val, MaxSub}).

lookup(C) ->
	srvdb:lookup(categories,C).


set_version(V) ->
	config:save({catver, V}).

version() ->
	[catver, V] = config:lookup(catver),
	V.

get_maxsub([_Ind, _Parent, _Ver, _Val, MaxSub]) -> MaxSub.

empty() ->
	N = srvdb:op_nop(),
	{N, N, N, N, N}.

array_labels() ->
	["categories", "index", "parent", "value", "version", "maxsubcategory"].

array_data({Ind, Parent, Ver, Val, MaxSub}) ->
	[Ind, Parent, Ver, Val, MaxSub].

set_constraint(ind, C, Ind) ->
	{_, Parent, Ver, Val, MaxSub} = C,
	{srvdb:op_eq(Ind), Parent, Ver, Val, MaxSub};
set_constraint(ver, C, Ver) ->
	{Ind, Parent, _, Val, MaxSub} = C,
	{Ind, Parent, srvdb:op_eq(Ver), Val, MaxSub};
set_constraint(parent, C, P) ->
	{Ind, _, Ver, Val, MaxSub} = C,
	{Ind, srvdb:op_eq(P), Ver, Val, MaxSub}.


