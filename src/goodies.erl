-module(goodies).
-export([init/0]).
-export([save/1, lookup/1]).
-export([array_data/1, array_labels/0]).
-export([empty/0,set_constraint/3]).

init() ->
	srvdb:init_tbl(goodies).

save(Goodie) ->
%FIXME extract repeating events
	srvdb:store(goodies, Goodie).

lookup(Goodie) ->
	srvdb:lookup(goodies, Goodie).

array_data(G) ->
	{UserId, Category, Lat, Lon, Begin, End, Description} = G,
	[UserId, Category, Lat, Lon, Begin, End, Description].

array_labels() ->
	["goodies", "userid", "category", ["location", "lat", "lon"], ["time", "begin", "end"], "description"].

empty() ->
	N = srvdb:op_nop(),
	{N,N,N,N,N,N,N}.

set_constraint(uid, G, Uid) ->
	{_, Cat, Lat, Lon, TimeBegin, TimeEnd, Desc} = G,
	{srvdb:op_eq(Uid), Cat, Lat, Lon, TimeBegin, TimeEnd, Desc};
set_constraint(cat, G, {CatMin, CatMax}) ->
	{UserId, _, Lat, Lon, TimeBegin, TimeEnd, Description} = G,
	case CatMin == CatMax of
		true -> {UserId, srvdb:op_eq(CatMin), Lat, Lon, TimeBegin, TimeEnd, Description};
		false -> {UserId, srvdb:op_inrange(CatMin, CatMax), Lat, Lon, TimeBegin, TimeEnd, Description}
	end;
set_constraint(loc, G, {{Lat, Lon}, Dist}) ->
	{UserId, Category, _, _, TimeBegin, TimeEnd, Description} = G,
	{{LatMin, LatMax}, {LonMin, LonMax}} = pw:get_location_range(Lat, Lon, Dist),
	{UserId, Category,
			srvdb:op_and(srvdb:op_greq(LatMin),srvdb:op_leq(LatMax)),
			srvdb:op_and(srvdb:op_greq(LonMin),srvdb:op_leq(LonMax))
		, TimeBegin, TimeEnd, Description};
set_constraint(tim, G, {Begin, End}) ->
	{UserId, Category, Lat, Lon, _, _, Description} = G,
	{UserId, Category, Lat, Lon,
			srvdb:op_le(End),
			srvdb:op_gr(Begin)
		, Description}.
