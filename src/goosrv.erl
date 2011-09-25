-module(goosrv).
-export([start/1]).
-export([start/0]).


% api
start(Port) ->
	srvdb:init(),
	config:init(),
	categories:init(),
	goodies:init(),
	misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req) end}]),
	ok.

start() -> start(9999).


%helpers
get_fields([], _, Acc) -> lists:reverse(Acc);
get_fields([Name | Rest], Args, Acc) ->
	get_fields(Rest, Args, [misultin_utility:get_key_value(Name, Args) | Acc]).
get_fields(Names, Args) -> get_fields(Names, Args, []).


% http request handlers
handle_http(Req) ->
	Ret = handle_req(Req:resource([lowercase, urlencode]), Req:parse_qs()),
	Req:ok([{"Content-Type", "application/json"}],
		lists:flatten(Ret)) .

handle_req(["provide"], Args) ->
	[UserId, Cat, Lat, Lon, EpochStart, EpochEnd, Description] =
		get_fields(["userid", "category", "lat", "lon",
		"epoch_start", "epoch_end", "desc"], Args),
	goodies:save({hlp:a2i(UserId), hlp:a2i(Cat),
		hlp:a2f(Lat), hlp:a2f(Lon),hlp:a2i(EpochStart),
		hlp:a2i(EpochEnd), Description}),
	response:reply("ok");
handle_req(["list", UserId], _Args) ->
	G0 = goodies:empty(),
	G1 = goodies:set_constraint(uid, G0, hlp:a2i(UserId)),
	response:format(goodies:array_labels(), goodies:lookup(goodies:array_data(G1)));
handle_req(["wish"], Args) ->
	[_UserId, Category, EpochStart, EpochEnd, Distance, Lat, Lon] =
		get_fields(["userid", "category",  "epoch_start", "epoch_end", "distance",
		"lat", "lon"], Args),
	D = pw:compromise_distance(hlp:a2i(Distance)),
	CatMin = hlp:a2i(Category),
	C0 = categories:empty(),
	C1 = categories:set_constraint(ind, C0, CatMin),
	C2 = categories:set_constraint(ver, C1, categories:version()),
	CatRow = categories:lookup(categories:array_data(C2)),
	CatMax = case CatRow of
		[SingleCatRow] -> categories:get_maxsub(SingleCatRow);
		_ -> 0
	end,
	G0 = goodies:empty(),
	G1 = goodies:set_constraint(cat, G0, {CatMin, CatMax}),
	G2 = goodies:set_constraint(tim, G1, {hlp:a2i(EpochStart), hlp:a2i(EpochEnd)}),
	G3 = goodies:set_constraint(loc, G2, {{hlp:a2f(Lat), hlp:a2f(Lon)}, D}),
	response:format(goodies:array_labels(), goodies:lookup(goodies:array_data(G3)));
handle_req(["categories", "setcurrentversion", Version], _Args) ->
	categories:set_version(hlp:a2i(Version)),
	response:reply(categories:version());
handle_req(["categories", "dump"], _Args) ->
	response:format(categories:array_labels(),categories:lookup(categories:array_data(categories:empty())));
handle_req(["categories", "set", Version, Index, Parent, Value, MaxSub], _Args) ->
	categories:save({hlp:a2i(Index), hlp:a2i(Parent), list_to_binary(Value),
		hlp:a2i(Version), hlp:a2i(MaxSub)}),
	response:reply("ok");

% debug
handle_req(["gpspos2distances", GpsLatStr, GpsLonStr, Distance], _Args) ->
	D =  pw:compromise_distance(hlp:a2i(Distance)),
	Lat = pw:gps2deg(hlp:a2f(GpsLatStr)),
	Lon = pw:gps2deg(hlp:a2f(GpsLonStr)),
	{{LatMin, LatMax}, {LonMin, LonMax}} = pw:get_location_range(Lat, Lon, D),
	lists:flatten(io_lib:format("real position ~p\nranges for ~p meters\nlat range ~p\nlon range ~p\n",
		[{Lat, Lon}, D, {LatMin, LatMax}, {LonMin, LonMax}]));
handle_req(["pos2distances", GpsLatStr, GpsLonStr, Distance], _Args) ->
	D =  pw:compromise_distance(hlp:a2i(Distance)),
	Lat = hlp:a2f(GpsLatStr),
	Lon = hlp:a2f(GpsLonStr),
	{{LatMin, LatMax}, {LonMin, LonMax}} = pw:get_location_range(Lat, Lon, D),
	lists:flatten(io_lib:format("real position ~p\nranges for ~p meters\nlat range ~p\nlon range ~p\n",
		[{Lat, Lon}, D, {LatMin, LatMax}, {LonMin, LonMax}]));
% 404
handle_req(_, _Args) ->
	"404\n".
% categories db

