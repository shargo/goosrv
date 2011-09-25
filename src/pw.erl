-module(pw).
% physical world module.
%contains helper functions related to space, time

-export([get_location_range/3, compromise_distance/1]).

% gps and distances
get_location_range(Lat, Lon, D) ->
	{LatMeters, LonMeters} = get_distances(deg2rad(Lat)),
%	{get_positive_range(Lat, D / LatMeters), get_positive_range(Lon, D / LonMeters)}.
	{get_range(Lat, D / LatMeters), get_range(Lon, D / LonMeters)}.

%get_positive_range(B, D) ->
%	{Min, Max} = get_range(B, D),
%	case (Min > Max) of
%		true -> {Max, Min};
%		false -> {Min, Max}
%	end.

get_range(B, D) -> {B - D, B + D}.

compromise_distance(D) -> D * 0.8.

%nmea2deg(GpsCoord) ->
%	Deg = trunc(GpsCoord) div 100,
%	Min = GpsCoord - Deg * 100,
%	Deg + Min / 60.

deg2rad(Deg) -> Deg * math:pi() / 180.

get_distances(Lat) ->
	{M1, M2, M3, M4} = {111132.92, -559.82, 1.175, -0.0023},
	{P1, P2, P3} = {111412.84, -93.5, 0.118},
	LatLen = M1 + (M2 * math:cos(2*Lat)) + (M3 * math:cos(4*Lat)) + (M4 * math:cos(6*Lat)),
	LonLen = (P1 * math:cos(Lat)) + (P2 * math:cos(3*Lat)) + (P3 * math:cos(5*Lat)),
	{LatLen, LonLen}.


