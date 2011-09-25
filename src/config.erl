-module(config).
-export([init/0]).
-export([save/1, lookup/1]).
-export([array_data/1, array_labels/0]).

% db exports
init() ->
	srvdb:init_tbl(config).

save({Key, Value}) ->
%FIXME extract repeating events
	srvdb:store(config, {Key, Value}).

lookup(Key) ->
	Ret = srvdb:lookup(config, [srvdb:op_eq(Key)]),
	case Ret of
	[] -> none;
	[H|T] ->
		case T of
			[] -> H;
			_ -> Ret
		end
	end.

% ui exports
array_data(Value) ->
	[Value].

array_labels() ->
	["value"].

