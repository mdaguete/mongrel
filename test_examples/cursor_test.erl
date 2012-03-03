-module(cursor_test).

-include("mongrel_macros.hrl").

-export([test/0]).

-record(coords, {'_id', x, y, desc}).
-record(str, {'_id', value}).

insert(0) ->
	ok;
insert(Counter) ->
	mongrel:insert(#coords{?id(), x=(Counter rem 10), y=Counter, desc=#str{?id(), value= <<"Coordinates">>}}),
	insert(Counter-1).

count(Cursor, Count) ->
	case mongrel_cursor:next(Cursor) of
		{} ->
			Count;
		_ ->
			count(Cursor, Count+1)
	end.

test() ->
	application:start(mongodb),
	application:start(mongrel),
	{ok, Connection} = mongo:connect(localhost),
	mongrel_mapper:add_mapping(?mapping(coords)),
	mongrel_mapper:add_mapping(?mapping(str)),
	mongrel:do(safe, master, Connection, cursor_test, 
			   fun() ->
					   mongrel:delete(#coords{}),
					   insert(100000),
					   Cursor = mongrel:find(#coords{x=7}, #coords{desc=0}, 0, 100),
					   count(Cursor, 0)
			   end).

															 