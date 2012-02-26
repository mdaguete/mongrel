-module(insert_test).

-export([test/0]).

-include("mongrel_macros.hrl").

-record(book, {'_id', title, isbn, author}).
-record(author, {'_id', first_name, last_name}).

test() ->
	mongrel_mapper:add_mapping(?mapping(book)),
	mongrel_mapper:add_mapping(?mapping(author)),
	Author = #author{?id(), last_name= <<"Tolstoy">>},
	Book = #book{?id(), title= <<"War and Peace">>, author=Author},
	Host = {localhost, 27017},
	{ok, Conn} = mongo:connect(Host),
	InsertBookFun = fun() ->
							mongrel:insert(Book)
					end,
	mongo:do(safe, master, Conn, mongrel_test, InsertBookFun). 
