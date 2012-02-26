-module(insert_test).

-export([test/0]).

-include("mongrel_macros.hrl").

-record(book, {'_id', title, isbn, author, reviews}).
-record(author, {'_id', first_name, last_name}).
-record(review, {star_rating, comment}).

test() ->
	mongrel_mapper:add_mapping(?mapping(book)),
	mongrel_mapper:add_mapping(?mapping(author)),
	mongrel_mapper:add_mapping(?mapping(review)),
	Author = #author{?id(), last_name= <<"Tolstoy">>},
	Book = #book{?id(), title= <<"War and Peace">>, author=Author},
	BookWithReviews = Book#book{reviews = [#review{star_rating=1, comment= <<"Turgid old nonsense">>}, #review{star_rating=5}]},
	Host = {localhost, 27017},
	{ok, Conn} = mongo:connect(Host),
	InsertBookFun = fun() ->
							mongrel:insert(BookWithReviews)
					end,
	mongo:do(safe, master, Conn, mongrel_test, InsertBookFun). 
