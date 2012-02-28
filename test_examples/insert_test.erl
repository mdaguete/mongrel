-module(insert_test).

-export([test/0]).

-include("mongrel_macros.hrl").

% Our "domain objects" are a book, an author and a review
-record(book, {'_id', title, isbn, author, reviews}).
-record(author, {'_id', first_name, last_name}).
-record(review, {star_rating, comment}).

test() ->
	application:start(mongodb),
	application:start(mongrel),
	
	% For mongrel to work, we need to specify how to map books, authors and reviews.
	mongrel_mapper:add_mapping(?mapping(book)),
	mongrel_mapper:add_mapping(?mapping(author)),
	mongrel_mapper:add_mapping(?mapping(review)),
	
	% Create a sample author and a book record with two reviews.
	% Notice we use a macro for assigning an _id
	Author = #author{?id(), last_name= <<"Tolstoy">>},
	Book = #book{?id(), title= <<"War and Peace 2">>, author=Author},
	BookWithReviews = Book#book{reviews = [#review{star_rating=1, comment= <<"Turgid old nonsense">>}, 
										   #review{star_rating=5}]},
	
	% Connect to MongoDB
	Host = {localhost, 27017},
	{ok, Conn} = mongo:connect(Host),
	
	% Use the mongrel insert function and the MongoDB driver to write the book record to the database
	{ok, _} = mongo:do(safe, master, Conn, mongrel_test, fun() ->
													   mongrel:insert(BookWithReviews)
			 end),
	
	mongo:do(safe, master, Conn, mongrel_test, fun() ->
													   mongrel:find_one(#book{title= {'$ne', <<"War and Peace">>}})
			 end).

