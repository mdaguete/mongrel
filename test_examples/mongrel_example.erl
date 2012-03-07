-module(mongrel_example).

-export([populate/0]).

-include("mongrel_macros.hrl").

% Our "domain objects" are books, authors and reviews
-record(book, {'_id', title, isbn, author, reviews}).
-record(author, {'_id', first_name, last_name}).
-record(review, {star_rating, comment}).

populate() ->
	application:start(mongodb),
	application:start(mongrel),
	
	% For mongrel to work, we need to specify how to map books, authors and reviews.
	mongrel_mapper:add_mapping(?mapping(book)),
	mongrel_mapper:add_mapping(?mapping(author)),
	mongrel_mapper:add_mapping(?mapping(review)),
	
	% Create some books, authors and reviews.
	Author1 = #author{?id(), last_name= <<"Eliott">>},
	Book1 = #book{?id(), title= <<"Thirty Days in the Samarkind Desert with the Duchess of Kent">>, author=Author1},
	
	Review2 = #review{star_rating = 5, comment = <<"By an Irish Gentleman whose name eludes me">>},
	Book2 = #book{?id(),  title = <<"A Hundred and One Ways to start a Fight">>, reviews=[Review2]},
	
	{ok, Connection} = mongo:connect(localhost),
	mongrel:do(safe, master, Connection, mongrel_books, 
			   fun() ->
					   mongrel:delete(#author{}),
					   mongrel:delete(#book{}),
					   %mongrel:insert(Book1),
					   mongrel:insert_all([Book1, Book2]),
					   mongrel:find(#book{})
			   end).
