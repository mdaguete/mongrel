% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(book_database).
-export([add_mappings/0, populate/0, get_all/0]).
-include_lib("mongrel/include/mongrel_macros.hrl").

% Our "domain objects" are books, authors and reviews
-record(book, {'_id', title, isbn, author, reviews}).
-record(author, {'_id', first_name, last_name}).
-record(review, {star_rating, comment}).

add_mappings() ->
	% For mongrel to work, we need to specify how to map books, authors and reviews.
	% If you hate typing, you can use the even shorter macro expansions:
	% ?add_mapping(book), ?add_mapping(author) and ?add_mapping(review)
	mongrel_mapper:add_mapping(?mapping(book)),
	mongrel_mapper:add_mapping(?mapping(author)),
	mongrel_mapper:add_mapping(?mapping(review)).
	
populate() ->
	% Create some books, authors and reviews.
	Author1 = #author{?id(), last_name = <<"Eliott">>},
	Book1 = #book{?id(), title = <<"Thirty Days in the Samarkind Desert with the Duchess of Kent">>, author = Author1},
	Review2 = #review{star_rating = 5, comment = <<"By an Irish gentleman whose name eludes me">>},
	Book2 = #book{?id(),  title = <<"A Hundred and One Ways to start a Fight">>, reviews = [Review2]},
	Author3 = #author{?id(), first_name = <<"Edmund">>, last_name = <<"Wells">>},
	Book3 = #book{?id(), title = <<"David Copperfield">>, author = Author3},
	Book4 = #book{?id(), title = <<"Grate Expectations">>, author = Author3},
	Author5 = #author{?id(), first_name = <<"Charles">>, last_name = <<"Dikkens">>},
	Book5 = #book{?id(), title = <<"Rarnaby Budge">>, author = Author5},
	Review6a = #review{comment = <<"Warning: Not the expurgated version.">>},
	Review6b = #review{star_rating = 2, comment = <<"Might be interesting to bird-watchers.">>},
	Book6 = #book{?id(), title = <<"Olsen's Standard Book of British Birds">>, reviews = [Review6a, Review6b]},

	{ok, Connection} = mongo:connect(localhost),
	mongrel:do(safe, master, Connection, mongrel_books, 
			   fun() ->
					   mongrel:delete(#author{}),
					   mongrel:delete(#book{}),
					   mongrel:insert_all([Book1, Book2, Book3, Book4, Book5, Book6])
			   end).

get_all() ->
	{ok, Connection} = mongo:connect(localhost),
	mongrel:do(safe, master, Connection, mongrel_books, 
			   fun() ->
					   Cursor = mongrel:find(#book{}),
					   mongrel_cursor:rest(Cursor)
			   end).
