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

%% This example code is a rough port of the MongoEngine example at 
%% http://mongoengine.org/docs/v0.5/tutorial.html
-module(tumblelog).
-include_lib("mongrel/include/mongrel_macros.hrl").
-export([add_mappings/0, populate/0]).

%% Users and posts are stored in collections so they have '_id' fields
-record(user, {'_id', email, first_name, last_name}).
-record(post, {'_id', title, author, tags, comments, body}).
-record(text_post, {content}).
-record(image_post, {image_path}).
-record(link_post, {link_url}).

%% Comments are embedded in posts so do not have an '_id' field.
-record(comment, {content, name}).

add_mappings() ->
	?add_mapping(user),
	?add_mapping(post),
	?add_mapping(comment),
	?add_mapping(text_post),
	?add_mapping(image_post),
	?add_mapping(link_post).

populate() ->
	John = #user{?id(), email= <<"jdoe@example.com">>, first_name= <<"John">>, last_name= <<"Doe">>},
	Post1 = #post{?id(), title= <<"Fun with MongoEngine">>, author=John, tags=[<<"mongodb">>, <<"mongoengine">>], 
				  body=#text_post{content= <<"Took a look at mongoengine, looks pretty cool.">>}},
	Post2 = #post{?id(), title= <<"MongoEngine Documentation">>, author=John, tags=[<<"mongoengine">>],
				  body=#link_post{link_url= <<"http://tractiondigital.com/labs/mongoengine/docs">>}},
	{ok, Conn} = mongo:connect(localhost),
	mongrel:do(safe, master, Conn, tumblelog, 
			   fun() ->
					   mongrel:delete(#user{}),
					   mongrel:delete(#post{}),
					   mongrel:insert_all([Post1, Post2])
			   end),
	mongo_connect:close(Conn).
