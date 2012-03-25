-module(tumblelog).
-include_lib("mongrel/include/mongrel_macros.hrl").
-export([add_mappings/0, populate/0]).

-record(user, {'_id', email, first_name, last_name}).
-record(post, {'_id', title, author, tags, comments, body}).
-record(comment, {content, name}).
-record(text_post, {content}).
-record(image_post, {image_path}).
-record(link_post, {link_url}).

add_mappings() ->
	?add_mapping(user),
	?add_mapping(post),
	?add_mapping(comment),
	?add_mapping(text_post),
	?add_mapping(image_post),
	?add_mapping(link_post).

populate() ->
	%John = #user{?id(), email= <<"jdoe@example.com">>, first_name= <<"John">>, last_name= <<"Doe">>},
	Conn = mongo:connect(localhost),
	mongrel:do(safe, master, Conn, tumblelog, 
			   fun() ->
					   mongrel:delete(#user{}),
					   mongrel:delete(#post{})
			   end).
