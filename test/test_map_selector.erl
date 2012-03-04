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

-module(test_map_selector).

%% Include files
-include_lib("eunit/include/eunit.hrl").
-include_lib("mongrel_macros.hrl").

%% records used for testing.
-record(coords, {x, y, z}).
-record(foo, {bar, baz, unused}).
-record(bar, {'_id', msg= <<"hello">>}).

setup() ->
    T = ets:new(myets,[named_table,public]), 
    mongrel_mapper:start_link(T). 

cleanup(_) ->
	ets:delete(myets).


basic_selector_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(coords)), 
		 Sel = #coords{x=3, z=5},
	     {x, 3, z, 5} = mongrel_mapper:map_selector(Sel)
     end}.

query_conditional_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(coords)), 
		 Sel = #coords{x=3, y={'$gt', 7, '$lt', 10}},
	     {x, 3, y, {'$gt', 7, '$lt', 10}} = mongrel_mapper:map_selector(Sel)
     end}.
	
query_non_record_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
		 Sel = {x, {'$gt', 7}},
	     {x, {'$gt', 7}} = mongrel_mapper:map_selector(Sel)
     end}.

query_non_tuple_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
		 Sel = 3,
	     3 = mongrel_mapper:map_selector(Sel)
     end}.
	
basic_nested_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  Sel = #coords{x=3, y={foo, 7, bar, 9}},
			  {x, 3, y, {foo, 7, bar, 9}} = mongrel_mapper:map_selector(Sel)
     end}.

record_nested_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  mongrel_mapper:add_mapping(?mapping(foo)),
			  Sel = #coords{x=3, y=#foo{bar=7, baz=9}},
			  {x, 3, 'y.#type', foo, 'y.bar', 7, 'y.baz', 9} = mongrel_mapper:map_selector(Sel)
     end}.
	
record_mixed_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  mongrel_mapper:add_mapping(?mapping(foo)),
			  Sel = #coords{x=3, y=#foo{bar=#foo{baz=-1}, baz={x, 9}}},
			  {x, 3, 'y.#type', foo, 'y.bar.#type', foo, 'y.bar.baz', -1, 'y.baz', {x, 9}} = mongrel_mapper:map_selector(Sel)
     end}.

record_with_id_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  mongrel_mapper:add_mapping(?mapping(bar)),
			  mongrel_mapper:add_mapping(?mapping(foo)),
			  Sel = #coords{x=0, y=#bar{'_id'=#foo{bar=3}}},
			  {x, 0, 'y.#type', bar, 'y.#id.#type', foo, 'y.#id.bar', 3} = mongrel_mapper:map_selector(Sel)
     end}.

map_empty_list_projection_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  [] = mongrel_mapper:map_projection([])
     end}.
	
map_mongo_style_projection_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  {title, 1, author, 1} = mongrel_mapper:map_projection({title, 1, author, 1})
     end}.
	
map_record_projection_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  {x, 1, y, 1, z, 0} = mongrel_mapper:map_projection(#coords{x=1, y=1, z=0})
     end}.
	
map_modifier_not_record_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  {foo, 3, bar, 5} = mongrel_mapper:map_modifier({foo, 3, bar, 5})
     end}.
	
map_modifier_record_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  {x, {'$inc', 1}} = mongrel_mapper:map_modifier(#coords{x = {'$inc', 1}})
     end}.
	
map_modifier_nested_record_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  mongrel_mapper:add_mapping(?mapping(foo)),
			  {'x.#type', foo, 'x.bar', 3} = mongrel_mapper:map_modifier(#coords{x = #foo{bar=3}})
     end}.

map_modifier_nested_record_with_id_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  mongrel_mapper:add_mapping(?mapping(bar)),
			  {'x.#type', bar, 'x.#id', 5} = mongrel_mapper:map_modifier(#coords{x = #bar{'_id'=5}})
     end}.
	
map_modifier_nested_record_with_id_not_set_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  mongrel_mapper:add_mapping(?mapping(bar)),
			  ?assertError(_, mongrel_mapper:map_modifier(#coords{x = #bar{}}))
     end}.
	