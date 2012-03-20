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
-record(bar, {'_id', msg}).
-record(baz, {y, z}).

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
	     {coords, {x, 3, z, 5}} = mongrel_mapper:map_selector(Sel)
     end}.

query_conditional_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(coords)), 
		 Sel = #coords{x=3, y={'$gt', 7, '$lt', 10}},
	     {coords, {x, 3, y, {'$gt', 7, '$lt', 10}}} = mongrel_mapper:map_selector(Sel)
     end}.
	
basic_nested_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  Sel = #coords{x=3, y={foo, 7, bar, 9}},
			  {coords, {x, 3, y, {foo, 7, bar, 9}}} = mongrel_mapper:map_selector(Sel)
     end}.

record_nested_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  mongrel_mapper:add_mapping(?mapping(foo)),
			  Sel = #coords{x=3, y=#foo{bar=7, baz=9}},
			  {coords, {x, 3, 'y.#type', foo, 'y.bar', 7, 'y.baz', 9}} = mongrel_mapper:map_selector(Sel)
     end}.
	
record_mixed_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  mongrel_mapper:add_mapping(?mapping(foo)),
			  Sel = #coords{x=3, y=#foo{bar=#foo{baz=-1}, baz={x, 9}}},
			  {coords, {x, 3, 'y.#type', foo, 'y.bar.#type', foo, 'y.bar.baz', -1, 'y.baz', {x, 9}}} = mongrel_mapper:map_selector(Sel)
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
			  {coords, {x, 0, 'y.#id.#type', foo, 'y.#id.bar', 3}} = mongrel_mapper:map_selector(Sel)
     end}.

selector_with_id_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(bar)),
			  Sel = #bar{msg = -3},
			  {bar, {msg, -3}} = mongrel_mapper:map_selector(Sel)
     end}.

map_selector_nested_record_with_non_id_not_set_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  mongrel_mapper:add_mapping(?mapping(bar)),
			  {coords, {'x.#type', bar}} = mongrel_mapper:map_selector(#coords{x = #bar{}})
     end}.

map_selector_nested_record_with_id_non_id_set_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  mongrel_mapper:add_mapping(?mapping(bar)),
			  {coords, {'x.#id', 30}} = mongrel_mapper:map_selector(#coords{x = #bar{'_id' = 30, msg = 90}})
     end}.

map_selector_nested_record_with_id_not_set_and_non_id_set_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  mongrel_mapper:add_mapping(?mapping(bar)),
			  ?assertError(_, mongrel_mapper:map_selector(#coords{x = #bar{msg = 12345}}))
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
			  {'$inc', {foo, 3}, []} = mongrel_mapper:map_modifier(coords, {'$inc', {foo, 3}})
     end}.
	
map_modifier_record_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  {'$inc', {x, 1}, []} = mongrel_mapper:map_modifier(coords, {'$inc', #coords{x=1}})
     end}.
	
map_modifier_record_with_id_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(baz)),
			  mongrel_mapper:add_mapping(?mapping(foo)),
			  {'$set', {y, 3}, []} = mongrel_mapper:map_modifier(baz, {'$set', #baz{y=3}})
     end}.
	
map_selector_with_list_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			 {coords, {x, [1,2,3], y, 7}} = mongrel_mapper:map_selector(#coords{x = [1,2,3], y=7})
     end}.

map_selector_with_list_with_record_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  mongrel_mapper:add_mapping(?mapping(baz)),
			  {coords, {x, [1,2, {'#type', baz, y, 1}]}} = mongrel_mapper:map_selector(#coords{x = [1,2, #baz{y=1}]})
     end}.

map_selector_with_list_with_record_with_id_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  mongrel_mapper:add_mapping(?mapping(bar)),
			  {coords, {x, [1,2, {'#type', bar, '#id', 7}, 4]}} = mongrel_mapper:map_selector(#coords{x = [1,2, #bar{'_id'=7, msg=1}, 4]})
     end}.

map_selector_with_nested_tuple_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(coords)),
			  mongrel_mapper:add_mapping(?mapping(baz)),
			  {coords, {x, [1, {'#type', baz, y, {'$gt', 7, '$lt', 9}}]}} = mongrel_mapper:map_selector(#coords{x = [1,#baz{y = {'$gt', 7, '$lt', 9}}]})
     end}.

map_projector_without_id_set_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(bar)), 
		 Bar = #bar{msg = 1},
	     {msg, 1} = mongrel_mapper:map_projection(Bar)
     end}.
	
map_query_tuple_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(coords)), 
		 Sel = #coords{x=3, z=5},
	     {coords, {'$query', {x, 3, z, 5}}} = mongrel_mapper:map_selector({'$query', Sel})
     end}.

map_nonquery_tuple_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(coords)), 
		 Sel = #coords{x=3, z=5},
	     ?assertError(_, mongrel_mapper:map_selector({'$notquery', Sel}))
     end}.

map_query_with_ordering_tuple_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(coords)), 
		 Sel = #coords{x=3, z=5},
	     {coords, {'$query', {x, 3, z, 5}, '$orderby', {y, 1}}} = mongrel_mapper:map_selector({'$query', Sel, '$orderby', {y, 1}})
     end}.

map_query_with_ordering_record_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(coords)), 
		 Sel = #coords{x=3, z=5},
		 OrderBy = #coords{y= -1},
	     {coords, {'$orderby', {y, -1}, '$query', {x, 3, z, 5}}} = mongrel_mapper:map_selector({'$orderby', OrderBy, '$query', Sel})
     end}.
