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

-module(test_map_modifier).

%% Include files
-include_lib("eunit/include/eunit.hrl").
-include_lib("mongrel_macros.hrl").

%% records used for testing.
-record(foo, {x, y, z}).
-record(bar, {a, b, c}).
-record(baz, {'_id', d, e, f}).

setup() ->
    T = ets:new(myets,[named_table,public]), 
    mongrel_mapper:start_link(T). 

cleanup(_) ->
	ets:delete(myets).

modify_flat_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)),
		 ok = mongrel_mapper:add_mapping(?mapping(bar)),
		 {'$inc', {'x', 3, 'y.z', 5}, []} = mongrel_mapper:map_modifier(foo, {'$inc', {'x', 3, 'y.z', 5}})
     end}.

modify_simple_record_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)),
		 {'$set', {'x', 3}, []} = mongrel_mapper:map_modifier(foo, {'$set', #foo{x=3}})
     end}.
	
modify_nested_record_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)),
	     ok = mongrel_mapper:add_mapping(?mapping(bar)),
		 {'$set', {'x', {'#type', bar, a, 1}}, []} = mongrel_mapper:map_modifier(foo, {'$set', #foo{x=#bar{a=1}}})
     end}.
	
modify_nested_record_with_id_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)),
	     ok = mongrel_mapper:add_mapping(?mapping(baz)),
		 {'$set', {'x', {'#type', baz, '#id', 1}}, [{baz, {'_id', 1, d, 2, f, 3}}]} = mongrel_mapper:map_modifier(foo, {'$set', #foo{x=#baz{'_id'=1, d=2, f=3}}})
     end}.

modify_record_without_id_set_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(baz)),
		 {'$set', {d, 2}, []} = mongrel_mapper:map_modifier(baz, {'$set', #baz{d=2}})
     end}.
	
modify_record_without_id_set_in_nested_doc_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(baz)),
		 ?assertThrow(_, mongrel_mapper:map_modifier(baz, {'$set', #baz{d=#baz{}}}))
     end}.
	