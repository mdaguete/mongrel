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


-module(test_mongrel_mapper).


%% Include files
-include_lib("eunit/include/eunit.hrl").
-include_lib("mongrel_macros.hrl").

%% record used for testing.
-record(foo, {bar, baz=4}).
-record(bar, {'_id'}).
-record(baz, {x=2, y=8}).

setup() ->
    T = ets:new(myets,[named_table,public]), 
    mongrel_mapper:start_link(T). 

cleanup(_) ->
	ets:delete(myets).

add_ok_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo))
     end}.

add_bad_record_name_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ?assertError(_,mongrel_mapper:add_mapping({"foo",[bar]}))
     end}.

add_bad_field_name_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ?assertError(_,mongrel_mapper:add_mapping({foo, [bar,"baz"]}))
     end}.

add_bad_field_list_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ?assertError(_,mongrel_mapper:add_mapping({foo, not_a_list}))
     end}.

get_mapping_ok_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
	     [bar,baz] = mongrel_mapper:get_mapping(foo)
     end}.

get_mapping_not_ok_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ?assertError(_,mongrel_mapper:get_mapping(foo))
     end}.

is_mapped_true_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
	     true = mongrel_mapper:is_mapped(foo)
     end}.

is_mapped_false_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
	     false = mongrel_mapper:is_mapped(bar)
     end}.

is_mapped_record_true_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
	     true = mongrel_mapper:is_mapped({foo, 1, 3})
     end}.

is_mapped_record_false_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
	     false = mongrel_mapper:is_mapped({foo, 1, 3, 4})
     end}.

has_id_false_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
	     false = mongrel_mapper:has_id(foo)
     end}.
	
has_id_true_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(bar)), 
	     true = mongrel_mapper:has_id(bar)
     end}.

record_has_id_true_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(bar)), 
	     true = mongrel_mapper:has_id(#bar{'_id'=3})
     end}.
	
record_has_id_false_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(bar)), 
	     false = mongrel_mapper:has_id({bar,1,2})
     end}.
	
get_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(bar)), 
	     3 = mongrel_mapper:get_id(#bar{'_id'=3})
     end}.

get_undefined_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
	     undefined = mongrel_mapper:get_id(#foo{})
     end}.
	
	
map_basic_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
		 Foo = #foo{bar=3, baz=5},
	     [{foo, {bar, 3, baz, 5}}] = mongrel_mapper:map(Foo)
     end}.
	
map_undefined_value_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
		 Foo = #foo{},
	     [{foo, {baz, 4}}] = mongrel_mapper:map(Foo)
     end}.
	
map_nested_doc_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
	     ok = mongrel_mapper:add_mapping(?mapping(baz)), 
		 Foo = #foo{bar=3, baz= #baz{}},
	     [{foo, {bar, 3, baz, {x, 2, y, 8}}}] = mongrel_mapper:map(Foo)
     end}.

map_nested_doc_with_id_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
	     ok = mongrel_mapper:add_mapping(?mapping(bar)), 
		 Foo = #foo{bar= #bar{'_id'=7}, baz=9},
	     [{bar, {'_id', 7}}, {foo, {bar, {'$type', bar, '$id', 7}, baz, 9}}] = mongrel_mapper:map(Foo)
     end}.

map_nested_doc_with_undefined_id_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
	     ok = mongrel_mapper:add_mapping(?mapping(bar)), 
		 Foo = #foo{bar= #bar{'_id'=undefined}, baz=9},
	     [{bar, {}}, {foo, {bar, {'$type', bar}, baz, 9}}] = mongrel_mapper:map(Foo)
     end}.

doc_with_simple_list_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
		 Foo = #foo{bar=[1,2,3], baz=5},
	     [{foo, {bar, [1,2,3], baz, 5}}] = mongrel_mapper:map(Foo)
     end}.
	
doc_with_complex_list_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
	     ok = mongrel_mapper:add_mapping(?mapping(bar)), 
		 Foo = #foo{bar=[1,2,#bar{'_id'=8}, 4], baz=5},
	     [{bar, {'_id', 8}}, {foo, {bar, [1,2, {'$type', bar, '$id', 8}, 4], baz, 5}}] = mongrel_mapper:map(Foo)
     end}.

doc_with_complex_list_2_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
	     ok = mongrel_mapper:add_mapping(?mapping(bar)), 
	     ok = mongrel_mapper:add_mapping(?mapping(baz)), 
		 Foo = #foo{bar=[1,2,#bar{'_id'=3}, #baz{}], baz=5},
	     [{bar, {'_id', 3}}, {foo, {bar, [1,2, {'$type', bar, '$id', 3}, {x, 2, y, 8}], baz, 5}}] = mongrel_mapper:map(Foo)
     end}.

doc_with_non_record_tuple_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)),
		 Bin = ?binary(<<1,2,3>>),
		 Foo = #foo{bar=Bin, baz= <<"hello, world">>},
	     [{foo, {bar, Bin, baz, <<"hello, world">>}}] = mongrel_mapper:map(Foo)
     end}.
	