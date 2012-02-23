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

to_document_ok_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
		 Foo = #foo{bar=3, baz=5},
	     {bar, 3, baz, 5} = mongrel_mapper:to_document(Foo)
     end}.
	
to_document_with_undefined_value_test_() ->
	{setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = mongrel_mapper:add_mapping(?mapping(foo)), 
		 Foo = #foo{},
	     {baz, 4} = mongrel_mapper:to_document(Foo)
     end}.
	