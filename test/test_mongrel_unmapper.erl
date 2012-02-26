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


-module(test_mongrel_unmapper).


%% Include files
-include_lib("eunit/include/eunit.hrl").
-include_lib("mongrel_macros.hrl").

%% records used for testing.
-record(foo, {bar, baz=4}).
-record(bar, {'_id'}).
-record(baz, {x=2, y=8}).
-record(buzz, {'_id', w, z}).

setup() ->
    T = ets:new(myets,[named_table,public]), 
    mongrel_mapper:start_link(T). 

cleanup(_) ->
	ets:delete(myets).

set_field_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(foo)),
			  Foo1 = #foo{bar=123},
			  Foo1 = mongrel_mapper:set_field(#foo{}, bar, 123, undefined),
			  Foo2 = Foo1#foo{baz=456},
			  Foo2 = mongrel_mapper:set_field(Foo1, baz, 456, ?MODULE)
     end}.

set_field_with_reference_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(baz)),
			  BazExpected = #baz{x=#buzz{'_id'=7, w = 3, z = 27}},
			  GetBuzz = fun(buzz, 7) ->
								#buzz{'_id'=7, w = 3, z = 27}
						end,
			  Baz = mongrel_mapper:set_field(#baz{}, x, {'$type', buzz, '$id', 7}, GetBuzz),
			  BazExpected = Baz
     end}.
	
set_non_existent_field_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  mongrel_mapper:add_mapping(?mapping(baz)),
			  ?assertError(_, mongrel_mapper:set_field(#baz{}, z, 123, undefined))
     end}.
	