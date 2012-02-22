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

-module(test_mongrel).


%% Include files
-include_lib("eunit/include/eunit.hrl").
-include_lib("mongrel_macros.hrl").

%% record used for testing.
-record(foo, {bar=3, baz=4}).

setup() ->
	T = ets:new(myets, [named_table, public]),
	mongrel:start_link(T). 

cleanup(_) ->
	ets:delete(myets).

generator_test_() ->
	{setup,
	 fun setup/0, 
	 fun cleanup/1,
	 [fun add_ok/0, 
	  fun add_bad_record_name/0, 
	  fun add_bad_field_names/0]
	}.

add_ok() ->
	ok = mongrel:add_mapping(?mapping(foo)).

add_bad_record_name() ->
	?assertError(_, mongrel:add_mapping({"foo", []})).

add_bad_field_names() ->
	?assertError(_, mongrel:add_mapping({foo, [bar, "hello"]})).
	