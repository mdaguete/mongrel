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
