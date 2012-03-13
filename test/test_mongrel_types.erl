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

-module(test_mongrel_types).

%% Include files
-include_lib("eunit/include/eunit.hrl").
-include_lib("mongrel_macros.hrl").

%% record used for testing.
-record(foo, {bar=3, baz=4}).

%%
%% Exported Functions
%%
-export([]).

to_binary_test() ->
	{bin, bin, <<1,2,3>>} = mongrel_types:binary(<<1,2,3>>),
	{bin, bin, <<>>} = ?binary(<<>>).

to_uuid_test() ->
	{bin, uuid, <<0,1,127,128,254,255>>} = mongrel_types:uuid(<<0,1,127,128,254,255>>),
	{bin, uuid, <<0,1,127,128,254,255>>} = ?uuid(<<0,1,127,128,254,255>>).

to_md5_test() ->
	{bin, md5, <<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>} = mongrel_types:md5(<<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>),
	{bin, md5, <<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>} = ?md5(<<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>).

mapping_test() ->
	{foo, [bar, baz]} = ?mapping(foo).

to_regex_test() ->
	{regex,<<"^\\d{5}$">>,<<>>} = mongrel_types:regex(<<"^\\d{5}$">>),
	{regex,<<"^\\d{5}$">>,<<>>} = ?regex(<<"^\\d{5}$">>).

to_regex_with_options_test() ->
	{regex,<<"^\\d{5}$">>,<<"i">>} = mongrel_types:regex(<<"^\\d{5}$">>, <<"i">>),
	{regex,<<"^\\d{5}$">>,<<"i">>} = ?regex(<<"^\\d{5}$">>, <<"i">>).
