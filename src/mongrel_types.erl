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

%%% @author CA Meijer
%%% @copyright 2012 CA Meijer
%%% @doc Provides convenience function for encapsulating data types in the form required by
%%       the Erlang MongoDB driver.
%%% @end

-module(mongrel_types).

%% API
-export([binary/1,
		 uuid/1,
		 md5/1,
		 regex/1,
		 regex/2,
		 id/0]).

%% External functions

%% @doc Encapsulates a binary value as 3-tuple expected by MongoDB driver.
-spec(binary(X::binary()) -> {bin, bin, X::binary()}).
binary(X) when is_binary(X) ->
	{bin, bin, X}.

%% @doc Encapsulates a UUID as 3-tuple expected by MongoDB driver.
-spec(uuid(X::binary()) -> {bin, uuid, X::binary()}).
uuid(X) when is_binary(X) ->
	{bin, uuid, X}.

%% @doc Encapsulates an MD5 hash as 3-tuple expected by MongoDB driver.
-spec(md5(X::binary()) -> {bin, md5, X::binary()}).
md5(X) when is_binary(X) ->
	{bin, md5, X}.

%% @doc Encapsulates a regex with no options as 3-tuple expected by MongoDB driver.
-spec(regex(X::binary()) -> {regex, X::binary(), binary()}).
regex(X) when is_binary(X) ->
	{regex, X, <<>>}.

%% @doc Encapsulates a regex with options as 3-tuple expected by MongoDB driver.
-spec(regex(X::binary(), Y::binary()) -> {regex, X::binary(), Y::binary()}).
regex(X, Y) when is_binary(X) andalso is_binary(Y) ->
	{regex, X, Y}.

%% @doc Returns a unique object ID. 
-spec(id() -> bson:objectid()).
id() ->
	mongodb_app:gen_objectid().
