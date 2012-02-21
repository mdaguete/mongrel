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
%%% @doc Mongrel application. This module implements the application behaviour. On startup
%%%      the application starts the mongrel supervisor, mongrel_sup.

-module(mongrel_app).

-behaviour(application).

%% application callbacks
-export([start/2,
		 stop/1]).


%% @doc Starts the application.
%% @spec start(any(), list()) -> {ok, pid()} | {error, any()}
%% @end
start(_Type, _StartArgs) ->
	TableId = ets:new(mongrel_table, [public]),
	mongrel_sup:start_link(TableId).

%% @doc Stops the application.
%% @spec stop(any()) -> ok
stop(_State) ->
    ok.
