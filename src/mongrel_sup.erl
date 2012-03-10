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
%%% @doc Mongrel supervisor. This module starts up the mongrel_mapper server and
%%%      restarts the server if it crashes.
%%% @end

-module(mongrel_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% External functions

%% @doc Starts the supervisor on the local node and registers the
%%      process.
-spec(start_link(integer()) -> {ok, pid()} | {error, any()}).
start_link(EtsTableId) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [EtsTableId]).


%% Supervisor functions

%% @doc Supervisor callback. Starts the mongrel server.
-spec(init(list(integer())) -> {ok, {RestartStrategy::tuple(), Servers::list(module())}}).
init(EtsTableIdList) ->
	[TableId] = EtsTableIdList,
    Server = {mongrel_mapper, {mongrel_mapper, start_link, [TableId]},
	      permanent, 2000, worker, [mongrel]},
	RestartStrategy = {one_for_one, 4, 3600}, 
    {ok, {RestartStrategy, [Server]}}.
