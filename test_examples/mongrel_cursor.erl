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
%%% @doc Mongrel cursor process. This module provides functions for getting documents from a cursor.
%%% @end

-module(mongrel_cursor).

-behaviour(gen_server).

%% External exports
-export([cursor/2,
		 next/1]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

-record(state, {mongo_cursor, mongo_db_connection}).

%% External functions
cursor(MongoDbCursor, MongoDbConnection) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [MongoDbCursor, MongoDbConnection], []),
	Pid.

next(Cursor) ->
	gen_server:call(Cursor, next, infinity).

%% Server functions

%% @doc Initializes the cursor with a MongoDB cursor and connection.
%% @spec init(MongoDbCursor, MongoDbConnection) -> {ok, State::tuple()}
%% @end
init([MongoDbCursor, MongoDbConnection]) ->
    {ok, #state{mongo_cursor=MongoDbCursor, mongo_db_connection=MongoDbConnection}}.

handle_call(next, _From, State) ->
		case mongo_cursor:next(State#state.mongo_cursor) of
			{} ->
				{stop, normal, {}, State};
			Document ->
				{reply, Document, State}
		end.

%% @doc Responds asynchronously to messages.
%% @spec handle_cast(any(), tuple()) -> {no_reply, State}
%% @end
handle_cast(_Message, State) ->
	{noreply, State}.

%% @doc Responds to non-OTP messages.
%% @spec handle_info(any(), tuple()) -> {no_reply, State}
%% @end
handle_info(_Info, State) ->
	{noreply, State}.

%% @doc Handles the shutdown of the server.
%% @spec terminate(any(), any()) -> ok
%% @end
terminate(_Reason, _State) ->
	ok.

%% @doc Responds to code changes.
%% @spec code_change(any(), any(), any()) -> {ok, State}
%% @end
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.


%%% Internal functions

