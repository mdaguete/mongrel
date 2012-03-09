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
%%%      Unlike a mongo cursor, this cursor may need to read from the database when the cursor
%%%      is read since the document that is read may reference nested documents that need to be 
%%%      fetched.
%%% @end

-module(mongrel_cursor).

-behaviour(gen_server).

%% External exports
-export([close/1,
		 cursor/7,
		 next/1,
		 rest/1,
		 get_mongo_cursor/1]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

-record(state, {mongo_cursor, write_mode, read_mode, connection, database, collection, timeout}).

%% External functions
cursor(MongoCursor, WriteMode, ReadMode, Connection, Database, Collection, Timeout) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [MongoCursor, WriteMode, ReadMode, Connection, Database, Collection, Timeout], []),
	Pid.

next(Cursor) ->
	gen_server:call(Cursor, next, infinity).

rest(Cursor) ->
	gen_server:call(Cursor, rest, infinity).

get_mongo_cursor(Cursor) ->
	gen_server:call(Cursor, get_mongo_cursor, infinity).

close(Cursor) ->
	gen_server:call(Cursor, close, infinity).

%% Server functions

%% @doc Initializes the cursor with a MongoDB cursor and connection.
%% @spec init(list()) -> {ok, State::tuple(), Timeout::integer()}
%% @end
init([MongoCursor, WriteMode, ReadMode, Connection, Database, Collection, Timeout]) ->
	{ok, #state{mongo_cursor=MongoCursor, write_mode=WriteMode, read_mode=ReadMode, connection=Connection, 
				database = Database, collection=Collection, timeout=Timeout}, Timeout}.

handle_call(next, _From, State) ->
	case mongo_cursor:next(State#state.mongo_cursor) of
		{} ->
			{stop, normal, {}, State};
		{Document} ->
			CallbackFunc = construct_callback_function(State),
			Reply = mongrel_mapper:unmap(State#state.collection, Document, CallbackFunc),
			{reply, Reply, State, State#state.timeout}
	end;
handle_call(rest, _From, State) ->
	Docs = rest(State, []),
	{stop, normal, Docs, State};
handle_call(get_mongo_cursor, _From, State) ->
	{reply, State#state.mongo_cursor, State, State#state.timeout};
handle_call(close, _From, State) ->
	{stop, normal, ok, State}.


%% @doc Responds asynchronously to messages.
%% @spec handle_cast(any(), tuple()) -> {no_reply, State}
%% @end
handle_cast(_Message, State) ->
	{noreply, State}.

%% @doc Responds to non-OTP messages.
%% @spec handle_info(any(), tuple()) -> {no_reply, State}
%% @end
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State}.

%% @doc Handles the shutdown of the server.
%% @spec terminate(any(), record()) -> ok
%% @end
terminate(_Reason, State) ->
	mongo:close_cursor(State#state.mongo_cursor),
	ok.

%% @doc Responds to code changes.
%% @spec code_change(any(), any(), any()) -> {ok, State}
%% @end
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.


%%% Internal functions
rest(State, Docs) ->
	MongoCursor = State#state.mongo_cursor,
	case mongo_cursor:next(MongoCursor) of
		{} ->
			Docs;
		{Doc} ->
			CallbackFunction = construct_callback_function(State),
			Collection = State#state.collection,
			Record = mongrel_mapper:unmap(Collection, Doc, CallbackFunction),
			rest(State, Docs ++ [Record])
	end.

construct_callback_function(State) ->
	fun(Coll, Id) ->
			ReadMode = State#state.read_mode,
			WriteMode = State#state.write_mode,
			Connection = State#state.connection,
			Database = State#state.database,
			{ok, Res} = mongo:do(WriteMode, ReadMode, Connection, Database,
								 fun() ->
										 {Reference} = mongo:find_one(Coll, {'_id', Id}),
										 Reference
								 end),
			Res
	end.
