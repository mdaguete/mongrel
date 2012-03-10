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

%% @doc Creates a cursor using a specified connection to a database collection. The cursor is
%%      configured to timeout after a specified length of inactivity. If the cursor has to return
%%      a document containing nested documents, the connection parameters are used to read the
%%      nested documents.
%%
%% @spec cursor(mongo:cursor(), mongo:write_mode(), mongo:read_mode(),
%%              mongo:connection()|mongo:rs_connection(), mongo:db(),
%%              mongo:collection(), integer()) -> pid()
%% @end
cursor(MongoCursor, WriteMode, ReadMode, Connection, Database, Collection, TimeoutInMilliseconds) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [MongoCursor, WriteMode, ReadMode, Connection, 
												Database, Collection, TimeoutInMilliseconds], []),
	Pid.

%% @doc Returns the next record from the cursor or an empty tuple if no more documents
%%      are available.
%%
%% @spec next(cursor()) -> record() | {}
%% @end
next(Cursor) ->
	gen_server:call(Cursor, next, infinity).

%% @doc Returns a list of records referenced by a cursor.
%%
%% @spec rest(cursor()) -> list(record())
%% @end
rest(Cursor) ->
	gen_server:call(Cursor, rest, infinity).

%% @doc Returns the mongo:cursor() used by a mongrel:cursor(). Using the mongo:cursor() can
%%      be significantly faster than the mongrel:cursor() since it returns documents
%%      from a single collection rather than constructed records which involves eagerly
%%      loading record fields which may require several trips to the database.
%%
%% @spec get_mongo_cursor(cursor()) -> mongo:cursor()
%% @end
get_mongo_cursor(Cursor) ->
	gen_server:call(Cursor, get_mongo_cursor, infinity).

%% @doc Closes the cursor. This involves terminating the cursor gen_server process.
%%
%% @spec close(cursor()) -> ok
%% @end
close(Cursor) ->
	gen_server:call(Cursor, close, infinity).

%% Server functions

%% @doc Initializes the cursor with a MongoDB cursor and connection.
%%
%% @spec init(list()) -> {ok, State::record(), Timeout::integer()}
%% @end
init([MongoCursor, WriteMode, ReadMode, Connection, Database, Collection, Timeout]) ->
	{ok, #state{mongo_cursor=MongoCursor, write_mode=WriteMode, read_mode=ReadMode, connection=Connection, 
				database = Database, collection=Collection, timeout=Timeout}, Timeout}.

%% @doc Responds to synchronous messages. Synchronous messages are sent to get the next record,
%%      to get the rest of the messages, to get the mongo:cursor() and to close the cursor.
%%
%% @spec handle_call(atom(), pid(), record()) -> tuple()
%% @end
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


%% @doc Responds asynchronously to messages. Asynchronous messages are ignored.
%% @spec handle_cast(any(), record()) -> {no_reply, State}
%% @end
handle_cast(_Message, State) ->
	{noreply, State}.

%% @doc Responds to non-OTP messages. The only out-of-band message of interest is a timeout.
%%      A timeout indicates that there has been no activity invoking the cursor for a 
%%      specified time. The cursor process is terminated on a timeout. All other messages are
%%      ignored.
%% @spec handle_info(any(), record()) -> tuple()
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

%% @doc Responds to code changes. Code change events are ignored.
%% @spec code_change(any(), record(), any()) -> {ok, State}
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
