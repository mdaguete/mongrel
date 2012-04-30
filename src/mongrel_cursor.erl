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
%%%
%%       A second difference from a mongo cursor is that a mongrel cursor terminates when
%%%      the process that created the cursor terminates. This second difference implies that a mongrel 
%%%      cursor cannot be used outside of the action of a mongrel:do/5 function. If you want a cursor
%%%      to be available outside the scope of the do/5 function, you must explictly set a timeout for
%%%      the cursor. The value of infinity is acceptable as a timeout.
%%% @end

-module(mongrel_cursor).

-behaviour(gen_server).

%% Includes
-include("mongrel.hrl").

%% External exports
-export([close/1,
		 cursor/3,
		 next/1,
		 rest/1,
		 take/2,
		 get_mongo_cursor/1,
		 set_timeout/2]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

%% Records
-record(state, {mongo_cursor, read_mode, connection, database, collection, parent_process, timeout=infinity, die_with_parent=true}).

%% Types
-type(cursor() :: pid()).

%% External functions

%% @doc Creates a cursor using a specified connection to a database collection. If the cursor has 
%%      to return a document containing nested documents, the connection parameters are used to 
%%      read the nested documents.
-spec(cursor(mongo:cursor(), #mongrel_connection{}, mongo:collection()) -> cursor()).
cursor(MongoCursor, MongrelConnection, Collection) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [MongoCursor, MongrelConnection#mongrel_connection.read_mode, 
												MongrelConnection#mongrel_connection.connection, 
												MongrelConnection#mongrel_connection.database, 
												Collection, self()], []),
	Pid.

%% @doc Returns the next record from the cursor or an empty tuple if no more documents
%%      are available.
-spec(next(cursor()) -> record() | {}).
next(Cursor) ->
	gen_server:call(Cursor, next, infinity).

%% @doc Returns a list of records referenced by a cursor.
-spec(rest(cursor()) -> list(record())).
rest(Cursor) ->
	gen_server:call(Cursor, rest, infinity).

%% @doc Returns a list of records referenced by a cursor up to a specified limit.
-spec(take(integer(), cursor()) -> list(record())).
take(Limit, Cursor) when Limit >= 0 ->
	gen_server:call(Cursor, {take, Limit}, infinity).

%% @doc Returns the mongo:cursor() used by a mongrel:cursor(). Using the mongo:cursor() can
%%      be significantly faster than the mongrel:cursor() since it returns documents
%%      from a single collection rather than constructed records which involves eagerly
%%      loading record fields which may require several trips to the database.
-spec(get_mongo_cursor(cursor()) -> mongo:cursor()).
get_mongo_cursor(Cursor) ->
	gen_server:call(Cursor, get_mongo_cursor, infinity).

%% @doc Closes the cursor. This stops the cursor gen_server process.
-spec(close(cursor()) -> ok).
close(Cursor) ->
	gen_server:call(Cursor, close, infinity).

%% @doc Sets the cursor timeout. The cursor will die after the specified length of inactivity.
%%      The cursor remains alive even if the parent process dies.
-spec(set_timeout(cursor(), integer()|infinity) -> ok).
set_timeout(Cursor, Timeout) ->
	gen_server:call(Cursor, {set_timeout, Timeout}, infinity).
	
%% Server functions

%% @doc Initializes the cursor with a MongoDB cursor and connection parameters.
init([MongoCursor, ReadMode, Connection, Database, Collection, Pid]=_ConnectionParameters) ->
	monitor(process, Pid),
	{ok, #state{mongo_cursor=MongoCursor, read_mode=ReadMode, connection=Connection, database = Database, 
				collection=Collection, parent_process=Pid}, infinity}.

%% @doc Responds to synchronous messages. Synchronous messages are sent to get the next record,
%%      to get any remaining records, to get the mongo:cursor(), to close the cursor and to set
%%      the timeout of the cursor.
-spec(handle_call(Message::any(), pid(), State::record()) -> {stop, normal, Reply::any(), State::record() | {reply, Reply::any(), State::record(), Timeout::integer()}}).
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
handle_call({take, Limit}, _From, State) ->
	Docs = take(State, Limit, []),
	{stop, normal, Docs, State};
handle_call(get_mongo_cursor, _From, State) ->
	{reply, State#state.mongo_cursor, State, State#state.timeout};
handle_call(close, _From, State) ->
	{stop, normal, ok, State};
handle_call({set_timeout, Timeout}, _From, State) ->
	{reply, ok, State#state{die_with_parent=false, timeout=Timeout}, Timeout}.


%% @doc Responds asynchronously to messages. Asynchronous messages are ignored.
-spec(handle_cast(any(), State::record()) -> {noreply, State::record()}).
handle_cast(_Message, State) ->
	{noreply, State, State#state.timeout}.

%% @doc Responds to non-OTP messages. The messages that are handled are a timeout and the
%%      the termination of the parent process.
-spec(handle_info(Message::any(), State::record()) -> {stop, normal, State::record()}|{noreply, State::record()}).
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) when Pid =:= State#state.parent_process andalso State#state.die_with_parent ->
	{stop, normal, State};
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State, State#state.timeout}.

%% @doc Handles the shutdown of the server.
-spec(terminate(any(), record()) -> ok).
terminate(_Reason, State) ->
	mongo:close_cursor(State#state.mongo_cursor),
	ok.

%% @doc Responds to code changes. Code change events are ignored.
-spec(code_change(any(), State::record(), any()) -> {ok, State::record()}).
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.


%%% Internal functions

% Reads documents from a cursor and returns them as a list of records.
rest(State, Docs) ->
	MongoCursor = State#state.mongo_cursor,
	case mongo_cursor:next(MongoCursor) of
		{} ->
			lists:reverse(Docs);
		{Doc} ->
			CallbackFunction = construct_callback_function(State),
			Collection = State#state.collection,
			Record = mongrel_mapper:unmap(Collection, Doc, CallbackFunction),
			rest(State, [Record|Docs])
	end.

% Reads documents from a cursor up to a limit and returns them as a list of records.
take(_State, 0, Docs) ->
	lists:reverse(Docs);
take(State, Limit, Docs) ->
	MongoCursor = State#state.mongo_cursor,
	case mongo_cursor:next(MongoCursor) of
		{} ->
			lists:reverse(Docs);
		{Doc} ->
			CallbackFunction = construct_callback_function(State),
			Collection = State#state.collection,
			Record = mongrel_mapper:unmap(Collection, Doc, CallbackFunction),
			take(State, Limit-1, [Record|Docs])
	end.

% Creates a function that uses connection settings to read nested documents
% referenced in the cursor.
construct_callback_function(State) ->
	fun(Coll, Id) ->
			ReadMode = State#state.read_mode,
			Connection = State#state.connection,
			Database = State#state.database,
			{ok, Res} = mongo:do(safe, ReadMode, Connection, Database,
								 fun() ->
										 {Reference} = mongo:find_one(Coll, {'_id', Id}),
										 Reference
								 end),
			Res
	end.
