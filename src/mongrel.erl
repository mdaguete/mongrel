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
%%% @doc Mongrel API. This module provides functions for creating, reading, updating and deleting
%%       documents. The functions exposed are similar to the CRUD functions exposed by the
%%       mongo API of the MongoDB driver. While the mongo functions take collection and document 
%%       arguments, the mongrel functions expect one or more records as arguments. 
%%% @end

-module(mongrel).

-behaviour(gen_server).

%% Includes
-include("mongrel.hrl").

%% API
-export([count/1,
		 count/2,
		 delete/1,
		 delete_one/1,
		 do/5,
		 find/1,
		 find/2,
		 find/3,
		 find/4,
		 find_one/1,
		 find_one/2,
		 find_one/3,
		 get_connection_parameters/0,
		 insert/1,
		 insert_all/1,
		 modify/2,
		 replace/2,
		 repsert/2,
		 save/1]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

%% Types
-type(action() :: fun()).

%% External functions

%% @doc Counts the number of documents that match some selector.
-spec(count(record()) -> integer()).
count(SelectorRecord) ->
	count(SelectorRecord, 0).

%% @doc Counts the number of documents that match some selector up to a specified
%%      maximum. A limit of 0 means that all matching documents are counted.
-spec(count(record(), integer()) -> integer()).
count(SelectorRecord, Limit) ->
	{Collection, Selector} = mongrel_mapper:map_selector(SelectorRecord),
	mongo:count(Collection, Selector, Limit).
	
%% @doc Deletes all documents that match a selector.
-spec(delete(record()) -> ok).
delete(SelectorRecord) ->
	{Collection, Selector} = mongrel_mapper:map_selector(SelectorRecord),
	mongo:delete(Collection, Selector).

%% @doc Deletes the first document that matches a selector.
-spec(delete_one(record()) -> ok).
delete_one(SelectorRecord) ->
	{Collection, Selector} = mongrel_mapper:map_selector(SelectorRecord),
	mongo:delete_one(Collection, Selector).

%% @doc Executes an 'action' using the specified read and write modes to a database using a connection.
%%      An 'action' is a function that takes no arguments. The fun will usually invoke functions
%%      to do inserts, finds, modifies, deletes, etc.
-spec(do(mongo:write_mode(), mongo:read_mode(), mongo:connection()|mongo:rs_connection(),mongo:db(), mongo:action()) -> {ok, any()}|{failure, any()}).
do(WriteMode, ReadMode, Connection, Database, Action) ->
	%% Since we need to store state information, we spawn a new process for this
	%% function so that if the Action also invokes the 'do' function we don't wind up trashing
	%% the original state.
	ConnectionParameters = #mongrel_connection{write_mode=WriteMode, read_mode=ReadMode, connection=Connection, database=Database},
	{ok, Pid} = gen_server:start_link(?MODULE, [ConnectionParameters], []),
	gen_server:call(Pid, {do, Action}, infinity).

%% @doc Finds all documents that match a selector and returns a cursor.
-spec(find(record()) -> mongrel_cursor:cursor()).
find(SelectorRecord) ->
	find(SelectorRecord, []).

%% @doc Finds all documents that match a selector and returns a cursor
%%      of a projection. The projection can be passed as a mapped
%%      record or as a Mongo tuple consisting of alternating keys and values.
%%      An empty list ([]) indicates that the full projection of documents must
%%      be returned.
-spec(find(record(), record()) -> mongrel_cursor:cursor()).
find(SelectorRecord, ProjectorRecord) ->
	find(SelectorRecord, ProjectorRecord, 0).

%% @doc Finds all documents that match a selector and returns a cursor
%%      of a projection result. A specified number of matching
%%      documents are skipped. 
-spec(find(record(), record(), integer()) -> mongrel_cursor:cursor()).
find(SelectorRecord, ProjectorRecord, Skip) ->
	find(SelectorRecord, ProjectorRecord, Skip, 0).

%% @doc Finds all documents that match a selector and returns a cursor
%%      of a projection result. A specified number of matching
%%      documents are skipped.  The cursor retrieves results in the specified batch size.
-spec(find(record(), record(), integer(), integer()) -> mongrel_cursor:cursor()).
find(SelectorRecord, ProjectorRecord, Skip, BatchSize) ->
	{Collection, Selector} = mongrel_mapper:map_selector(SelectorRecord),
	Projector = mongrel_mapper:map_projection(ProjectorRecord),
	MongoCursor = mongo:find(Collection, Selector, Projector, Skip, BatchSize),
	MongrelConnection = get_connection_parameters(),
	mongrel_cursor:cursor(MongoCursor, MongrelConnection, Collection).

%% @doc Finds the first document that matches a selector and returns the document as a record.
-spec(find_one(record()) -> record()|{}).
find_one(SelectorRecord) ->
	find_one(SelectorRecord, []).

%% @doc Finds the first document that matches a selector and returns a
%%      projection of the document. The empty projection ([]) means
%%      that all fields in the document are populated.  The projection can be 
%%      passed as a mapped record or as a MongoDB tuple consisting of alternating 
%%      keys and values.
-spec(find_one(record(), record()|tuple()) -> record()|{}).
find_one(SelectorRecord, ProjectorRecord) ->
	find_one(SelectorRecord, ProjectorRecord, 0).

%% @doc Finds a document that matches a selector and returns a
%%      projection of the document after skipping a certain number of 
%%      matching documents.
-spec(find_one(record(), record()|tuple(), integer()) -> {record()}).
find_one(SelectorRecord, ProjectorRecord, Skip) ->
	{Collection, Selector} = mongrel_mapper:map_selector(SelectorRecord),
	Projector = mongrel_mapper:map_projection(ProjectorRecord),
	{Res} = mongo:find_one(Collection, Selector, Projector, Skip),
	CallbackFunc = fun(Coll, Id) ->
						   {Reference} = mongo:find_one(Coll, {'_id', Id}),
						   Reference
				   end,
	mongrel_mapper:unmap(Collection, Res, CallbackFunc).

%% @doc Gets the parameters used to read from and write to the database.
%%      It only makes sense to invoke this function from within the process
%%      spawned by the do/5 function.
-spec(get_connection_parameters() -> #mongrel_connection{}).
get_connection_parameters() ->
	get(mongrel_state).

%% @doc Inserts a record into a collection with the same name as the record type. If the 
%%      record contains nested records with '_id' fields, the nested documents are upserted
%%      into their appropriate collections as well. The ID of the inserted document is
%%      returned.
-spec(insert(record()) -> bson:value()).
insert(Record) ->
	{{Collection, Document}, ChildDocuments} = mongrel_mapper:map(Record),
	[mongo:save(ChildCollection, ChildDocument) || {ChildCollection, ChildDocument} <- ChildDocuments],
	mongo:insert(Collection, Document).

%% @doc Inserts a list of records into collections with the same name as the corresponding
%%      record type. If a record contains nested records with '_id' fields, the nested documents are 
%%      upserted into their appropriate collections as well. A list of IDs of the inserted documents is
%%      returned.
-spec(insert_all(list(record())) -> list(bson:value())).
insert_all(Records) ->
	[insert(Record) || Record <- Records].
	
%% @doc Updates selected documents using a modifier. The modifier can be specified as a {key, value}
%%      tuple where the key is a modifier and the value is a mapped record (e.g {'$set', #foo{bar=3}}) 
%%      or as a mongo modifier tuple (e.g. {'$set', {bar, 3}).
-spec(modify(record(), record()) -> ok).
modify(SelectorRecord, ModifierRecord) ->
	{Collection, Selector} = mongrel_mapper:map_selector(SelectorRecord),
	{ModifierKey, ModifierValue, ChildDocuments} = mongrel_mapper:map_modifier(Collection, ModifierRecord),
	[mongo:save(ChildCollection, ChildDocument) || {ChildCollection, ChildDocument} <- ChildDocuments],
	mongo:modify(Collection, Selector, {ModifierKey, ModifierValue}).

%% @doc Replaces the first document that matches the selector with a new document.
-spec(replace(record(), record()) -> ok).
replace(SelectorRecord, NewRecord) ->
	{{Collection, NewDocument}, ChildDocuments} = mongrel_mapper:map(NewRecord),
	{Collection, Selector} = mongrel_mapper:map_selector(SelectorRecord),
	mongo:replace(Collection, Selector, NewDocument),
	[mongo:save(ChildCollection, ChildDocument) || {ChildCollection, ChildDocument} <- ChildDocuments].
	
%% @doc Replaces the first document that matches the selector with a new document. If no
%%      document can be matched, the new document is inserted.
-spec(repsert(record(), record()) -> ok).
repsert(SelectorRecord, NewRecord) ->
	{{Collection, NewDocument}, ChildDocuments} = mongrel_mapper:map(NewRecord),
	{Collection, Selector} = mongrel_mapper:map_selector(SelectorRecord),
	mongo:repsert(Collection, Selector, NewDocument),
	[mongo:save(ChildCollection, ChildDocument) || {ChildCollection, ChildDocument} <- ChildDocuments].
	
%% @doc Upserts a record into a collection with the same name as the record type. If the 
%%      record contains nested records with '_id' fields, the nested documents are upserted
%%      into their appropriate collections as well.
-spec(save(record()) -> ok).
save(Record) ->
	{{Collection, Document}, ChildDocuments} = mongrel_mapper:map(Record),
	[mongo:save(ChildCollection, ChildDocument) || {ChildCollection, ChildDocument} <- ChildDocuments],
	mongo:save(Collection, Document).
	

%% Server functions

%% @doc Initializes the server with a write mode, read mode, a connection and database.
%%      The parameters are stored in the process dictionary so that they can be used
%%      if a connection is needed by a cursor to access collections.
-spec(init([State::#mongrel_connection{}]) -> {ok, State::#mongrel_connection{}}).
init([State]) ->
    {ok, State}.

%% @doc Responds synchronously to server calls.  The action of the do/5 function is executed by
%%      this function. The process is stopped after this call.
-spec(handle_call({do, action()}, pid(), #mongrel_connection{}) -> {stop, normal, any(), #mongrel_connection{}}).
handle_call({do, Action}=_Request, _From, State) ->
    Reply = mongo:do(State#mongrel_connection.write_mode, State#mongrel_connection.read_mode, 
					 State#mongrel_connection.connection, State#mongrel_connection.database,
					 fun() ->
							 put(mongrel_state, State),
							 Action()
					 end),
    {stop, normal, Reply, State}.

%% @doc Responds asynchronously to messages. The server ignores any asynchronous messages.
-spec(handle_cast(any(), State::#mongrel_connection{}) -> {noreply, State::#mongrel_connection{}}).
handle_cast(_Message, State) ->
	{noreply, State}.

%% @doc Responds to out-of-band messages. The server ignores any such messages.
-spec(handle_info(any(), State::#mongrel_connection{}) -> {noreply, State::#mongrel_connection{}}).
handle_info(_Info, State) ->
	{noreply, State}.

%% @doc Handles the shutdown of the server.
-spec(terminate(any(), #mongrel_connection{}) -> ok).
terminate(_Reason, _State) ->
	ok.

%% @doc Responds to code changes. Any code changes are ignored (the server's state is unchanged).
-spec(code_change(any(), State::#mongrel_connection{}, any()) -> {ok, State::#mongrel_connection{}}).
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.
