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
%%% @doc Mongrel mapping server. This module provides the Record/Mapping API.
%%% @end

-module(mongrel).

-behaviour(gen_server).

%% API
-export([count/1,
		 count/2,
		 delete/1,
		 delete_one/1,
		 do/5,
		 find_one/1,
		 insert/1,
		 insert_all/1,
		 save/1]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

-record(state, {}).

%% External functions
count(RecordSelector) ->
	Collection = mongrel_mapper:get_type(RecordSelector),
	Selector = mongrel_mapper:map_selector(RecordSelector),
	mongo:count(Collection, Selector).

count(RecordSelector, Limit) ->
	Collection = mongrel_mapper:get_type(RecordSelector),
	Selector = mongrel_mapper:map_selector(RecordSelector),
	mongo:count(Collection, Selector, Limit).
	
delete(RecordSelector) ->
	Collection = mongrel_mapper:get_type(RecordSelector),
	Selector = mongrel_mapper:map_selector(RecordSelector),
	mongo:delete(Collection, Selector).

delete_one(RecordSelector) ->
	Collection = mongrel_mapper:get_type(RecordSelector),
	Selector = mongrel_mapper:map_selector(RecordSelector),
	mongo:delete_one(Collection, Selector).

do(WriteMode, ReadMode, Connection, Database, Action) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [Connection], []),
	gen_server:call(Pid, {do, WriteMode, ReadMode, Connection, Database, Action}, infinity).

find_one(RecordSelector) ->
	{{Collection, Selector}, _} = mongrel_mapper:map(RecordSelector),
	{Res} = mongo:find_one(Collection, Selector),
	CallbackFunc = fun(Coll, Id) ->
						   {Reference} = mongo:find_one(Coll, {'_id', Id}),
						   Reference
				   end,
	mongrel_mapper:unmap(Collection, Res, CallbackFunc).

insert(Record) ->
	{{Collection, Document}, ChildDocuments} = mongrel_mapper:map(Record),
	[mongo:save(ChildCollection, ChildDocument) || {ChildCollection, ChildDocument} <- ChildDocuments],
	mongo:insert(Collection, Document).

insert_all(Records) ->
	[insert(Record) || Record <- Records].
	
save(Record) ->
	{{Collection, Document}, ChildDocuments} = mongrel_mapper:map(Record),
	[mongo:save(ChildCollection, ChildDocument) || {ChildCollection, ChildDocument} <- ChildDocuments],
	mongo:save(Collection, Document).
	

%% Server functions

%% @doc Initializes the server with a MongoDB connection.
%% @spec init(MongoDbConnection) -> {ok, State::tuple()}
%% @end
init([MongoDbConnection]) ->
	put(db_connection, MongoDbConnection),
    {ok, #state{}}.

%% @doc Responds synchronously to server calls.
%% @spec handle_call(Message::tuple(), From::pid(), State::tuple()) -> {stop, normal, Reply::any(), NewState::tuple()}
%% @end
handle_call({do, WriteMode, ReadMode, Connection, Database, Action}, _From, State) ->
    Reply = mongo:do(WriteMode, ReadMode, Connection, Database, Action),
    {stop, normal, Reply, State}.

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


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

