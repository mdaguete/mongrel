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
%%% @doc Mongrel server. This module provides the Record/Mapping API.
%%% @end


-module(mongrel_mapper).

-behaviour(gen_server).

%% API
-export([start_link/1, 
		 add_mapping/1, 
		 get_mapping/1,
		 is_mapped/1,
		 has_id/1,
		 get_id/1,
		 map/1]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

-define(SERVER, ?MODULE).

%% We store the ETS table ID across calls.
-record(state, {ets_table_id}).

%% External functions
start_link(EtsTableId) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [EtsTableId], []).

add_mapping({RecordName, FieldIds}) when is_atom(RecordName) ->
	[true = is_atom(FieldId) || FieldId <- FieldIds],
	gen_server:call(?SERVER, {add_mapping, {RecordName, FieldIds}}, infinity).

get_mapping(RecordName) when is_atom(RecordName) ->
	[{RecordName, FieldIds}] = gen_server:call(?SERVER, {get_mapping, RecordName}, infinity),
	FieldIds.

is_mapped(RecordName) when is_atom(RecordName) ->
	case gen_server:call(?SERVER, {get_mapping, RecordName}, infinity) of
		[] ->
			false;
		[{RecordName, _}] ->
			true
	end;
is_mapped(Record) when is_tuple(Record) andalso size(Record) > 1 ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	case gen_server:call(?SERVER, {get_mapping, RecordName}, infinity) of
		[] ->
			false;
		[{RecordName, FieldIds}] ->
			length(FieldIds) =:= length(FieldValues)
	end.

has_id(RecordName) when is_atom(RecordName) ->
	FieldIds = get_mapping(RecordName),
	CheckHasId = fun(FieldId, Result) ->
									   Result or (FieldId =:= '_id')
				 end,
	lists:foldl(CheckHasId, false, FieldIds);
has_id(Record) when is_tuple(Record) andalso size(Record) > 1 ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	has_id(RecordName) andalso length(FieldValues) =:= length(get_mapping(RecordName)). 

get_id(Record) ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	FieldIds = get_mapping(RecordName),
	Fields = lists:zip(FieldIds, FieldValues),
	GetId = fun({FieldId, FieldValue}, Result) ->
					case FieldId of
						'_id' ->
							FieldValue;
						_ ->
							Result
					end
			end,
	lists:foldl(GetId, undefined, Fields).

map(Record) ->
	[RecordName|_FieldValues] = tuple_to_list(Record),
	{Document, ChildDocs} = parse_record_value(Record),
	ChildDocs ++ [{RecordName, Document}].


%% Server functions

%% @doc Initializes the server with the ETS table used to persist the
%%      mappings needed for mapping records to documents.
%% @spec init(EtsTableId::list(integer())) -> {ok, tuple()}
%% @end
init([EtsTableId]) ->
	{ok, #state{ets_table_id = EtsTableId}}.

%% @doc Responds synchronously to server calls.
%% @spec handle_call(Message::tuple(), From::pid(), State::tuple()) -> {reply, ok, NewState::tuple()}
%% @end
handle_call({add_mapping, {Key, Value}}, _From, State) ->
	true = ets:insert(State#state.ets_table_id, {Key, Value}),
	{reply, ok, State};
handle_call({get_mapping, Key}, _From, State) ->
	Reply = ets:lookup(State#state.ets_table_id, Key),
	{reply, Reply, State}.

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


%% Internal functions
parse_value(Value) when is_tuple(Value) ->
	parse_mapped_tuple(Value);
parse_value(Value) ->
	{Value, []}.

parse_mapped_tuple(Value) ->
	case has_id(Value) of
		false ->
			parse_record_value(Value);
		true ->
			[RecordName|_FieldValues] = tuple_to_list(Value),
			{ChildDoc, GrandChildren} = parse_record_value(Value),
			Id = get_id(Value),
			case Id of
				undefined ->
					{{'$type', RecordName}, GrandChildren ++ [{RecordName, ChildDoc}]};
				_ ->
					{{'$type', RecordName, '$id', Id}, GrandChildren ++ [{RecordName, ChildDoc}]}
			end
	end.

parse_record_value(Record) ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	FieldIds = get_mapping(RecordName),
	Result = [],
	ChildDocs = [],
	parse_record_value(FieldIds, FieldValues, ChildDocs, Result).

parse_record_value([], [], ChildDocs, Result) ->
	{list_to_tuple(Result), ChildDocs};
parse_record_value([_FieldId|IdTail], [undefined|ValueTail], ChildDocs, Result) ->
	parse_record_value(IdTail, ValueTail, ChildDocs, Result);
parse_record_value([FieldId|IdTail], [FieldValue|ValueTail], ChildDocs, Result) ->
	{ChildValue, GrandChildrenDocs} = parse_value(FieldValue),
	parse_record_value(IdTail, ValueTail, GrandChildrenDocs ++ ChildDocs, Result ++ [FieldId, ChildValue]).
