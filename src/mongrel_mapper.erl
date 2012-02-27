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

-module(mongrel_mapper).

-behaviour(gen_server).

%% API
-export([start_link/1, 
		 add_mapping/1, 
		 get_mapping/1,
		 is_mapped/1,
		 has_id/1,
		 get_field/2,
		 set_field/4,
		 map/1,
		 unmap/3]).

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
	end;
is_mapped(_) ->
	false.

has_id(RecordName) when is_atom(RecordName) ->
	FieldIds = get_mapping(RecordName),
	CheckHasId = fun(FieldId, Result) ->
									   Result or (FieldId =:= '_id')
				 end,
	lists:foldl(CheckHasId, false, FieldIds);
has_id(Record) when is_tuple(Record) andalso size(Record) > 1 ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	has_id(RecordName) andalso length(FieldValues) =:= length(get_mapping(RecordName)). 

get_field(Record, Field) ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	FieldIds = get_mapping(RecordName),
	get_field(FieldIds, FieldValues, Field).

set_field(Record, FieldId, {'$type', Collection, '$id', Id}, MapReferenceFun) ->
	set_field(Record, FieldId, MapReferenceFun(Collection, Id), MapReferenceFun);
set_field(Record, FieldId, FieldValue, _MapReferenceFun) ->
	[RecordName|RecordList] = tuple_to_list(Record),
	FieldIds = get_mapping(RecordName),
	UpdatedRecordList = set_field(RecordList, FieldIds, FieldId, FieldValue, []),
	list_to_tuple([RecordName|UpdatedRecordList]).

map(Record) ->
	[RecordName|_FieldValues] = tuple_to_list(Record),
	{Document, ChildDocs} = parse_record_value(Record, []),
	AllDocs = ChildDocs ++ [{RecordName, Document}],
	remove_repeat_docs(AllDocs, []).

unmap(RecordName, Tuple, MapReferenceFun) when is_atom(RecordName) ->
	FieldIds = get_mapping(RecordName),
	TupleList = tuple_to_list(Tuple),
	InitialTuple = list_to_tuple([RecordName] ++ lists:map(fun(_) -> undefined end, FieldIds)),
	unmap_list(TupleList, MapReferenceFun, InitialTuple);
unmap(RecordName, Tuple, MapReferenceFun) when is_binary(RecordName) ->
	unmap(erlang:list_to_atom(unicode:characters_to_list(RecordName)), Tuple, MapReferenceFun).



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
parse_value(Value, DocList) when is_tuple(Value) ->
	case mongrel_mapper:is_mapped(Value) of
		true ->
			parse_mapped_tuple(Value, DocList);
		false ->
			{Value, DocList}
	end;
parse_value(Value, DocList) when is_list(Value) ->
	parse_list_values(Value, DocList, []);
parse_value(Value, DocList) ->
	{Value, DocList}.

parse_mapped_tuple(Value, DocList) ->
	[RecordName|_FieldValues] = tuple_to_list(Value),
	case has_id(Value) of
		false ->
			{ChildDoc, UpdatedDocList} = parse_record_value(Value, DocList),
			ChildDocList = ['$type', RecordName] ++ tuple_to_list(ChildDoc),
			{list_to_tuple(ChildDocList), UpdatedDocList};
		true ->
			{ChildDoc, UpdatedDocList} = parse_record_value(Value, DocList),
			{{'$type', RecordName, '$id', get_field(Value, '_id')}, UpdatedDocList ++ [{RecordName, ChildDoc}]}
	end.

parse_record_value(Record, DocList) ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	FieldIds = get_mapping(RecordName),
	Result = [],
	parse_record_value(FieldIds, FieldValues, DocList, Result).

parse_record_value([], [], DocList, Result) ->
	{list_to_tuple(Result), DocList};
parse_record_value([_FieldId|IdTail], [undefined|ValueTail], DocList, Result) ->
	parse_record_value(IdTail, ValueTail, DocList, Result);
parse_record_value([FieldId|IdTail], [FieldValue|ValueTail], DocList, Result) ->
	{ChildValue, UpdatedDocList} = parse_value(FieldValue, DocList),
	parse_record_value(IdTail, ValueTail, UpdatedDocList, Result ++ [FieldId, ChildValue]).

parse_list_values([], DocList, Result) ->
	{Result, DocList};
parse_list_values([Value|Tail], DocList, Result) ->
	{ChildValue, UpdatedDocList} = parse_value(Value, DocList),
	parse_list_values(Tail, UpdatedDocList, Result ++ [ChildValue]).

get_field([FieldId|_IdTail], [FieldValue|_ValuesTail], FieldId) ->
	FieldValue;
get_field([_FieldIdHead|IdTail], [_FieldValueHead|ValuesTail], FieldId) ->
	get_field(IdTail, ValuesTail, FieldId).

remove_repeat_docs([], Result) ->
	Result;
remove_repeat_docs([Doc|DocTail], Result) ->
	case doc_in_list(Doc, Result) of
		true ->
			remove_repeat_docs(DocTail, Result);
		false ->
			remove_repeat_docs(DocTail, Result ++ [Doc])
	end.

doc_in_list(_Doc, []) ->
	false;
doc_in_list(Doc, [Doc|_DocTail]) ->
	true;
doc_in_list(Doc, [_DocHead|DocTail]) ->
	doc_in_list(Doc, DocTail).


set_field([], [], _FieldId, _NewFieldValue, Result) ->
	Result;
set_field([_FieldValue|ValuesTail], [FieldId|_TailsList], FieldId, NewFieldValue, Result) ->
	Result ++ [NewFieldValue] ++ ValuesTail;
set_field([FieldValue|ValuesTail], [_FieldId|TailsList], FieldId, NewFieldValue, Result) ->
	set_field(ValuesTail, TailsList, FieldId, NewFieldValue, Result ++ [FieldValue]).

unmap_list([] = _TupleList, _MapReferenceFun, ResultTuple) ->
	ResultTuple;
unmap_list([FieldId, FieldValue|Tail], MapReferenceFun, ResultTuple) when is_list(FieldValue) ->
	ListValue = unmap_list_value(FieldValue, MapReferenceFun, []),
	unmap_list(Tail, MapReferenceFun, set_field(ResultTuple, FieldId, ListValue, MapReferenceFun));
unmap_list([FieldId, FieldValue|Tail], MapReferenceFun, ResultTuple) when is_tuple(FieldValue) ->
	FieldValueList = tuple_to_list(FieldValue),
	case FieldValueList of
		['$type', Type, '$id', Id] when is_atom(Type) ->
			NestedDoc = MapReferenceFun(Type, Id),
			NestedRecord = unmap(Type, NestedDoc, MapReferenceFun),
			unmap_list(Tail, MapReferenceFun, set_field(ResultTuple, FieldId, NestedRecord, MapReferenceFun));
		['$type', Type, '$id', Id] ->
			NestedDoc = MapReferenceFun(list_to_atom(unicode:characters_to_list(Type)), Id),
			NestedRecord = unmap(Type, NestedDoc, MapReferenceFun),
			unmap_list(Tail, MapReferenceFun, set_field(ResultTuple, FieldId, NestedRecord, MapReferenceFun));
		['$type', Type|ValueTail] ->
			NestedRecord = unmap(Type, list_to_tuple(ValueTail), MapReferenceFun),
			unmap_list(Tail, MapReferenceFun, set_field(ResultTuple, FieldId, NestedRecord, MapReferenceFun));
		_ ->
			unmap_list(Tail, MapReferenceFun, set_field(ResultTuple, FieldId, FieldValue, MapReferenceFun))
	end;
unmap_list([FieldId, FieldValue|Tail], MapReferenceFun, ResultTuple) ->
	unmap_list(Tail, MapReferenceFun, set_field(ResultTuple, FieldId, FieldValue, MapReferenceFun)).

unmap_list_value([], _MapReferenceFun, Result) ->
	Result;
unmap_list_value([Value|Tail], MapReferenceFun, Result) when is_tuple(Value)->
	FieldValueList = tuple_to_list(Value),
	case FieldValueList of
		['$type', Type, '$id', Id] ->
			NestedDoc = MapReferenceFun(list_to_atom(unicode:characters_to_list(Type)), Id),
			NestedRecord = unmap(Type, NestedDoc, MapReferenceFun),
			unmap_list_value(Tail, MapReferenceFun, Result ++ [NestedRecord]);
		['$type', Type|ValueTail] ->
			NestedRecord = unmap(Type, list_to_tuple(ValueTail), MapReferenceFun),
			unmap_list_value(Tail, MapReferenceFun, Result ++ [NestedRecord]);
		_ ->
			unmap_list_value(Tail, MapReferenceFun, Result ++ [Value])
	end;
unmap_list_value([Value|Tail], MapReferenceFun, Result) when is_list(Value) ->
	ListValue = unmap_list_value(Value, MapReferenceFun, []),
	unmap_list(Tail, MapReferenceFun, Result ++ [ListValue]);
unmap_list_value([Value|Tail], MapReferenceFun, Result) ->
	unmap_list_value(Tail, MapReferenceFun, Result ++ [Value]).
