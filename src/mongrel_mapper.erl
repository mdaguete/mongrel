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
		 get_type/1,
		 get_field/2,
		 set_field/4,
		 map/1,
		 unmap/3,
		 map_selector/1,
		 map_projection/1,
		 map_modifier/1]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

-define(SERVER, ?MODULE).

%% Include files
-include_lib("mongrel_macros.hrl").

%% We store the ETS table ID across calls.
-record(state, {ets_table_id}).

%% External functions
start_link(EtsTableId) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [EtsTableId], []).

add_mapping({RecordName, FieldIds}) when is_atom(RecordName) ->
	[true = is_atom(FieldId) || FieldId <- FieldIds],
	server_call(add_mapping, {RecordName, FieldIds}).

get_mapping(RecordName) when is_atom(RecordName) ->
	[{RecordName, FieldIds}] = server_call(get_mapping, RecordName),
	FieldIds.

is_mapped(RecordName) when is_atom(RecordName) ->
	case server_call(get_mapping, RecordName) of
		[] ->
			false;
		[{RecordName, _}] ->
			true
	end;
is_mapped(Record) when is_tuple(Record) andalso size(Record) > 1 ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	case server_call(get_mapping, RecordName) of
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

set_field(Record, FieldId, {?TYPE_REF, Collection, ?ID_REF, Id}, MapReferenceFun) ->
	set_field(Record, FieldId, MapReferenceFun(Collection, Id), MapReferenceFun);
set_field(Record, FieldId, FieldValue, _MapReferenceFun) ->
	[RecordName|RecordList] = tuple_to_list(Record),
	FieldIds = get_mapping(RecordName),
	UpdatedRecordList = set_field(RecordList, FieldIds, FieldId, FieldValue, []),
	list_to_tuple([RecordName|UpdatedRecordList]).

get_type(Record) ->
	true = is_mapped(Record),
	[RecordName|_] = tuple_to_list(Record),
	RecordName.

map(Record) ->
	{Document, ChildDocs} = map_record(Record, []),
	{{get_type(Record), Document}, ChildDocs}.

unmap(RecordName, Tuple, MapReferenceFun) when is_atom(RecordName) ->
	FieldIds = get_mapping(RecordName),
	TupleList = tuple_to_list(Tuple),
	InitialTuple = list_to_tuple([RecordName] ++ lists:map(fun(_) -> undefined end, FieldIds)),
	unmap_record(TupleList, MapReferenceFun, InitialTuple).

map_selector(Selector) ->
	case is_mapped(Selector) of
		true ->
			RecordName = get_type(Selector),
			[{RecordName, FieldIds}] = server_call(get_mapping, RecordName),
			SelectorList = [{FieldId, get_field(Selector, FieldId)} || FieldId <- FieldIds],
			list_to_tuple(map_spm(SelectorList, false, []));
		false ->
			Selector
	end.
			
map_projection(Projection) ->
	map_selector(Projection).

map_modifier({ModifierKey, ModifierValue}) when is_tuple(ModifierValue) ->
	case is_mapped(ModifierValue) of
		false ->
			{ModifierKey, ModifierValue};
		true ->
			RecordName = get_type(ModifierValue),
			[{RecordName, FieldIds}] = server_call(get_mapping, RecordName),
			ModifierList = [{FieldId, get_field(ModifierValue, FieldId)} || FieldId <- FieldIds],
			{ModifierKey, list_to_tuple(map_spm(ModifierList, true, []))}
	end;
map_modifier(Modifier) when is_tuple(Modifier) ->
	case is_mapped(Modifier) of
		false ->
			Modifier;
		true ->
			RecordName = get_type(Modifier),
			[{RecordName, FieldIds}] = server_call(get_mapping, RecordName),
			ModifierList = [{FieldId, get_field(Modifier, FieldId)} || FieldId <- FieldIds],
			list_to_tuple(map_spm(ModifierList, true, []))
	end.

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
server_call(Command, Args) ->
	gen_server:call(?SERVER, {Command, Args}, infinity).

map_value(Value, DocList) when is_tuple(Value) ->
	case mongrel_mapper:is_mapped(Value) of
		true ->
			RecordName = get_type(Value),
			{MappedDoc, UpdatedDocList} = map_record(Value, DocList),
			case has_id(Value) of
				false ->
					MappedDocList = [?TYPE_REF, RecordName] ++ tuple_to_list(MappedDoc),
					{list_to_tuple(MappedDocList), UpdatedDocList};
				true ->
					{{?TYPE_REF, RecordName, ?ID_REF, get_field(Value, '_id')}, UpdatedDocList ++ [{RecordName, MappedDoc}]}
			end;
		false ->
			{Value, DocList}
	end;
map_value(Value, DocList) when is_list(Value) ->
	map_list(Value, DocList, []);
map_value(Value, DocList) ->
	{Value, DocList}.

map_record(Record, DocList) ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	FieldIds = get_mapping(RecordName),
	Result = [],
	{DocAsList, ChildDocs} = map_record(FieldIds, FieldValues, DocList, Result),
	{list_to_tuple(DocAsList), ChildDocs}.

map_record([], [], DocList, Result) ->
	{Result, DocList};
map_record([_FieldId|IdTail], [undefined|ValueTail], DocList, Result) ->
	map_record(IdTail, ValueTail, DocList, Result);
map_record([FieldId|IdTail], [FieldValue|ValueTail], DocList, Result) ->
	{ChildValue, UpdatedDocList} = map_value(FieldValue, DocList),
	map_record(IdTail, ValueTail, UpdatedDocList, Result ++ [FieldId, ChildValue]).

map_list([], DocList, Result) ->
	{Result, DocList};
map_list([Value|Tail], DocList, Result) ->
	{ChildValue, UpdatedDocList} = map_value(Value, DocList),
	map_list(Tail, UpdatedDocList, Result ++ [ChildValue]).

get_field([FieldId|_IdTail], [FieldValue|_ValuesTail], FieldId) ->
	FieldValue;
get_field([_FieldIdHead|IdTail], [_FieldValueHead|ValuesTail], FieldId) ->
	get_field(IdTail, ValuesTail, FieldId).

set_field([], [], _FieldId, _NewFieldValue, Result) ->
	Result;
set_field([_FieldValue|ValuesTail], [FieldId|_TailsList], FieldId, NewFieldValue, Result) ->
	Result ++ [NewFieldValue] ++ ValuesTail;
set_field([FieldValue|ValuesTail], [_FieldId|TailsList], FieldId, NewFieldValue, Result) ->
	set_field(ValuesTail, TailsList, FieldId, NewFieldValue, Result ++ [FieldValue]).

unmap_record([], _MapReferenceFun, Result) ->
	Result;
unmap_record([FieldId, FieldValue|FieldsTail], MapReferenceFun, Result) ->
	UnmappedValue = unmap_value(FieldValue, MapReferenceFun),
	unmap_record(FieldsTail, MapReferenceFun, set_field(Result, FieldId, UnmappedValue, MapReferenceFun)).

unmap_value(Value, MapReferenceFun) when is_tuple(Value) ->
	unmap_tuple(Value, MapReferenceFun);
unmap_value(Value, MapReferenceFun) when is_list(Value) ->
	unmap_list(Value, MapReferenceFun, []);
unmap_value(Value, _MapReferenceFun) ->
	Value.

unmap_tuple(Tuple, MapReferenceFun) ->
	TupleAsList = tuple_to_list(Tuple),
	case TupleAsList of
		[?TYPE_REF, Type, ?ID_REF, Id|_] ->
			unmap(Type, MapReferenceFun(Type, Id), MapReferenceFun);
		[?TYPE_REF, Type|Fields] ->
			unmap(Type, list_to_tuple(Fields), MapReferenceFun);
		_ ->
			Tuple
	end.

unmap_list([], _MapReferenceFun, Result) ->
	Result;
unmap_list([Value|ValueTail], MapReferenceFun, Result) ->
	unmap_list(ValueTail, MapReferenceFun, Result ++ [unmap_value(Value, MapReferenceFun)]).

map_spm([], _IsModifier, Result) ->
	Result;
map_spm([{_FieldId, undefined}|Tail], IsModifier, Result) ->
	map_spm(Tail, IsModifier, Result);
map_spm([{FieldId, FieldValue}|Tail], IsModifier, Result) ->
	case is_mapped(FieldValue) of
		false ->
			map_spm(Tail, IsModifier, Result ++ [FieldId, FieldValue]);
		true ->
			RecordType = get_type(FieldValue),
			case IsModifier of
				false ->
					MappedValueList = tuple_to_list(map_selector(FieldValue)),
					MappedIdValueList = concat_field_ids(FieldId, ['#type', RecordType] ++ MappedValueList, has_id(RecordType), []);
				true ->
					MappedValueList = tuple_to_list(map_modifier(FieldValue)),
					MappedIdValueList = concat_field_ids(FieldId, MappedValueList, has_id(RecordType), [])
			end,
			map_spm(Tail, IsModifier, Result ++ MappedIdValueList)
	end.

concat_field_ids(_FieldId, [], _HasId, Result) ->
	Result;
concat_field_ids(FieldId1, [FieldId2, FieldValue|Tail], false, Result) ->
	FieldId = list_to_atom(atom_to_list(FieldId1) ++ "." ++ atom_to_list(FieldId2)),
	concat_field_ids(FieldId1, Tail, false, Result ++ [FieldId, FieldValue]);
concat_field_ids(FieldId1, [FieldId2, FieldValue|Tail], true, Result) ->
	FieldId2List = atom_to_list(FieldId2),
	case FieldId2List of
		"#type" ->
			FieldId = list_to_atom(atom_to_list(FieldId1) ++ ".#type"),
			concat_field_ids(FieldId1, Tail, true, Result ++ [FieldId, FieldValue]);
		"_id" ++ IdTail ->
			FieldId = list_to_atom(atom_to_list(FieldId1) ++ ".#id" ++ IdTail),
			concat_field_ids(FieldId1, Tail, true, Result ++ [FieldId, FieldValue]);
		_ ->
			concat_field_ids(FieldId1, Tail, true, Result)
	end.
