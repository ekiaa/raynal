-module(raynal).

-export([
	init_state/0,
	set_process_state/2,
	traverse/4,
	build/3,
	clean/3,
	handle_msg/3,
	send_message/3,
	send_message/5,
	get_message/4,
	get_message/2,
	get_method_message/1
]).

-type process() :: pid().
-type key() :: atom() | binary() | string().
-type neighbors() :: sets:set(pid()).
-type process_state() :: term().
% -type traverse_result() :: term().

-type method() :: 
	  raynal_build:method() 
	| raynal_traverse:method() 
	| raynal_clean:method().
-type method_message() ::
	  raynal_build:message()
	| raynal_traverse:message()
	| raynal_clean:message().
-type method_state() ::
	  raynal_build:state()
	| raynal_traverse:state()
	| raynal_clean:state().

-type algorithm() :: 
	  raynal_rst:algorithm()
	| raynal_bfst:algorithm().
-type algorithm_state() :: 
	  raynal_rst:state()
	| raynal_bfst:state().

-type message() ::  
	#{
		method := method(),
		from := process(),
		key := key(),
		algorithm := algorithm(),
		message := method_message()
	}.
-type tagged_message() :: {raynal, message()}.

-type algorithm_handle_msg_result() :: 
	  ok
	| {ok, {algorithm, algorithm_state()}}
	| {ok, {process, process_state()}}
	| {ok, algorithm_state(), process_state()}.

-type algorithm_state_map() :: 
	#{
		algorithm() => algorithm_state()
	}.
-type method_state_map() :: 
	#{
		reference() => method_state()
	}.
-type key_map() :: 
	#{
		key() => algorithm_state_map()
	}.

-type state() :: 
	#{
		process_state := process_state(),
		method_state_map := method_state_map(),
		key_map := key_map()
	}.

-export_type([
	method/0,
	process/0,
	algorithm/0,
	key/0,
	message/1,
	tagged_message/1,
	neighbors/0,
	algorithm_state/0,
	state/0,
	algorithm_handle_msg_result/0,
	process_state/0
	% ,
	% traverse_result/0
]).

-define(BUILD_TIMEOUT, 30000).

%===============================================================================
% Type functions
%===============================================================================

-spec message(
	Method :: method(),
	Key :: key(),
	Algorithm :: algorithm(),
	MethodMessage :: method_message()
) -> message().

message(Method, Key, Algorithm, MethodMessage) ->
	#{
		method => Method,
		from => self(),
		key => Key,
		algorithm => Algorithm,
		message => MethodMessage
	}.

-spec message(
	PreviousMessage :: message(),
	MethodMessage :: method_message()
) -> message().

message(PreviousMessage, MethodMessage) ->
	PreviousMessage#{
		from => self(),
		message => MethodMessage
	}.

%-------------------------------------------------------------------------------

-spec tagged_message(
	Message :: message()
) -> tagged_message().

tagged_message(Message) ->
	{raynal, Message}.

%-------------------------------------------------------------------------------

-spec state(
	ProcessState :: process_state(),
	MethodStateMap :: method_state_map(),
	KeyMap :: key_map()
) -> state().

state(
	{process_state, _} = ProcessState,
	{method_state_map, _} = MethodStateMap,
	{key_map, _} = KeyMap
) ->
	#{
		process_state => ProcessState,
		method_state_map => MethodStateMap,
		key_map => KeyMap
	}.

-spec state(ProcessState :: process_state(), State :: state()) -> state()
		;  (MethodStateMap :: method_state_map(), State :: state()) -> state()
		;  (KeyMap :: key_map(), State :: state()) -> state().

state({process_state, _} = ProcessState, State) ->
	State#{process_state => ProcessState};
state({method_state_map, _} = MethodStateMap, State) ->
	State#{method_state_map => MethodStateMap};
state({key_map, _} = KeyMap, State) ->
	State#{key_map => KeyMap}.

%===============================================================================
% API functions
%===============================================================================

-spec init_state() -> state().

init_state() ->
	#{
		process_state => undefined
	}.

%===============================================================================

-spec set_process_state(RaynalState :: state(), ProcessState :: process_state()) -> state().

set_process_state(RaynalState, ProcessState) ->
	RaynalState#{
		process_state => ProcessState
	}.

%===============================================================================

-spec traverse(
	ProcessPid :: process(),
	Key :: key(),
	Algorithm :: algorithm(),
	CallbackModule :: atom(),
	Params :: term()
) -> reference().

traverse(ProcessPid, Key, Algorithm, CallbackModule, Params) when is_pid(ProcessPid), is_atom(CallbackModule) ->
	lager:debug("~p try traverse from root ~p with callback module ~p", [self(), ProcessPid, CallbackModule]),
	Ref = erlang:make_ref(),
	raynal_traverse:send_message(ProcessPid, Key, Algorithm, Ref, CallbackModule, self(), Params, 'START'),
	Ref.

%===============================================================================

-spec build(
	Algorithm :: algorithm(),
	ProcessPid :: process(),
	Key :: key()
) -> ok | {error, term()}.

build(Algorithm, ProcessPid, Key) when is_pid(ProcessPid) ->
	lager:debug("~p try build ~p for ~p with key ~p", [self(), Algorithm, ProcessPid, Key]),
	send_message(ProcessPid, build, Key, Algorithm, 'START'),
	receive
		{raynal, #{algorithm := Algorithm, key := Key, message := {'RESULT', Result}}} ->
			lager:debug("~p receive result: ~p", [self(), Result]),
			ok
	after
		?BUILD_TIMEOUT ->
			{error, timeout}
	end.

%===============================================================================

-spec clean(
	Algorithm :: algorithm(),
	ProcessPid :: process(),
	Key :: key()
) -> ok.

clean(Algorithm, ProcessPid, Key) when is_pid(ProcessPid) ->
	lager:debug("~p try clean ~p for ~p with key ~p", [self(), Algorithm, ProcessPid, Key]),
	send_message(ProcessPid, clean, Key, Algorithm, 'CLEAN'),
	ok.

%===============================================================================

-spec handle_msg(
	Message :: tagged_message(),
	Neighbors :: neighbors(),
	RaynalState :: state()
) -> state().

handle_msg({raynal, #{algorithm := Algorithm} = Message}, Neighbors, #{process_state := ProcessState} = RaynalState) ->
	AlgorithmState = get_algorithm_state(Message, RaynalState),
	case Algorithm:handle_msg(Message, Neighbors, AlgorithmState, ProcessState) of
		ok ->
			RaynalState;
		{ok, {algorithm, NewAlgorithmState}} ->
			put_algorithm_state(Message, RaynalState, NewAlgorithmState);
		{ok, {process, NewProcessState}} ->
			RaynalState#{process_state => NewProcessState};
		{ok, NewAlgorithmState, NewProcessState} ->
			put_algorithm_state(Message, RaynalState#{process_state => NewProcessState}, NewAlgorithmState);
		clean ->
			clean_algorithm_state(Message, RaynalState)
	end.

%===============================================================================

-spec send_message(
	ProcessPid :: process(),
	Method :: method(),
	Key :: key(),
	Algorithm :: algorithm(),
	MethodMessage :: MethodMessageType
) -> ok.

send_message(ProcessPid, Method, Key, Algorithm, MethodMessage) ->
	ProcessPid ! get_message(Method, Key, Algorithm, MethodMessage).

-spec send_message(
	ProcessPid :: process(),
	PreviousMessage :: message(_),
	MethodMessage :: MethodMessageType
) -> tagged_message(MethodMessageType).

send_message(ProcessPid, PreviousMessage, MethodMessage) ->
	ProcessPid ! get_message(PreviousMessage, MethodMessage).

%-------------------------------------------------------------------------------

-spec get_method_message(Message :: message(MethodMessageType)) -> MethodMessageType.

get_method_message(#{message := MethodMessage}) ->
	MethodMessage.

%===============================================================================
% Internal functions
%===============================================================================

-spec get_algorithm_state(message(), state()) -> algorithm_state().

get_algorithm_state(#{key := Key, algorithm := Algorithm}, RaynalState) ->
	case maps:get(Key, RaynalState, undefined) of
		undefined ->
			undefined;
		AlgorithmMap ->
			maps:get(Algorithm, AlgorithmMap, undefined)
	end.

-spec put_algorithm_state(message(), state(), algorithm_state()) -> state().

put_algorithm_state(#{key := Key, algorithm := Algorithm}, RaynalState, AlgorithmState) ->
	case maps:get(Key, RaynalState, undefined) of
		undefined ->
			RaynalState#{Key => #{Algorithm => AlgorithmState}};
		AlgorithmMap ->
			RaynalState#{Key => AlgorithmMap#{Algorithm => AlgorithmState}}
	end.

-spec clean_algorithm_state(message(), state()) -> state().

clean_algorithm_state(#{key := Key, algorithm := Algorithm}, RaynalState) ->
	case maps:get(Key, RaynalState, undefined) of
		undefined ->
			RaynalState;
		AlgorithmMap ->
			NewAlgorithmMap = maps:remove(Algorithm, AlgorithmMap),
			case maps:size(NewAlgorithmMap) of
				0 ->
					maps:remove(Key, RaynalState);
				_ ->
					RaynalState#{Key => NewAlgorithmMap}
			end
	end.