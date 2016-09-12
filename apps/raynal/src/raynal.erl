-module(raynal).

-export([
	init_state/0,
	set_process_state/2,
	traverse/4,
	build/3,
	clean/3,
	handle_msg/3,
	send_message/3,
	send_message/5
]).

-type command() :: build | traverse | clean.
-type process() :: pid().
-type algorithm() :: rst | bfst.
-type key() :: atom() | binary() | string().
-type message() ::  
	#{
		command := command(),
		from := process(),
		key := key(),
		algorithm := algorithm(),
		message := term()
	}.
-type tagged_message() :: {raynal, message()}.

-type neighbors() :: sets:set(pid()).

-type algorithm_state() :: undefined | bfst:state() | rst:state().
-type process_state() :: term().
-type traverse_result() :: term().

-type algorithm_handle_msg_result() :: ok
									 | {ok, {algorithm, algorithm_state()}}
									 | {ok, {process, process_state()}}
									 | {ok, algorithm_state(), process_state()}.

-type algorithm_map() :: #{
	algorithm() => algorithm_state()
}.
-type state() :: #{
	process_state := process_state(),
	key() => algorithm_map()
}.

-export_type([
	command/0,
	process/0,
	algorithm/0,
	key/0,
	message/0,
	tagged_message/0,
	neighbors/0,
	algorithm_state/0,
	state/0,
	algorithm_handle_msg_result/0,
	process_state/0,
	traverse_result/0
]).

-define(BUILD_TIMEOUT, 30000).

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
	Algorithm :: algorithm(),
	ProcessPid :: process(),
	Key :: key(),
	CallbackModule :: atom()
) -> {ok, term()} | {error, term()}.

traverse(Algorithm, ProcessPid, Key, CallbackModule) when is_pid(ProcessPid), is_atom(CallbackModule) ->
	lager:debug("~p try traverse from root ~p with callback module ~p", [self(), ProcessPid, CallbackModule]),
	send_message(ProcessPid, traverse, Key, Algorithm, {'START', CallbackModule}),
	receive
		{raynal, #{algorithm := Algorithm, key := Key, message := {'RESULT', Result}}} ->
			lager:debug("~p receive result: ~p", [self(), Result]),
			{ok, Result}
	after
		?BUILD_TIMEOUT ->
			{error, timeout}
	end.

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
	Command :: command(),
	Key :: key(),
	Algorithm :: algorithm(),
	SentMessage :: term()
) -> tagged_message().

send_message(ProcessPid, Command, Key, Algorithm, SentMessage) ->
	ProcessPid ! {raynal, #{command => Command, from => self(), key => Key, algorithm => Algorithm, message => SentMessage}}.

-spec send_message(
	ProcessPid :: process(),
	SentMessage :: term(),
	PreviousMessage :: message()
) -> tagged_message().

send_message(ProcessPid, SentMessage, PreviousMessage) ->
	ProcessPid ! {raynal, PreviousMessage#{from => self(), message => SentMessage}}.

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