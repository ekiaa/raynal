-module(raynal).

-export([init_state/0, set_process_state/2, traversal/2, build/3, handle_msg/3, send_message/4]).

-type neighbors() :: sets:set(pid()).
-type algorithm() :: rst | bfst.
-type key() :: atom() | binary() | string().
-type message() :: {raynal, {algorithm(), {key(), term()}}}.
-type internal_state() :: bfst:state() | rst:state().

-type algorithm_handle_msg_result() :: ok
									 | {internal, internal_state()}
									 | {external, term()}
									 | {ok, internal_state(), term()}.

-type state() :: #{
	process_state := term(),
	key() => internal_state()
}.

-export_type([neighbors/0, state/0, message/0]).
-export_type([key/0, internal_state/0, algorithm_handle_msg_result/0]).

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

-spec set_process_state(RaynalState :: state(), ProcessState :: term()) -> state().

set_process_state(RaynalState, ProcessState) ->
	RaynalState#{
		process_state => ProcessState
	}.

%===============================================================================

-spec traversal(pid(), fun()) -> ok.

traversal(ProcessPid, Fun) ->
	ok.

%===============================================================================

-spec build(Algorithm :: algorithm(), ProcessPid :: pid(), Key :: key()) -> ok | {error, term()}.

build(Algorithm, ProcessPid, Key) when is_pid(ProcessPid) ->
	lager:debug("~p try build ~p for ~p with key ~p", [self(), Algorithm, ProcessPid, Key]),
	send_message(ProcessPid, Algorithm, Key, {'START', self()}),
	receive
		{raynal, {Algorithm, {Key, {'RESULT', Result}}}} ->
			lager:debug("~p receive result: ~p", [self(), Result]),
			ok
	after
		?BUILD_TIMEOUT ->
			{error, timeout}
	end.

%===============================================================================

-spec handle_msg(
	Message :: message(),
	Neighbors :: neighbors(),
	RaynalState :: state()
) -> state().

handle_msg({raynal, {Algorithm, {Key, Message}}}, Neighbors, #{process_state := ProcessState} = RaynalState) ->
	State = maps:get(Key, RaynalState, undefined),
	case Algorithm:handle_msg(Key, Message, Neighbors, State, ProcessState) of
		ok ->
			RaynalState;
		{internal, NewState} ->
			RaynalState#{Key => NewState};
		{external, NewProcessState} ->
			RaynalState#{process_state => NewProcessState};
		{ok, NewState, NewProcessState} ->
			RaynalState#{Key => NewState, process_state => NewProcessState}
	end.

%===============================================================================

-spec send_message(
	ProcessPid :: pid(),
	Algorithm :: algorithm(),
	Key :: key(),
	Message :: term()
) -> message().

send_message(ProcessPid, Algorithm, Key, Message) ->
	ProcessPid ! {raynal, {Algorithm, {Key, Message}}}.

%===============================================================================
% Internal functions
%===============================================================================