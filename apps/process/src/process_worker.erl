-module(process_worker).

-behaviour(gen_server).

-compile({no_auto_import,[error/1]}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, start_link/0, set_neighbor/2, set_neighbors/2, get_state/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type state() :: #{
	'raynal_state' := raynal:state(),
	'neighbors_set' := raynal:neighbors()
}.

-export_type([state/0]).

%===============================================================================
% API functions
%===============================================================================

start() ->
	process_worker_sup:start_child().

start_link() ->
	gen_server:start_link(?MODULE, [], []).

set_neighbor(Pid, Neighbor) ->
	gen_server:cast(Pid, {neighbor, Neighbor}).

set_neighbors(Pid, Neighbors) ->
	gen_server:cast(Pid, {neighbors, Neighbors}).

-spec get_state(pid()) -> raynal:state().

get_state(Pid) ->
	gen_server:call(Pid, get_state).

%===============================================================================
% gen_server callbacks
%===============================================================================

-spec init([]) -> {'ok', state()}.

init([]) ->
	lager:debug("Start ~p", [self()]),
	RaynalState = raynal:init_state(),
	{ok, #{
		neighbors_set => sets:new(),
		raynal_state => RaynalState
	}};

init(Args) ->
	lager:error("[init] nomatch Args: ~p", [Args]),
	{stop, error_msg({nomatch, ?MODULE, ?LINE, Args})}.

%===============================================================================

handle_call(get_state, _, #{raynal_state := RaynalState} = State) ->
	{reply, RaynalState, State};

handle_call(Request, From, State) ->
	lager:error("[handle_call] nomatch From: ~p; Request: ~p", [From, Request]),
	Error = error_msg({nomatch, ?MODULE, ?LINE, {From, Request}}),
	{stop, Error, Error, State}.

%===============================================================================

handle_cast({neighbor, Neighbor}, #{neighbors_set := NeighborsSet} = State) ->
	lager:debug("~p set neighbor: ~p", [self(), Neighbor]),
	{noreply, State#{neighbors_set => sets:add_element(Neighbor, NeighborsSet)}};

handle_cast({neighbors, Neighbors}, State) when is_list(Neighbors) ->
	NeighborsSet = sets:from_list(Neighbors),
	NList = sets:to_list(NeighborsSet),
	lager:debug("~p set neighbors: ~p", [self(), NList]),
	lists:foreach(fun(Pid) -> set_neighbor(Pid, self()) end, NList),
	{noreply, State#{neighbors_set => NeighborsSet}};

handle_cast(Message, State) ->
	lager:error("[handle_cast] nomatch Message: ~p", [Message]),
	{stop, error_msg({nomatch, ?MODULE, ?LINE, Message}), State}.

%===============================================================================

-spec handle_info(RaynalMessage :: raynal:message(), State :: state()) -> {'noreply', state()}.

handle_info({raynal, _} = RaynalMessage, 
	#{
		raynal_state := RaynalState,
		neighbors_set := Neighbors
	} = State
) ->
	{noreply, State#{
		raynal_state => raynal:handle_msg(RaynalMessage, Neighbors, RaynalState)
	}};

handle_info(Message, State) ->
	lager:debug("~p not have process behaviour; Message: ~p", [self(), Message]),
	{noreply, State}.
	
%===============================================================================

terminate(normal, _State) ->
	ok;

terminate(Reason, _State) ->
	lager:error("[terminate] Reason: ~p", [Reason]),
	ok.

%===============================================================================

code_change(OldVsn, State, Extra) ->
	lager:debug("[code_change] OldVsn: ~p; Extra", [OldVsn, Extra]),
	{ok, State}.

%===============================================================================
% Internal functions
%===============================================================================

%===============================================================================

-spec error_msg(Reason :: any()) -> {'error', any()}.

error_msg(Reason) -> {error, Reason}.
