-module(process_worker).

-behaviour(gen_server).

-compile({no_auto_import,[error/1]}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, start_link/0, set_neighbor/2, set_neighbors/2, set_behaviour/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

set_behaviour(Pid, BehaviourFun, BehaviourState) ->
	gen_server:cast(Pid, {set_behaviour, BehaviourFun, BehaviourState}).

%===============================================================================
% gen_server callbacks
%===============================================================================

init([]) ->
	lager:debug("Start ~p", [self()]),
	{ok, #{
		neighbors_set => [],
		behaviour_fun => undefined,
		behaviour_state => undefined
	}};

init(Args) ->
	lager:error("[init] nomatch Args: ~p", [Args]),
	error({nomatch, ?MODULE, ?LINE, Args}).

%===============================================================================

handle_call(Request, From, State) ->
	lager:error("[handle_call] nomatch From: ~p; Request: ~p", [From, Request]),
	Error = error({nomatch, ?MODULE, ?LINE, {From, Request}}),
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

handle_cast({set_behaviour, BehaviourFun, BehaviourState}, State) when is_function(BehaviourFun, 3) ->
	lager:debug("~p set process behaviour", [self()]),
	{noreply, State#{
		behaviour_fun => BehaviourFun,
		behaviour_state => BehaviourState
	}};

handle_cast(Message, State) ->
	lager:error("[handle_cast] nomatch Message: ~p", [Message]),
	{stop, error({nomatch, ?MODULE, ?LINE, Message}), State}.

%===============================================================================

handle_info(Message,
	#{
		neighbors_set := NeighborsSet,
		behaviour_state := BehaviourState,
		behaviour_fun := BehaviourFun
	} = State
) when is_function(BehaviourFun, 3) ->
	lager:debug("~p process message: ~p", [self(), Message]),
	case BehaviourFun(Message, NeighborsSet, BehaviourState) of
		ok ->
			{noreply, State};
		{ok, NewBehaviourState} ->
			{noreply, State#{behaviour_state => NewBehaviourState}}
	end;

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

error({error, Reason}) -> {error, Reason};
error({'EXIT', Reason}) -> {error, {'EXIT', Reason, erlang:get_stacktrace()}};
error(Reason) -> {error, Reason}.
