-module(dreg_worker).

-behaviour(gen_statem).

-type name() :: atom().
-type key() :: binary().
-type state() :: idle | building.
-type data() :: #{
	name => name(),
	neighbors_set => sets:set(),
	raynal_state => raynal:state(),
	key => key()
}.

-export_type([name/0]).

% gen_statem callbacks
-export([init/1, handle_event/4, terminate/3, code_change/4]).

% API functions
-export([start/1, start_link/1, add_neighbor/2]).

%===============================================================================
% API functions
%===============================================================================

-spec start(Name :: name()) -> supervisor:startchild_ret().

start(Name) ->
	dreg_worker_sup:start_child([Name]).

-spec start_link(Name :: name()) -> gen_statem:start_ret().

start_link(Name) ->
	gen_statem:start_link({local, Name}, ?MODULE, {name, Name}, []).

-spec add_neighbor(Name :: name(), Neighbor :: name() | pid()) -> ok.

add_neighbor(Name, Neighbor) when is_atom(Neighbor) ->
	case erlang:whereis(Neighbor) of
		Pid when is_pid(Pid) ->
			add_neighbor(Name, Pid);
		Result ->
			lager:error("[add_neighbor] not matched erlang:whereis(Neighbor) result: ~p; Neighbor: ~p", [Result, Neighbor]),
			{error, not_found}
	end;

add_neighbor(Name, Neighbor) when is_pid(Neighbor) ->
	gen_statem:cast(Name, {add_neighbor, Neighbor}).

%===============================================================================
% gen_statem callbacks
%===============================================================================

-spec init(Args :: {name, name()}) -> {handle_event_function, state(), data()}.

init({name, Name}) ->
	State = idle,
	RaynalState = raynal:init_state(),
	Key = get_key(Name),
	Data = #{
		name => Name,
		neighbors_set => sets:new(),
		raynal_state => RaynalState,
		key => Key
	},
	{handle_event_function, State, Data}.

%===============================================================================

-spec handle_event(
	EventType :: gen_statem:event_type(),
	EventContent :: term(),
	State :: state(),
	Data :: data()
) -> 
	  keep_state_and_data 
	| {keep_state, data()}
	| {keep_state_and_data, {postpone, true}}
	| {next_state, state(), data()}.

%-------------------------------------------------------------------------------

handle_event(cast, {add_neighbor, Neighbor}, idle, _Data) ->
	Neighbor ! {set_neighbor, first, self()},
	keep_state_and_data;

handle_event(cast, {add_neighbor, _}, _State, _Data) ->
	{keep_state_and_data, {postpone, true}};

handle_event(info, {set_neighbor, first, Neighbor}, _State, Data) ->
	Neighbor ! {set_neighbor, second, self()},
	#{neighbors_set := Neighbors} = Data,
	lager:debug("[~p] (first) add neighbor: ~p", [_State, Neighbor]),
	{keep_state, Data#{neighbors_set => sets:add_element(Neighbor, Neighbors)}};

handle_event(info, {set_neighbor, second, Neighbor}, idle = _State, Data) ->
	#{neighbors_set := Neighbors, name := Name, key := OldKey} = Data,
	lager:debug("[~p] (second) add neighbor: ~p", [_State, Neighbor]),
	Self = self(),
	NewKey = get_key(Name),
	spawn(fun() ->
		raynal:clean(rst, Self, OldKey),
		raynal:build(rst, Self, NewKey),
		Self ! building_stop
	end),
	lager:debug("[~p] run raynal:build(rst)", [_State]),
	{next_state, building, 
		Data#{
			neighbors_set => sets:add_element(Neighbor, Neighbors),
			key => NewKey
		}
	};

handle_event(info, {set_neighbor, second, _}, _State, _Data) ->
	{keep_state_and_data, {postpone, true}};

%-------------------------------------------------------------------------------

handle_event(info, building_stop = Message, building = _State, Data) ->
	lager:debug("[~p] receive: ~p", [_State, Message]),
	#{key := Key} = Data,
	Self = self(),
	spawn(fun() ->
		Res = raynal:traverse(rst, Self, Key, dreg_worker_building_traverse),
		Self ! {traverse, Res}
	end),
	keep_state_and_data;

handle_event(info, {traverse, Result} = Message, building = _State, Data) ->
	lager:debug("[~p] receive: ~p", [_State, Message]),
	{next_state, idle, Data};

handle_event(info, {raynal, _} = RaynalMessage, _State, Data) ->
	lager:debug("[~p] receive: ~p", [_State, RaynalMessage]),
	#{raynal_state := RaynalState, neighbors_set := Neighbors} = Data,
	{keep_state, Data#{
		raynal_state => raynal:handle_msg(RaynalMessage, Neighbors, RaynalState)
	}};

%-------------------------------------------------------------------------------

handle_event(info, rebuild, idle = _State, Data) ->
	lager:debug("[~p] rebuild tree", [_State]),
	#{neighbors_set := Neighbors, name := Name, key := OldKey} = Data,
	Self = self(),
	NewKey = get_key(Name),
	spawn(fun() ->
		raynal:clean(rst, Self, OldKey),
		raynal:build(rst, Self, NewKey),
		Self ! rebuild_stop
	end),
	lager:debug("[~p] run raynal:build(rst)", [_State]),
	{next_state, building, 
		Data#{
			key => NewKey
		}
	};

handle_event(info, rebuild_stop, building = _State, Data) ->
	lager:debug("[~p] rebuild tree stop", [_State]),
	{next_state, idle, Data};

%-------------------------------------------------------------------------------

handle_event(_EventType, _EventContent, _State, _Data) ->
	lager:debug("[~p] not matched event: ~p", [_State, _EventContent]),
	keep_state_and_data.

%===============================================================================

terminate(_Reason, _State, _Data) ->
	lager:debug("[~p] terminated: ~p~n~p", [_State, _Reason, erlang:get_stacktrace()]),
	ok.

%===============================================================================

code_change(_Vsn, State, Data, _Extra) ->
	{handle_event_function, State, Data}.

%===============================================================================
% Internal functions
%===============================================================================

-spec get_key(name()) -> key().

get_key(Name) ->
	<<(atom_to_binary(Name, utf8))/binary, "-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>.