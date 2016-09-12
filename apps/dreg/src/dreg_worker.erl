-module(dreg_worker).

-behaviour(gen_statem).

-type name() :: atom().
-type state() :: idle.
-type data() :: #{
	name := name(),
	neighbors_set := sets:set(),
	raynal_state := raynal:state()
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
	Data = #{
		name => Name,
		neighbors_set => sets:new(),
		raynal_state => RaynalState
	},
	{handle_event_function, State, Data}.

%===============================================================================

-spec handle_event(
	EventType :: gen_statem:event_type(),
	EventContent :: term(),
	State :: state(),
	Data :: data()
) -> keep_state_and_data | {keep_state, data()}.

handle_event(cast, {add_neighbor, Neighbor}, _State, _Data) ->
	Neighbor ! {set_neighbor, first, self()},
	keep_state_and_data;

handle_event(info, {set_neighbor, first, Neighbor}, State, Data) ->
	Neighbor ! {set_neighbor, second, self()},
	set_neighbor(Neighbor, State, Data);

handle_event(info, {set_neighbor, second, Neighbor}, State, Data) ->
	set_neighbor(Neighbor, State, Data);

handle_event(_EventType, _EventContent, _State, _Data) ->
	lager:debug("[~p] not matched event: ~p", [_State, _EventContent]),
	keep_state_and_data.

%===============================================================================

terminate(_Reason, _State, _Data) ->
	lager:debug("[~p] terminated: ~p", [_State, _Reason]),
	ok.

%===============================================================================

code_change(_Vsn, State, Data, _Extra) ->
	{handle_event_function, State, Data}.

%===============================================================================
% Internal functions
%===============================================================================

-spec set_neighbor(Neighbor :: pid(), State :: state(), Data :: data()) -> keep_state_and_data | {keep_state, data()}.

set_neighbor(Neighbor, _State, #{neighbors_set := Neighbors} = Data) ->
	case sets:is_element(Neighbor, Neighbors) of
		false ->
			lager:debug("[~p] add neighbor: ~p", [_State, Neighbor]),
			{keep_state, Data#{neighbors_set => sets:add_element(Neighbor, Neighbors)}};
		true ->
			lager:debug("[~p] ~p already is neighbor", [_State, Neighbor]),
			keep_state_and_data
	end.