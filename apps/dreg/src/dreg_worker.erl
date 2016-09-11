-module(dreg_worker).

-behaviour(gen_statem).

% gen_statem callbacks
-export([init/1, handle_event/4, terminate/3, code_change/4]).

% API functions
-export([start/0, start_link/0]).

%===============================================================================
% API functions
%===============================================================================

start() ->
	dreg_worker_sup:start_child([]).

start_link() ->
	gen_statem:start_link(?MODULE, [], []).

%===============================================================================
% gen_statem callbacks
%===============================================================================

init([]) ->
	State = idle,
	RaynalState = raynal:init_state(),
	Data = #{
		neighbors_set => sets:new(),
		raynal_state => RaynalState
	},
	{handle_event_function, State, Data}.

handle_event(_EventType, _EventContent, _State, _Data) ->
	lager:debug("[~p in ~p] not matched event: ~p", [?MODULE, _State, _EventContent]),
	keep_state_and_data.

terminate(_Reason, _State, _Data) ->
	lager:debug("[~p in ~p] terminated: ~p", [?MODULE, _State, _Reason]),
	ok.

code_change(_Vsn, State, Data, _Extra) ->
	{handle_event_function, State, Data}.