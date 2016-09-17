-module(dreg_worker_building_traverse).

-behaviour(raynal_traverse).

-export([execute/1, merge/3]).

execute(State) ->
	lager:debug("[execute] State: ~p", [State]),
	{ok, State}.

merge(State, Result, ChildResult) ->
	lager:debug("[merge] State: ~p; Result: ~p; ChildResult: ~p", [State, Result, ChildResult]),
	self() ! rebuild,
	{ok, State}.

