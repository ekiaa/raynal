-module(process).

-export([start/2, broadcast/1]).

-define(DEFAULT_NUMBER_OF_PROCESSES, 10).
-define(DEFAULT_NUMBER_OF_NEIGHBORS, 2).

start(BehaviourFun, BehaviourState) when is_function(BehaviourFun, 3) ->
	case application:ensure_started(?MODULE) of
		ok ->
			terminate_processes(),
			generate_processes(),
			set_behaviour(BehaviourFun, BehaviourState);
		{error, Reason} ->
			lager:debug("Reason: ~p", [Reason])
	end.

set_behaviour(BehaviourFun, BehaviourState) when is_function(BehaviourFun, 3) ->
	lists:foreach(
		fun(Pid) -> 
			process_worker:set_behaviour(Pid, BehaviourFun, BehaviourState) 
		end, 
		get_processes()
	).

broadcast(Start) when is_function(Start, 1) ->
	case get_processes() of
		[] ->
			{error, no_processes};
		Processes ->
			PN = length(Processes),
			RN = rand:uniform(PN),
			Pid = lists:nth(RN, Processes),
			Start(Pid),
			ok
	end.

terminate_processes() ->
	process_worker_sup:terminate_all_child(get_processes()).

generate_processes() ->
	generate_processes(?DEFAULT_NUMBER_OF_PROCESSES).
generate_processes(N) ->
	generate_processes(N, []).
generate_processes(0, Processes) ->
	application:set_env(?MODULE, processes, Processes),
	distribute_neighbors(Processes);
generate_processes(N, Processes) ->
	{ok, Pid} = process_worker:start(),
	generate_processes(N-1, [Pid | Processes]).

distribute_neighbors(Processes) ->
	N = erlang:length(Processes),
	distribute_neighbors(N, Processes, Processes).
distribute_neighbors(0, _, _) ->
	ok;
distribute_neighbors(N, [Pid | Rest], Processes) ->
	ListWithoutPid = lists:delete(Pid, Processes),
	Neighbors = get_random_noighbors(ListWithoutPid),
	process_worker:set_neighbors(Pid, Neighbors),
	distribute_neighbors(N-1, Rest, Processes).

get_random_noighbors(Processes) ->
	get_random_noighbors(?DEFAULT_NUMBER_OF_NEIGHBORS, Processes).
get_random_noighbors(N, Processes) ->
	get_random_noighbors(N, Processes, []).
get_random_noighbors(0, _, Neighbors) ->
	Neighbors;
get_random_noighbors(N, Processes, Neighbors) ->
	PN = erlang:length(Processes),
	RN = rand:uniform(PN),
	Pid = lists:nth(RN, Processes),
	ListWithoutPid = lists:delete(Pid, Processes),
	get_random_noighbors(N-1, ListWithoutPid, [Pid | Neighbors]).

get_processes() ->
	application:get_env(?MODULE, processes, []).