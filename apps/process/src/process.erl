-module(process).

-export([generate/0, generate/1, clean/0, get_random/0, get_state/0, print_state/0]).

-define(DEFAULT_NUMBER_OF_PROCESSES, 10).
-define(DEFAULT_NUMBER_OF_NEIGHBORS, 2).

-type options() :: #{
	process_amount := pos_integer(),
	neighbor_amount := pos_integer()
}.

-export_type([options/0]).

%===============================================================================
% API functions
%===============================================================================

-spec generate() -> ok.

generate() ->
	DefaultOptions = #{
		process_amount => ?DEFAULT_NUMBER_OF_PROCESSES,
		neighbor_amount => ?DEFAULT_NUMBER_OF_NEIGHBORS
	},
	generate(DefaultOptions).

%===============================================================================

-spec generate(options()) -> ok.

generate(Options) ->
	case get_processes() of
		[] ->
			generate_processes(Options);
		_ ->
			ok
	end.

%===============================================================================

-spec clean() -> ok.

clean() ->
	process_worker_sup:terminate_all_child(get_processes()).

%===============================================================================

-spec get_random() -> {ok, pid()} | {error, not_generated_processes}.

get_random() ->
	case get_processes() of
		[] ->
			{error, not_generated_processes};
		Processes ->
			PN = length(Processes),
 			RN = rand:uniform(PN),
 			Pid = lists:nth(RN, Processes),
 			{ok, Pid}
	end.

%===============================================================================

-spec get_state() -> list(raynal:state()).

get_state() ->
	[{Pid, process_worker:get_state(Pid)} || Pid <- get_processes()].

%===============================================================================

-spec print_state() -> ok.

print_state() ->
	StateList = get_state(),
	io:format("~p~n", [StateList]),
	ok.

%===============================================================================
% Internal functions
%===============================================================================

generate_processes(#{process_amount := N} = Options) ->
	generate_processes(N, Options).
generate_processes(N, Options) ->
	generate_processes(N, [], Options).
generate_processes(0, Processes, Options) ->
	application:set_env(?MODULE, processes, Processes),
	distribute_neighbors(Processes, Options);
generate_processes(N, Processes, Options) ->
	{ok, Pid} = process_worker:start(),
	generate_processes(N-1, [Pid | Processes], Options).

distribute_neighbors(Processes, Options) ->
	N = erlang:length(Processes),
	distribute_neighbors(N, Processes, Processes, Options).
distribute_neighbors(0, _, _, _) ->
	ok;
distribute_neighbors(N, [Pid | Rest], Processes, Options) ->
	ListWithoutPid = lists:delete(Pid, Processes),
	Neighbors = get_random_noighbors(ListWithoutPid, Options),
	process_worker:set_neighbors(Pid, Neighbors),
	distribute_neighbors(N-1, Rest, Processes, Options).

get_random_noighbors(Processes, #{neighbor_amount := N}) ->
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