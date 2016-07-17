-module(rst_flood).

-export([start/0, start/1, process/3]).

-define(DEFAULT_NUMBER_OF_PROCESSES, 10).
-define(DEFAULT_NUMBER_OF_NEIGHBORS, 2).

start() ->
	case application:ensure_started(process) of
		ok ->
			process:start(fun rst_flood:process/3, undefined),
			process:broadcast(fun rst_flood:start/1);
		{error, Reason} ->
			lager:debug("[start] application:ensure_started(process) return: ~p", [Reason])
	end.

start(Pid) ->
	Fun = fun(_) -> self() end,
	Pid ! {'START', self(), Fun},
	receive
		{'RESULT', Result} ->
			Processes = sets:to_list(Result),
			lager:debug("Processes: ~p", [Processes]),
			ok
	end.

process(Message, NeighborsSet, undefined) ->
	process(Message, NeighborsSet,
		#{
			parent => undefined,
			expected_msg => undefined,
			reply_to => undefined,
			val_set => undefined
		});

process({'START', From, Fun}, NeighborsSet,
	#{
		parent := undefined,
		expected_msg := undefined
	} = State
) ->
	lager:debug("~p START", [self()]),
	ValSet = sets:from_list([Fun(State)]),
	N = sets:size(NeighborsSet),
	NList = sets:to_list(NeighborsSet),
	lists:foreach(fun(Pid) -> Pid ! {'GO', self(), Fun} end, NList),
	{ok, State#{
		parent => self(),
		expected_msg => N,
		val_set => ValSet,
		reply_to => From
	}};

process({'GO', From, Fun}, NeighborsSet,
	#{
		parent := undefined,
		expected_msg := undefined
	} = State
) ->
	lager:debug("~p GO from ~p", [self(), From]),
	ValSet = sets:from_list([Fun(State)]),
	N = sets:size(NeighborsSet) - 1,
	case N of
		0 ->
			From ! {'BACK', self(), ValSet};
		_ ->
			NList = sets:to_list(sets:del_element(From, NeighborsSet)),
			lists:foreach(fun(Pid) -> Pid ! {'GO', self(), Fun} end, NList)
	end,
	{ok, State#{
		parent => From,
		expected_msg => N,
		val_set => ValSet
	}};

process({'GO', From, _Fun}, NeighborsSet, State) ->
	lager:debug("~p GO from ~p; send empty BACK", [self(), From]),
	From ! {'BACK', self(), sets:new()},
	ok;

process({'BACK', From, Val}, NeighborsSet,
	#{
		parent := Parent,
		expected_msg := N,
		val_set := ValSet,
		reply_to := ReplyTo
	} = State
) ->
	lager:debug("~p BACK from ~p; Val size: ~p", [self(), From, sets:size(Val)]),
	NewN = N - 1,
	NewValSet = sets:union(Val, ValSet),
	case NewN of
		0 ->
			case self() of
				Parent ->
					ReplyTo ! {'RESULT', NewValSet},
					{ok, State#{
						parent => undefined,
						expected_msg => undefined,
						val_set => undefined,
						reply_to => undefined
					}};
				_ ->
					Parent ! {'BACK', self(), NewValSet},
					{ok, State#{
						parent => undefined,
						expected_msg => undefined,
						val_set => undefined
					}}
			end;
		_ ->
			{ok, State#{
				expected_msg => NewN,
				val_set => NewValSet
			}}
	end;

process(Message, Neighbors, State) ->
	lager:debug("~p process message: ~p; Neighbors: ~p; State: ~p", [self(), Message, Neighbors, State]),
	ok.
