-module(rst_flood).

-export([handle_msg/5]).

-type state() :: #{
	parent       := undefined | pid(),
	val_set      := undefined | sets:set({pid(), term()}),
	expected_msg := undefined | non_neg_integer(),
	reply_to     := undefined | pid()
}.

-export_type([state/0]).

-spec handle_msg(
	Key :: raynal:key(),
	Message :: term(),
	Neighbors :: raynal:neighbors(),
	State :: undefined | state(),
	ProcessState :: term()
) -> raynal:algorithm_handle_msg_result().

handle_msg(Key, Message, Neighbors, undefined, ProcessState) ->
	handle_msg(Key, Message, Neighbors,
		#{
			parent => undefined,
			expected_msg => undefined,
			reply_to => undefined,
			val_set => undefined
		},
		ProcessState
	);

handle_msg(Key, {'START', ReplyTo, Fun}, Neighbors,
	#{
		parent := undefined,
		expected_msg := undefined
	} = State, ProcessState
) ->
	lager:debug("~p START; ReplyTo: ~p", [self(), ReplyTo]),
	{Result, NewProcessState} = Fun(ProcessState),
	ValSet = sets:from_list([{self(), Result}]),
	N = sets:size(Neighbors),
	lists:foreach(
		fun(Pid) -> raynal:send_message(Pid, ?MODULE, Key, {'GO', self(), Fun}) end,
		sets:to_list(Neighbors)
	),
	{ok, State#{
		parent => self(),
		expected_msg => N,
		val_set => ValSet,
		reply_to => ReplyTo
	}, NewProcessState};

handle_msg(Key, {'GO', From, Fun}, Neighbors,
	#{
		parent := undefined,
		expected_msg := undefined
	} = State, ProcessState
) ->
	lager:debug("~p GO from ~p", [self(), From]),
	{Result, NewProcessState} = Fun(ProcessState),
	ValSet = sets:from_list([{self(), Result}]),
	N = sets:size(Neighbors) - 1,
	case N of
		0 ->
			raynal:send_message(From, ?MODULE, Key, {'BACK', self(), ValSet}),
			{external, NewProcessState};
		_ ->
			lists:foreach(
				fun(Pid) -> raynal:send_message(Pid, ?MODULE, Key, {'GO', self(), Fun}) end,
				sets:to_list(sets:del_element(From, Neighbors))
			),
			{ok, State#{
				parent => From,
				expected_msg => N,
				val_set => ValSet
			}, NewProcessState}
	end;

handle_msg(Key, {'GO', From, _Fun}, Neighbors, State, ProcessState) ->
	lager:debug("~p GO from ~p; send empty BACK", [self(), From]),
	raynal:send_message(From, ?MODULE, Key, {'BACK', self(), sets:new()}),
	ok;

handle_msg(Key, {'BACK', From, Val}, Neighbors,
	#{
		parent := Parent,
		expected_msg := N,
		val_set := ValSet,
		reply_to := ReplyTo
	} = State, ProcessState
) ->
	lager:debug("~p BACK from ~p; Val size: ~p", [self(), From, sets:size(Val)]),
	NewN = N - 1,
	NewValSet = sets:union(Val, ValSet),
	case NewN of
		0 ->
			case self() of
				Parent ->
					raynal:send_message(ReplyTo, ?MODULE, Key, {'RESULT', NewValSet}),
					{ok, State#{
						parent => undefined,
						expected_msg => undefined,
						val_set => undefined,
						reply_to => undefined
					}};
				_ ->
					raynal:send_message(Parent, ?MODULE, Key, {'BACK', self(), NewValSet}),
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
	end.
