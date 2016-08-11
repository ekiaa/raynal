-module(rst).

-export([handle_msg/5]).

-type state() :: #{
	parent       := undefined | pid(),
	children     := undefined | list(pid()),
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
			children => [],
			expected_msg => undefined,
			reply_to => undefined
		},
		ProcessState
	);

handle_msg(Key, {'START', ReplyTo}, Neighbors,
	#{
		parent := undefined,
		expected_msg := undefined
	} = State, _ProcessState
) ->
	lager:debug("~p START; ReplyTo: ~p", [self(), ReplyTo]),
	N = sets:size(Neighbors),
	lists:foreach(
		fun(Pid) -> raynal:send_message(Pid, ?MODULE, Key, {'GO', self()}) end,
		sets:to_list(Neighbors)
	),
	{internal, State#{
		parent => self(),
		expected_msg => N,
		reply_to => ReplyTo
	}};

handle_msg(Key, {'GO', From}, Neighbors,
	#{
		parent := undefined,
		expected_msg := undefined
	} = State, _ProcessState
) ->
	lager:debug("~p GO from ~p; matched", [self(), From]),
	N = sets:size(Neighbors) - 1,
	case N of
		0 ->
			raynal:send_message(From, ?MODULE, Key, {'BACK', self(), yes}),
			ok;
		_ ->
			lists:foreach(
				fun(Pid) -> raynal:send_message(Pid, ?MODULE, Key, {'GO', self()}) end,
				sets:to_list(sets:del_element(From, Neighbors))
			),
			{internal, State#{
				parent => From,
				expected_msg => N
			}}
	end;

handle_msg(Key, {'GO', From}, _Neighbors, _State, _ProcessState) ->
	lager:debug("~p GO from ~p; not matched", [self(), From]),
	raynal:send_message(From, ?MODULE, Key, {'BACK', self(), no}),
	ok;

handle_msg(Key, {'BACK', From, Resp}, _Neighbors,
	#{
		parent := Parent,
		children := Children,
		expected_msg := N,
		reply_to := ReplyTo
	} = State, _ProcessState
) ->
	lager:debug("~p BACK from ~p; Resp: ~p", [self(), From, Resp]),
	NewN = N - 1,
	NewChildren = case Resp of
		yes -> [From | Children];
		no -> Children
	end,
	case NewN of
		0 when self() /= Parent ->
			raynal:send_message(Parent, ?MODULE, Key, {'BACK', self(), yes});
		0 ->
			raynal:send_message(ReplyTo, ?MODULE, Key, {'RESULT', ok});
		_ ->
			ok
	end,
	{internal, State#{
		children => NewChildren,
		expected_msg => NewN
	}}.
