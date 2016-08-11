-module(bfst).

-export([handle_msg/5]).

-type state() :: #{
	parent       := undefined | pid(),
	children     := undefined | list(pid()),
	level        := undefined | pos_integer(),
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
			level => undefined,
			expected_msg => undefined,
			reply_to => undefined
		},
		ProcessState
	);

handle_msg(Key, {'START', ReplyTo}, _Neighbors, State, _ProcessState) ->
	lager:debug("~p START; ReplyTo: ~p", [self(), ReplyTo]),
	raynal:send_message(self(), ?MODULE, Key, {'GO', self(), -1}),
	{internal, State#{
		reply_to => ReplyTo
	}};

handle_msg(Key, {'GO', From, D}, Neighbors, 
	#{
		level := OldLevel
	} = State, _ProcessState
) when OldLevel == undefined; OldLevel > D + 1 ->
	lager:debug("~p GO from ~p; D: ~p; Resp: yes", [self(), From, D]),
	Level = D + 1,
	Children = sets:del_element(From, Neighbors),
	N = sets:size(Children),
	case N of
		0 ->
			raynal:send_message(From, ?MODULE, Key, {'BACK', self(), {yes, Level}});
		_ ->
			lists:foreach(
				fun(Neighbor) -> raynal:send_message(Neighbor, ?MODULE, Key, {'GO', self(), Level}) end,
				sets:to_list(Children)
			)
	end,
	{internal, State#{
		parent => From,
		level => Level,
		expected_msg => N
	}};

handle_msg(Key, {'GO', From, D}, _Neighbors, _State, _ProcessState) ->
	lager:debug("~p GO from ~p; D: ~p; Resp: no", [self(), From, D]),
	raynal:send_message(From, ?MODULE, Key, {'BACK', self(), {no, D + 1}}),
	ok;

handle_msg(Key, {'BACK', From, {Resp, D}}, _Neighbors,
	#{
		parent := Parent,
		level := Level,
		children := Children,
		expected_msg := N,
		reply_to := ReplyTo
	} = State, _ProcessState
) when D == Level + 1 ->
	lager:debug("~p BACK from ~p; matched", [self(), From]),
	NewChildren = case Resp of
		yes -> [From | Children];
		no -> Children
	end,
	NewN = N - 1,
	case NewN of
		0 when self() /= Parent ->
			raynal:send_message(Parent, ?MODULE, Key, {'BACK', self(), {yes, Level}});
		0 ->
			lager:debug("~p FINISHED", [self()]),
			raynal:send_message(ReplyTo, ?MODULE, Key, {'RESULT', ok});
		_ ->
			ok
	end,
	{internal, State#{
		children => NewChildren,
		expected_msg => NewN
	}};

handle_msg(_Key, {'BACK', From, _}, _Neighbors, _State, _ProcessState) ->
	lager:debug("~p BACK from ~p; not matched", [self(), From]),
	ok.