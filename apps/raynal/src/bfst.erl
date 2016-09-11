-module(bfst).

-export([handle_msg/4]).

-type state() :: #{
	parent       := undefined | pid(),
	children     := [] | list(pid()),
	level        := undefined | pos_integer(),
	expected_msg := undefined | non_neg_integer(),
	reply_to     := undefined | pid()
}.

-export_type([state/0]).

%===============================================================================
% API functions
%===============================================================================

-spec handle_msg(
	Message :: raynal:message(),
	Neighbors :: raynal:neighbors(),
	State :: undefined | state(),
	ProcessState :: raynal:process_state()
) -> raynal:algorithm_handle_msg_result().

handle_msg(Message, Neighbors, undefined, ProcessState) ->
	State = #{
		parent => undefined,
		children => [],
		level => undefined,
		expected_msg => undefined,
		reply_to => undefined
	},
	handle_msg(Message, Neighbors, State, ProcessState);

handle_msg(#{command := build} = Message, Neighbors, State, ProcessState) ->
	build(Message, Neighbors, State, ProcessState).

%===============================================================================
% Internal functions
%===============================================================================

build(
	#{message := 'START', from := ReplyTo} = Message,
	_Neighbors,
	State,
	_ProcessState
) ->
	lager:debug("~p START; ReplyTo: ~p", [self(), ReplyTo]),
	raynal:send_message(self(), {'GO', -1}, Message),
	{ok, {algorithm, State#{reply_to => ReplyTo}}};

build(
	#{message := {'GO', D}, from := From} = Message,
	Neighbors, 
	#{level := OldLevel} = State,
	_ProcessState
) when OldLevel == undefined; OldLevel > D + 1 ->
	lager:debug("~p GO from ~p; D: ~p; Resp: yes", [self(), From, D]),
	Level = D + 1,
	Children = sets:del_element(From, Neighbors),
	N = sets:size(Children),
	case N of
		0 ->
			raynal:send_message(From, {'BACK', {yes, Level}}, Message);
		_ ->
			lists:foreach(
				fun(Neighbor) -> raynal:send_message(Neighbor, {'GO', Level}, Message) end,
				sets:to_list(Children)
			)
	end,
	{ok, {algorithm, State#{parent => From, level => Level, expected_msg => N}}};

build(
	#{message := {'GO', D}, from := From} = Message,
	_Neighbors,
	_State,
	_ProcessState
) ->
	lager:debug("~p GO from ~p; D: ~p; Resp: no", [self(), From, D]),
	raynal:send_message(From, {'BACK', {no, D + 1}}, Message),
	ok;

build(
	#{message := {'BACK', {Resp, D}}, from := From} = Message,
	_Neighbors,
	#{parent := Parent, level := Level, children := Children, expected_msg := N, reply_to := ReplyTo} = State,
	_ProcessState
) when D == Level + 1 ->
	lager:debug("~p BACK from ~p; matched", [self(), From]),
	NewChildren = case Resp of
		yes -> [From | Children];
		no -> Children
	end,
	NewN = N - 1,
	case NewN of
		0 when self() /= Parent ->
			raynal:send_message(Parent, {'BACK', {yes, Level}}, Message);
		0 ->
			lager:debug("~p FINISHED", [self()]),
			raynal:send_message(ReplyTo, {'RESULT', ok}, Message);
		_ ->
			ok
	end,
	{ok, {algorithm, State#{children => NewChildren, expected_msg => NewN}}};

build(#{message := {'BACK', _}, from := From}, _Neighbors, _State, _ProcessState) ->
	lager:debug("~p BACK from ~p; not matched", [self(), From]),
	ok.