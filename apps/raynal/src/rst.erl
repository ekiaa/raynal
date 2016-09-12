-module(rst).

-export([handle_msg/4]).

-type state() :: #{
	parent       := undefined | pid(),
	children     := [] | list(pid()),
	expected_msg := undefined | non_neg_integer(),
	reply_to     := undefined | pid(),
	result       := undefined | term()
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
		expected_msg => undefined,
		reply_to => undefined,
		result => undefined
	},
	handle_msg(Message, Neighbors, State, ProcessState);

handle_msg(#{command := build} = Message, Neighbors, State, ProcessState) ->
	build(Message, Neighbors, State, ProcessState);

handle_msg(#{command := traverse} = Message, Neighbors, State, ProcessState) ->
	traverse(Message, Neighbors, State, ProcessState);

handle_msg(#{command := clean} = Message, Neighbors, State, ProcessState) ->
	clean(Message, Neighbors, State, ProcessState).

%===============================================================================
% Internal functions
%===============================================================================

build(
	#{message := 'START', from := ReplyTo} = Message,
	Neighbors,
	#{parent := undefined, expected_msg := undefined} = State,
	_ProcessState
) ->
	lager:debug("~p START; ReplyTo: ~p", [self(), ReplyTo]),
	N = sets:size(Neighbors),
	lists:foreach(
		fun(Pid) -> raynal:send_message(Pid, 'GO', Message) end,
		sets:to_list(Neighbors)
	),
	{ok, {algorithm, State#{parent => self(), expected_msg => N, reply_to => ReplyTo}}};

build(
	#{message := 'GO', from := From} = Message,
	Neighbors,
	#{parent := undefined, expected_msg := undefined} = State,
	_ProcessState
) ->
	lager:debug("~p GO from ~p; matched", [self(), From]),
	N = sets:size(Neighbors) - 1,
	case N of
		0 ->
			raynal:send_message(From, {'BACK', yes}, Message),
			ok;
		_ ->
			lists:foreach(
				fun(Pid) -> raynal:send_message(Pid, 'GO', Message) end,
				sets:to_list(sets:del_element(From, Neighbors))
			),
			{ok, {algorithm, State#{parent => From, expected_msg => N}}}
	end;

build(
	#{message := 'GO', from := From} = Message,
	_Neighbors,
	_State,
	_ProcessState
) ->
	lager:debug("~p GO from ~p; not matched", [self(), From]),
	raynal:send_message(From, {'BACK', no}, Message),
	ok;

build(
	#{message := {'BACK', Resp}, from := From} = Message,
	_Neighbors,
	#{parent := Parent, children := Children, expected_msg := N, reply_to := ReplyTo} = State,
	_ProcessState
) ->
	lager:debug("~p BACK from ~p; Resp: ~p", [self(), From, Resp]),
	NewN = N - 1,
	NewChildren = case Resp of
		yes -> [From | Children];
		no -> Children
	end,
	case NewN of
		0 when self() /= Parent ->
			raynal:send_message(Parent, {'BACK', yes}, Message);
		0 ->
			raynal:send_message(ReplyTo, {'RESULT', ok}, Message);
		_ ->
			ok
	end,
	{ok, {algorithm, State#{children => NewChildren, expected_msg => NewN}}}.

%===============================================================================

traverse(
	#{message := {'START', CallbackModule}} = Message,
	_Neighbors,
	#{parent := Parent, children := Children, reply_to := ReplyTo, result := undefined} = State,
	ProcessState
) ->
	N = erlang:length(Children),
	{NewResult, NewProcessState} = CallbackModule:execute(ProcessState),
	case N of
		0 ->
			case ReplyTo of
				undefined -> raynal:send_message(Parent, {'BACK', NewResult, CallbackModule}, Message);
				_ -> raynal:send_message(ReplyTo, {'RESULT', NewResult}, Message)
			end,
			{ok, {process, NewProcessState}};
		_ ->
			lists:foreach(
				fun(Pid) -> raynal:send_message(Pid, {'START', CallbackModule}, Message) end,
				Children),
			{ok, State#{expected_msg => N, result => NewResult}, NewProcessState}
	end;

traverse(
	#{message := {'BACK', ChildResult, CallbackModule}} = Message,
	_Neighbors,
	#{parent := Parent, expected_msg := N, reply_to := ReplyTo, result := Result} = State,
	ProcessState
) ->
	NewN = N - 1,
	{NewResult, NewProcessState} = CallbackModule:merge(ProcessState, Result, ChildResult),
	case NewN of
		0 ->
			case ReplyTo of
				undefined -> raynal:send_message(Parent, {'BACK', NewResult, CallbackModule}, Message);
				_ -> raynal:send_message(ReplyTo, {'RESULT', NewResult}, Message)
			end,
			{ok, State#{expected_msg => 0, result => undefined}, NewProcessState};
		_ ->
			{ok, State#{expected_msg => NewN, result => NewResult}, NewProcessState}
	end.

%===============================================================================

clean(
	#{message := 'CLEAN'} = Message,
	_Neighbors,
	#{children := Children} = _State,
	_ProcessState
) ->
	lists:foreach(fun(Pid) -> raynal:send_message(Pid, 'CLEAN', Message) end, Children),
	clean.