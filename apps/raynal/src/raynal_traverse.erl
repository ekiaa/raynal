-module(raynal_traverse).

-callback execute(
	Params :: term(),
	ProcessState :: raynal:process_state()
) -> 
	{
		Result :: raynal:traverse_result(),
		NewProcessState :: raynal:process_state()
	}.

-callback merge(
	Params :: term(),
	ProcessState :: raynal:process_state(), 
	ProcessResult :: raynal:traverse_result(), 
	ChildResult :: raynal:traverse_result()
) -> 
	{
		ProcessResult :: raynal:traverse_result(),
		NewProcessState :: raynal:process_state()
	}.

%===============================================================================

-type command() :: 'START' | {'BACK', term()}.

-type message() :: 
	#{
		command := command(),
		ref := reference(),
		callback := atom(),
		reply_to := raynal:process(),
		params := term()
	}.

-export_type([
	command/0
	message/0
]).

-export([send_message/8, send_message/3]).

%===============================================================================

-spec send_message(
	ProcessPid :: raynal:process(),
	Key :: raynal:key(),
	Algorithm :: raynal:algorithm(),
	Ref :: reference(),
	Callback :: atom(),
	ReplyTo :: raynal:process(),
	Params :: term(),
	Command :: command()
) -> ok.

send_message(ProcessPid, Key, Algorithm, Ref, Callback, ReplyTo, Params, Command) ->
	raynal:send_message(
		ProcessPid,
		traverse,
		Key,
		Algorithm,
		get_message(
			Command,
			Ref,
			Callback,
			ReplyTo,
			Params
		)
	).

-spec send_message(
	ProcessPid :: raynal:process(),
	PreviousMessage :: raynal:message(message()),
	Command :: command()
) -> ok.

send_message(ProcessPid, PreviousMessage, Command) ->
	raynal:send_message(
		ProcessPid,
		PreviousMessage,
		get_message(
			raynal:get_method_message(PreviousMessage),
			Command
		)
	).

%-------------------------------------------------------------------------------

-spec get_message(
	Command :: command(),
	Ref :: reference(),
	Callback :: atom(),
	ReplyTo :: raynal:process()(),
	Params :: term()
) -> message().

get_message(Command, Ref, Callback, ReplyTo, Params) ->
	#{
		command => Command,
		ref => Ref,
		callback => Callback,
		reply_to => ReplyTo,
		params => Params
	}.

-spec get_message(
	PreviousMessage :: message(),
	Command :: command()
) ->
	PreviousMessage#{
		command => Command
	}.