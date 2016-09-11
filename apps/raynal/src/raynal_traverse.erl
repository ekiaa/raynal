-module(raynal_traverse).

-callback execute(
	ProcessState :: raynal:process_state()
) -> 
	{
		Result :: raynal:traverse_result(),
		NewProcessState :: raynal:process_state()
	}.

-callback merge(
	ProcessState :: raynal:process_state(), 
	ProcessResult :: raynal:traverse_result(), 
	ChildResult :: raynal:traverse_result()
) -> 
	{
		ProcessResult :: raynal:traverse_result(),
		NewProcessState :: raynal:process_state()
	}.