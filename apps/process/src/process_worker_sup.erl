%%%-------------------------------------------------------------------
%% @doc process top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(process_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0, terminate_all_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
	supervisor:start_child(?SERVER, []).

terminate_all_child(Processes) ->
	lists:foreach(
		fun(Pid) -> 
			Result = supervisor:terminate_child(?SERVER, Pid),
			lager:debug("Terminate ~p result: ~p", [Pid, Result])
		end,
		Processes
	).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{simple_one_for_one, 0, 1}, [
		{process_worker, {process_worker, start_link, []}, temporary, 1000, worker, [process_worker]}
	]}}.

%%====================================================================
%% Internal functions
%%====================================================================
