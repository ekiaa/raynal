%%%-------------------------------------------------------------------
%% @doc process top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(process_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_all, 0, 1}, [
		{process_worker_sup, {process_worker_sup, start_link, []}, permanent, infinity, supervisor, [process_worker_sup]}
	]}}.

%%====================================================================
%% Internal functions
%%====================================================================
