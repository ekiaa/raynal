%%%-------------------------------------------------------------------
%% @doc dreg worker supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('dreg_worker_sup').

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Args) ->
	supervisor:start_child(?SERVER, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{simple_one_for_one, 0, 1}, [
    	{dreg_worker, {dreg_worker, start_link, []}, transient, 1000, worker, [dreg_worker]}
    ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
