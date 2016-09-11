%%%-------------------------------------------------------------------
%% @doc dreg top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('dreg_sup').

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
    {ok, {#{}, [
    	#{
    		id => dreg_worker_sup,
    		start => {dreg_worker_sup, start_link, []},
    		type => supervisor
    	}
    ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
