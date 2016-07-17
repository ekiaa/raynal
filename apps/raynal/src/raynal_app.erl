%%%-------------------------------------------------------------------
%% @doc raynal public API
%% @end
%%%-------------------------------------------------------------------

-module(raynal_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    raynal_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================