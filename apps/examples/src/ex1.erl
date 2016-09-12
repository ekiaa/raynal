-module(ex1).

-behaviour(raynal_traverse).

-export([run/0, execute/1, merge/3]).

run() ->
	lager:debug("Run example #1"),
	lager:info("Clean processes"),
	process:clean(),
	lager:info("Generate processes"),
	process:generate(),
	timer:sleep(10),
	{ok, Root} = process:get_random(),
	lager:info("Build RST with root: ~p", [Root]),
	raynal:build(rst, Root, ex1),
	lager:info("Traverse RST with root: ~p", [Root]),
	Result = raynal:traverse(rst, Root, ex1, ?MODULE),
	lager:info("Travese result: ~p", [Result]).

execute(State) ->
	lager:debug("[execute] State: ~p", [State]),
	{{self(), []}, State}.

merge(State, {Self, List} = Result, ChildResult) ->
	lager:debug("[merge] State: ~p; Result: ~p; ChildResult: ~p", [State, Result, ChildResult]),
	{{Self, [ChildResult | List]}, State}.

