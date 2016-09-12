-module(ex2).

-export([run/0]).

run() ->
	lager:debug("Run example #2"),
	dreg_worker:start(r1),
	dreg_worker:start(r2),
	dreg_worker:start(r3),
	dreg_worker:start(r4),
	dreg_worker:add_neighbor(r1, r2),
	dreg_worker:add_neighbor(r1, r3),
	dreg_worker:add_neighbor(r1, r4).
