-module(fll3).
-export([start/0]).

start()->
	ok=application:start(ranch),
	ok=application:start(fll3).
