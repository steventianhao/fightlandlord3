-module(fll3_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	{ok,_}=ranch:start_listener(fll3_tcp,100,ranch_tcp,[{port,7070},{max_connections,10000}],fll3_tcp,[]),
    fll3_sup:start_link().
    
stop(_State) ->
    ok.