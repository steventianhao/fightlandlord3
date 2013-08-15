-module(fll3_table_sup).
-behavior(supervisor).

-export([init/1]).
-export([start_table/1,start_link/0]).

init(_Args)->
	StartFunc={fll3_table,start_link,[]},
	Child={table,StartFunc,permanent,brutal_kill,worker,[fll3_table]},
	Strategy={simple_one_for_one,5,10},
	{ok,{Strategy,[Child]}}.

start_link() ->
    supervisor:start_link({local,?MODULE},?MODULE, []).

start_table(Id)->
	supervisor:start_child(?MODULE,[Id]).