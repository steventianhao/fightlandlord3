-module(fll3_player_sup).
-behavior(supervisor).

-export([init/1]).
-export([start_player/1,start_player_mgr/0]).

init(_Args)->
	StartFunc={fll3_player_fsm,start_link,[]},
	Child={player,StartFunc,permanent,brutal_kill,worker,[fll3_player_fsm]},
	Strategy={simple_one_for_one,5,9000},
	{ok,{Strategy,[Child]}}.

start_player_mgr() ->
    supervisor:start_link({local,player_manager},?MODULE, []).

start_player(Conn)->
	supervisor:start_child(player_manager,[Conn]).