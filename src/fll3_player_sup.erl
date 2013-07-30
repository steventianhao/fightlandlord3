-module(fll3_player_sup).
-behavior(supervisor).

-export([init/1]).
-export([start_player/2,start_player_mgr/1]).

init(Lobby)->
	StartFunc={fll3_player_fsm,start_link,[Lobby]},
	Child={player,StartFunc,permanent,brutal_kill,worker,[fll3_player_fsm]},
	Strategy={simple_one_for_one,5,9000},
	{ok,{Strategy,[Child]}}.

start_player_mgr(Lobby) ->
    supervisor:start_link(?MODULE, Lobby).

start_player(Supervisor,Conn)->
	supervisor:start_child(Supervisor,[Conn]).