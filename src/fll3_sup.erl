
-module(fll3_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Strategy={one_for_one, 5, 10},
	PlayerSup={player_sup,{fll3_player_sup,start_link,[]},permanent,infinity,supervisor,[fll3_player_sup]},
	Lobby={lobby,{fll3_lobby,start_link,[]},permanent,brutal_kill,worker,[fll3_lobby]},
    {ok, {Strategy, [PlayerSup,Lobby]} }.