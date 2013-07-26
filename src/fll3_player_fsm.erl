-module(fll3_player_fsm).
-behavior(gen_fsm).

-export([code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,init/1,terminate/3]).

-record(state,{user,table,lobby}).


init([User,Lobby])->
	{ok,hangout,#state{user=User,lobby=Lobby}.

code_change(_OldVsn,StateName,StateData,Extra)->
	{ok,StateName,StateData}.

%%handle all the event for all states,
handle_event(Event,StateName,StateData)->
	{next_state,StateName,StateData}.

handle_sync_event(_Event,_From,StateName,StateData)->
	{reply,ok,StateName,StateData}.

%% send by table via gproc:send({p,l,{player_on_table,TableId}},start_game)
handle_info(start_game,ready,StateData)->
	{next_state,ready,StateData};
handle_info(Info,StateName,StateData)->
	{next_state,StateName,StateData}.

terminate(Reason,StateName,StateData)->
	ok.

%% STM  hangout----(enter_table)--> wait----(exit_table)-->hangout
%% wait-----(get_ready)--> ready --

hangout({enter_table,Table},StateData)->
	Lobby=StateData#state.lobby,
	IsValid=fll3_lobby:check_table_id(Lobby,Table),
	case IsValid of
		false->{stop,baddata,StateData};
		true->
			gproc:reg({p,l,{player_on_table,Table}}),
			NewStateData=StateData#state{table=Table},
			%%ask the table to send current status to player.
			gproc:send({p,l,{table,Table}},{current_status,self()}),
			{next_state,wait,NewStateData}
	end.

wait(exit_table,StateData)->
	Table=StateData#state.table,
	gproc:unreg({p,l,{player_on_table,Table}}),
	NewStateData=StateData#state{table=undefined},
	{next_state,hangout,NewStateData};
wait(get_ready,StateData)->
	Table=StateData#state.table,
	gproc:send({p,l,{table,Table}},{get_ready,self()}),
	{next_state,ready,StateData}.

%%from player
ready(exit_table,StateData)->
	Table=StateData#state.table,
	gproc:unreg({p,l,{player_on_table,Table}}),
	NewStateData=StateData#state{table=undefined},
	{next_state,hangout,NewStateData};

%%from table, when all player get ready, then start the game

start({bet,Bet},StateData)->
	