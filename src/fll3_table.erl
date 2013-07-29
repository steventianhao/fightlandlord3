-module(fll3_table).
-behavior(gen_fsm).

-export([init/1,handle_event/3,handle_info/3,terminate/3,code_change/4]).
-export([start_link/1]).

-define(PLAYER_ON_TABLE(Id),{p,l,{player_on_table,Id}}).
-define(TABLE(Id),{n,l,{table,Id}}).
-define(MAX_PLAYER,3).
-define(MAX_READY_PLAYER,2).
-define(MAX_IDLE,15).

-record(state,{id,players,ready_timer,max_ready_ticks=15}).
-record(timer,{ref,ticks}).
-record(player,{pid,status=not_ready,idle=0}).

start_link(Id)->
	gen_server:start_link(?MODULE,Id,[]).

init(Args)->
	gproc:reg(n,l,{table,Args}),
	{ok,open,#state{id=Args,players=dict:new()}}.

open(enter_table,From,S)->
	{Pid,_}=From,Players=S#state.players,
	{Result,NS} =
	case dict:is_key(Pid,Players) of
		true-> {already_accept,S};
		false->
			case dict:size(Players) of 
				Size when Size < ?MAX_PLAYER ->
					Player=#player{pid=Pid},
					NS=S#state{players=dict:append(Pid,Player,Players)},
					gproc:send(?PLAYER_ON_TABLE(S#state.id),{notify_enter_table,Pid}),
					{accept,NS};
				_ -> {reject,S}
			end
	end.
	{reply,Result,open,NewState}.

open({get_ready,Pid},StateData)->
	Players=StateData#state.players,
	Player=dict:fetch(Pid,Players)
	{StateName,NewStateData}=
		case Player#player.status of 
				ready ->{open,StateData};
				not_ready->
					F=fun(P)->P#player{status=ready} end,
					NewPlayers=dict:update(Pid,F,Players),
					NS=S#state{players=NewPlayers},
					case count_ready_players(NewPlayers) of
						?MAX_PLAYER->
							%%deal the cards

							{start,NS};
						?MAX_READY_PLAYER->
							{ok,Timer}=timer:send_interval(1000,self(),check_ready),
							NS2=NS#state{ready_time=Timer},
							{open,NS};
						_->{open,NS}
					end
		end,
	{next_state,StateName,NewStateData};
open({exit_table,Pid},StateData)->
	#state{id=Id,players=Players0,ready_timer=Timer}=StateData,
	Players1=dict:erase(Pid,Players),
	case Timer of
		undefined->ok;
		TRef-> timer:cancel(TRef)
	end,
	NewStateData=State#state{players=Players1},
	gproc:send(?PLAYER_ON_TABLE(Id),{notify_exit_table,Pid}),
	{next_state,open,NewStateData}.

code_change(_OldVsn,StateName,StateData,_Extra)->
	{ok,StateName,StateData}.

terminate(_Reason,_StateName,_StateData)->
	ok.

count_ready_players(Players)->
	F=fun(_Key,ready,Acc)->Acc+1;(_Key,not_ready,Acc)->Acc end,
	dict:fold(F,0,Players).

% increase_idle(_K,V=#player{idle=Idle,status=not_ready})-> V#player{idle=Idle+1};
% increase_idle(_K,V)-> V.

handle_info(check_ready,open,StateData)->
	#state{players=Players0,id=Id,ready_timer=Timer}=StateData,
	Ticks=Timer#timer.ticks
	if
		Ticks =< 0 ->
			Players3=dict:filter(fun(_Key,V)->V#player.status =< ready end,Player0),
			case dict:size(Player3) of
				Size when Size>0 -> 
					timer:cancel(Timer),
					gproc:send(?PLAYER_ON_TABLE(Id),{kick_off,Player3});
				_ -> ok
			end,
			Players2=dict:filter(fun(_Key,V)->V#player.status ==ready end,Players0),
			NewStateData=StateData#state{players=Players2,ready_timer=undefined},
			{next_state,open,NewStateData};
		true-> 
			gproc:send(?PLAYER_ON_TABLE(Id),{ready_tick,Timer#timer.ticks});
			NewTimer=Timer#timer{ticks=Ticks-1}
			NewStateData=StateData#state{ready_timer=},
			{next_state,open,StateData};
	end.
	
handle_info(Info,StateName,StateData)->
	{next_state,StateName,StateData}.
