-module(fll3_table).
-behavior(gen_fsm).

-export([init/1,handle_event/3,handle_sync_event/4,handle_info/3,terminate/3,code_change/4]).
-export([start_link/1,open/3,open/2,start/2]).

-define(PLAYER_ON_TABLE(Id),{p,l,{player_on_table,Id}}).
-define(TABLE(Id),{n,l,{table,Id}}).

-define(MAX_PLAYER,3).
-define(MAX_READY_TICKS,15).


-record(state,{id,players,ready_timer}).
-record(timer,{ref,ticks}).
-record(player,{pid,status=not_ready}).

start_link(Id)->
	gen_fsm:start_link({global,table},?MODULE,Id,[]).

init(Args)->
	%gproc:reg(n,l,{table,Args}),
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
					%gproc:send(?PLAYER_ON_TABLE(S#state.id),{notify_enter_table,Pid}),
					io:format("notify_enter_table"),
					{accept,S#state{players=dict:append(Pid,Player,Players)}};
				_ -> {reject,S}
			end
	end,
	NPlayers=dict:size(NS#state.players),
	if
		NPlayers == ?MAX_PLAYER ->
			{ok,Timer}=timer:send_interval(1000,self(),check_ready),
			NewStateData=NS#state{ready_timer=Timer};
		true-> 
			NewStateData=NS
	end,
	{reply,Result,open,NewStateData}.

count_ready_players(Players)->
	F=fun(_Key,ready,Acc)->Acc+1;(_Key,not_ready,Acc)->Acc end,
	dict:fold(F,0,Players).

open({get_ready,Pid},StateData)->
	#state{players=Players,ready_timer=Timer}=StateData,
	Player=dict:fetch(Pid,Players),
	{StateName,NewStateData}=
		case Player#player.status of 
				ready -> {open,StateData};
				not_ready->
					F=fun(P)->P#player{status=ready} end,
					NewPlayers=dict:update(Pid,F,Players),
					NS=StateData#state{players=NewPlayers},
					case count_ready_players(NewPlayers) of
						?MAX_PLAYER->
							io:format("all get ready, start playing game"),
							timer:cancel(Timer#timer.ref),
							{start,NS#state{ready_timer=undefined}};
						_->{open,NS}
					end
		end,
	{next_state,StateName,NewStateData};
open({exit_table,Pid},StateData)->
	#state{players=Players,ready_timer=Timer}=StateData,
	Players1=dict:erase(Pid,Players),
	case Timer of
		undefined-> ok;
		#timer{ref=Ref}-> timer:cancel(Ref)
	end,
	NewStateData=StateData#state{players=Players1,ready_timer=undefined},
	%gproc:send(?PLAYER_ON_TABLE(Id),{notify_exit_table,Pid}),
	io:format("notify_exit_table"),
	{next_state,open,NewStateData}.

start(Request,StateData)->
	io:format("request is ~p, state is ~p~n",[Request,StateData]).

code_change(_OldVsn,StateName,StateData,_Extra)->
	{ok,StateName,StateData}.

terminate(_Reason,_StateName,_StateData)->
	ok.

handle_info(check_ready,open,StateData)->
	#state{players=Players,ready_timer=Timer}=StateData,
	Ticks=Timer#timer.ticks,
	if
		Ticks =< 0 ->
			PlayersNotReady=dict:filter(fun(_Key,V)->V#player.status =< ready end,Players),
			case dict:size(PlayersNotReady) of
				Size when Size>0 -> 
					io:format("kick_off ~p~n",[PlayersNotReady]);
					%gproc:send(?PLAYER_ON_TABLE(Id),{kick_off,PlayersNotReady});
				_ -> ok
			end,
			PlayersReady=dict:filter(fun(_Key,V)->V#player.status ==ready end,Players),
			timer:cancel(Timer#timer.ref),
			NewStateData=StateData#state{players=PlayersReady,ready_timer=undefined},
			{next_state,open,NewStateData};
		true-> 
			%gproc:send(?PLAYER_ON_TABLE(Id),{ready_tick,Timer#timer.ticks}),
			io:format("ready_tick~p~n",[Timer#timer.ticks]),
			NewTimer=Timer#timer{ticks=Ticks-1},
			NewStateData=StateData#state{ready_timer=NewTimer},
			{next_state,open,NewStateData}
	end;
	
handle_info(Info,StateName,StateData)->
	io:fomrat("in handle_info ~p#~p#~p~n",[Info,StateName,StateData]),
	{next_state,StateName,StateData}.

handle_event(_Request,StateName,StateData)->
	error("shouldn't be called"),
	{next_state,StateName,StateData}.

handle_sync_event(_Event,_From,StateName,StateData)->
	error("shouldn't be called"),
	{next_state,StateName,StateData}.
