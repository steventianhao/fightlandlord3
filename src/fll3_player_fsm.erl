-module(fll3_player_fsm).
-behavior(gen_fsm).

-export([code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,init/1,terminate/3]).
-export([anonymous/2,user/2,start_link/2]).

-record(state,{conn,user,tables=[],lobby}).
-define(PLAYER_ON_TABLE(Table),{p,l,{player_on_table,Table}}).
-define(TABLE(Table),{n,l,{table,Table}}).

start_link(Conn,Lobby)->
	gen_fsm:start_link(?MODULE,{Conn,Lobby},[]).

init({Conn,Lobby})->
	{ok,anonymous,#state{conn=Conn,lobby=Lobby}}.

code_change(_OldVsn,StateName,StateData,_Extra)->
	{ok,StateName,StateData}.

%%handle all the event for all states,
handle_event(_Event,StateName,StateData)->
	{next_state,StateName,StateData}.

handle_sync_event(_Event,_From,StateName,StateData)->
	{reply,ok,StateName,StateData}.

handle_info(_Info,StateName,StateData)->
	{next_state,StateName,StateData}.

terminate(_Reason,_StateName,_StateData)->
	ok.

anonymous({login,Token},StateData)->
	io:format("login, token is ~p,state is ~p~n",[Token,StateData]),
	case Token of
		"simon"->
			{next_state,user,StateData};
		_ ->
			{next_state,anonymous,StateData}
	end.

validate_table(Tables,Table)->lists:any(fun(T)->T==Table end,Tables).

user({enter_table,Table},StateData)->
	#state{lobby=Lobby,tables=Tables}=StateData,
	case validate_table(Tables,Table) of
		false ->
			case fll3_lobby:check_table_id(Lobby,Table) of
				false->{stop,badarg,StateData};
				true->
					Ptable=gproc:where(?TABLE(Table)),
					case gen_server:call(Ptable,{enter_table,self()}) of
						accept->
							gproc:reg(?PLAYER_ON_TABLE(Table)),
							NewStateData=StateData#state{tables=[Table|StateData#state.tables]},
							{next_state,user,NewStateData};
						reject->
							{next_state,user,StateData}
					end
			end;
		true ->
			{next_state,user,StateData}
	end;

user({exit_table,Table},StateData)->
	Tables=StateData#state.tables,
	case validate_table(Tables,Table) of
		true->
			NewTables=lists:filter(fun(T)->T /= Table end,Tables),
			NewStateData=StateData#state{tables=NewTables},
			gproc:unreg(?PLAYER_ON_TABLE(Table)),
			gproc:send(?TABLE(Table),{exit_table,Table,self()}),
			{next_state,user,NewStateData};
		fase->
			{stop,badarg,StateData}
	end;

user({get_ready,Table},StateData)->
	Tables=StateData#state.tables,
	case lists:any(fun(T)->T==Table end,Tables) of
		true -> 
			gproc:send(?TABLE(Table),{get_ready,Table,self()}),
			{next_state,user,StateData};
		false -> 
			{stop,badarg,StateData}
	end.