-module(fll3_player_fsm).
-behavior(gen_fsm).

-compile([{parse_transform, lager_transform}]).
-export([code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,init/1,terminate/3]).
-export([anonymous/2,user/2,start_link/1]).

-record(state,{conn,user,tables=[]}).

-define(PLAYER_ON_TABLE(Table),{p,l,{player_on_table,Table}}).
-define(TABLE(Table),{n,l,{table,Table}}).

start_link(Conn)->
	gen_fsm:start_link(?MODULE,Conn,[]).

init(Conn)->
	{ok,anonymous,#state{conn=Conn}}.

code_change(_OldVsn,StateName,StateData,_Extra)->
	{ok,StateName,StateData}.

%%handle all the event for all states,
handle_event(conn_closed,StateName,StateData)->
	lager:info("connection closed,what should I do# ~p@~p",[StateName,StateData]),
	{next_state,StateName,StateData};
handle_event(_Event,StateName,StateData)->
	{next_state,StateName,StateData}.

handle_sync_event(_Event,_From,StateName,StateData)->
	{reply,ok,StateName,StateData}.

handle_info(_Info,StateName,StateData)->
	{next_state,StateName,StateData}.

terminate(_Reason,_StateName,_StateData)->
	ok.

anonymous({login,Token},StateData)->
	lager:info("login, token is ~p,state is ~p",[Token,StateData]),
	case Token of
		<<"simon">>->
			lager:info("I am simon"),
			NewStateData=StateData#state{user="simon"},
			{next_state,user,NewStateData};
		<<"sammi">>->
			lager:info("I am sammi"),
			NewStateData=StateData#state{user="sammi"},
			{next_state,user,NewStateData};
		_ ->
			lager:info("I am nothing"),
			{next_state,anonymous,StateData}
	end;
anonymous(Info,_StateData)->
	lager:info("in anonymous state,message unhandled ~p",[Info]).

validate_table(Tables,Table)->lists:any(fun(T)->T==Table end,Tables).

user({enter_table,Table},StateData)->
	#state{tables=Tables}=StateData,
	case validate_table(Tables,Table) of
		false ->
			case fll3_lobby:check_table_id(Table) of
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
user(show_lobby,StateData)->
	lager:info("showlobby message got, state is ~p",[StateData]),
	Reply=fll3_lobby:show_lobby(),
	ConnRef=StateData#state.conn,
	gproc:send({n,l,{conn,ConnRef}},{output,Reply}),
	{next_state,user,StateData};

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