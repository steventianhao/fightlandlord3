-module(fll3_player_fsm).
-behavior(gen_fsm).

-compile([{parse_transform, lager_transform}]).
-export([code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,init/1,terminate/3]).
-export([anonymous/3,user/2,start_link/0]).

-record(state,{conn,user,tables=[]}).

-define(PLAYER_ON_TABLE(Table),{p,l,{player_on_table,Table}}).
-define(TABLE(Table),{n,l,{table,Table}}).

%% error code table
-define(NO_TABLE_AVAILABLE,-1).

%%private functions

write_json(Conn,Json)->
	Conn ! {output,Json}.

start_link()->
	gen_fsm:start_link(?MODULE,[],[]).

init(_Args)->
	{ok,anonymous,#state{}}.

code_change(_OldVsn,StateName,StateData,_Extra)->
	{ok,StateName,StateData}.


%%handle all the event for all states,
handle_event(conn_closed,StateName,StateData)->
	lager:info("connection closed,what should I do# ~p@~p",[StateName,StateData]),
	{stop,normal,StateData};
handle_event(_Event,StateName,StateData)->
	{next_state,StateName,StateData}.

handle_sync_event(_Info,_From,StateName,StateData)->
	{reply,ok,StateName,StateData}.

handle_info(_Info,StateName,StateData)->
	{next_state,StateName,StateData}.

terminate(_Reason,_StateName,_StateData)->
	ok.

anonymous({login,Token},{Pid,_},StateData)->
	lager:info("login, token is ~p,state is ~p",[Token,StateData]),
	case Token of
		<<"simon">>->
			lager:info("I am simon"),
			NewStateData=StateData#state{user="simon",conn=Pid},
			{reply,ok,user,NewStateData};
		<<"sammi">>->
			lager:info("I am sammi"),
			NewStateData=StateData#state{user="sammi",conn=Pid},
			{reply,ok,user,NewStateData};
		_ ->
			lager:info("I am nothing"),
			{stop,normal,err,StateData}
	end;
anonymous(Info,_From,_StateData)->
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
	Reply=fll3_lobby:show_lobby(),
	lager:info("showlobby message got, state is ~p,reply is ~p",[StateData,Reply]),
	Json={[{kind,show_lobby},{result,fll3_lobby:tables_to_json(Reply)}]},
	write_json(StateData#state.conn,Json),
	{next_state,user,StateData};

user(fast_join,StateData)->
	case fll3_lobby:fast_join() of
		{ok,TableId} ->
			Json={[{kind,fast_join},{result,TableId}]};
		not_available->
			Json={[{kind,fast_join},{error,?NO_TABLE_AVAILABLE}]}
	end,
	write_json(StateData#state.conn,Json);

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

