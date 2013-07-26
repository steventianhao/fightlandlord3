-module(fll3_table).
-behavior(gen_server).

-export([init/1,handle_call/3,handle_cast/2,handle_inf/2,terminate/2,code_change/3]).
-record(state,{id,players=[]}).

start_link(Id)->
	gen_server:start_link(?MODULE,Id,[]).

init(Args)->
	{ok,#state{id=Args}}.

handle_cast({exit_table,Pid},State)->
	Players=
	{noreply,}

handle_cast(_Request,State)->
	{noreply,State}.


handle_call(enter_table,From,State)->
	{Pid,_}=From,
	{Result,NewState} = 
	if length(players)<3 ->
			Players=State#state.players,
			NewState=State#state{players=[Pid|Players]},
			{accepted,NewState};
		true -> 
			{rejected,State}
	end,
	{reply,Result,NewState};

handle_call(_Reqeust,_From,State)->
	{noreply,State}.

handle_info({enter_table,Player},State)->
	Players=
	NewState=State#state{players=[Player]
	{noreply,State}
handle_info(_Info,State)->
	{noreply,State}.

