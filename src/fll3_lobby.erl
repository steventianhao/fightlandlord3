-module(fll3_lobby).
-behavior(gen_server).

-record(state,{name,tables}).
-record(table,{id,status="open",users=[]}).

-export([init/1,terminate/2,handle_info/2,handle_call/3,handle_cast/2,code_change/3]).
-export([start_link/0,show_lobby/0,check_table_id/1]).

start_link()->
	gen_server:start_link({local,lobby},?MODULE,[],[]).

show_lobby()->
	gen_server:call(lobby,show_lobby).

check_table_id(TableId)->
	gen_server:call(lobby,{check_table_id,TableId}).

handle_call(show_lobby,_From,State)->
	Reply=State#state.tables,
	{reply,Reply,State};
handle_call({check_table_id,TableId},_From,State)->
	Reply=lists:any(fun(T)->T#table.id==TableId end,State#state.tables),
	{reply,Reply,State}.

handle_cast(_Request,State)->
	{noreply,State}.

handle_info(_Info,State)->
	{noreply,State}.

init(Args)->
	{ok,#state{name=Args,tables=[#table{id=1},#table{id=2},#table{id=3}]}}.

terminate(_Reason,_State)->
	ok.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.