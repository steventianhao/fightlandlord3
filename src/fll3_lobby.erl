-module(fll3_lobby).
-behavior(gen_server).

-record(state,{name,tables}).
-record(table,{id,status="open",users=[]}).

-export([init/1,terminate/2,handle_info/2,handle_call/3,handle_cast/2,code_change/3]).
-export([start_link/0,show_lobby/0,check_table_id/1,user_join_table/2,user_quit_table/2,fast_join/0]).
-export([table_to_json/1,tables_to_json/1]).

-define(MAX_PLAYERS,3).

start_link()->
	gen_server:start_link({local,lobby},?MODULE,[],[]).

show_lobby()->
	gen_server:call(lobby,show_lobby).

fast_join()->
	gen_server:call(lobby,fast_join).

check_table_id(TableId)->
	gen_server:call(lobby,{check_table_id,TableId}).

user_join_table(User,TableId)->
	gen_server:cast(lobby,{user_join_table,User,TableId}).
user_quit_table(User,TableId)->
	gen_server:cast(lobby,{user_quit_table,User,TableId}).

table_to_json(Table)->
	{[{id,Table#table.id},{status,list_to_binary(Table#table.status)}]}.
tables_to_json(Tables)->
	lists:map(fun(T)->table_to_json(T) end,Tables).	

fast_choose_table(TableDict)->
	Dict=dict:filter(
		fun(_Key,Value)-> 
			#table{users=Users,status=Status}=Value,
			length(Users)<?MAX_PLAYERS andalso Status == "open" 
		end,
		TableDict),
	case dict:size(Dict) >0 of
		true -> {ok,(lists:nth(1,dict:fetch_keys(Dict)))};
		false -> not_available
	end.

handle_call(show_lobby,_From,State)->
	Reply=utils:fetch_values(State#state.tables),
	{reply,Reply,State};
handle_call(fast_join,_From,State)->
	{reply,fast_choose_table(State#state.tables)};
handle_call({check_table_id,TableId},_From,State)->
	Reply=dict:is_key(TableId,State#state.tables),
	{reply,Reply,State}.

handle_cast({user_join_table,User,TableId},State)->
	NewTables=dict:update(TableId,fun(Table)-> Table#table{users=[User|Table#table.users]} end,State#state.tables),
	{noreply,State#state{tables=NewTables}};

handle_cast({user_quit_table,User,TableId},State)->
	NewTables=dict:update(TableId,
		fun(Table)-> Table#table{users=lists:delete(User,Table#table.users)} end,
		State#state.tables),
	{noreply,State#state{tables=NewTables}};
	
handle_cast(_Request,State)->
	{noreply,State}.

handle_info(_Info,State)->
	{noreply,State}.

init(Args)->
	lists:map(fun(Id)->fll3_table_sup:start_table(Id) end,[1,2,3]),
	Tables=lists:foldl(fun(El,Acc)-> dict:append(El,#table{id=El},Acc) end,dict:new(),[1,2,3]),
	{ok,#state{name=Args,tables=Tables}}.

terminate(_Reason,_State)->
	ok.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.