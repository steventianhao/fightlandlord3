-module(fll3_tcp).
-behavior(ranch_protocol).
-behavior(gen_server).

-compile([{parse_transform, lager_transform}]).
-export([start_link/4]).
-export([init/4]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-export([send/2]).

-record(state,{socket,transport,player}).

start_link(Ref,Socket,Transport,Opts)->
	proc_lib:start_link(?MODULE,init,[Ref,Socket,Transport,Opts]).
	
init(Ref,Socket,Transport,_)->
	ok=proc_lib:init_ack({ok,self()}),
	ok=ranch:accept_ack(Ref),
	ok=Transport:setopts(Socket,[{active,once},{packet,line},{recbuf,512},{packet_size,512}]),
	{ok,Timeout}=application:get_env(fll3,timeout),
	gen_server:enter_loop(?MODULE,[],#state{socket=Socket,transport=Transport},Timeout).

init(_Args)-> ignore.

send(Pid,Json)->
	gen_server:cast(Pid,{output,Json}).

code_change(_OldVsn,State,_Extra)->
	{ok,State}.

handle_call(_Request,_From,State)->
	{noreply,State}.

handle_cast({output,Reply},State)->
	#state{socket=Socket,transport=Transport}=State,
	Transport:send(Socket,[jiffy:encode(Reply),"\r\n"]),
	{noreply,State};
handle_cast(_Request,State)->
	{noreply,State}.

handle_info(Info,State)->
	#state{socket=Socket,transport=Transport}=State,
	Transport:setopts(Socket,[{active,once}]),
	handle(Info,State).
	
notify_conn_closed(Player)->
	case Player of
		undefined -> ok;
		Pid-> gen_fsm:send_all_state_event(Pid,conn_closed)
	end.

handle({tcp,Socket,Packet},State)->
	lager:info("get data from connection@~p, data@~p",[Socket,Packet]),
	Action=fll3_json_action:handle(Packet),
	lager:info("Action got:~p",[Action]),
	case Action of
		badjson-> 
			notify_conn_closed(State#state.player),
			lager:info("get message is invalid jsoin ~p",[Packet]),
			{stop,badjson,State};
		Login={login,_Token} ->
			{ok,Pid}=fll3_player_sup:start_player(),
			case gen_fsm:sync_send_event(Pid,Login) of
				ok-> 
					NewState=State#state{player=Pid},
					{noreply,NewState};
				err->
					{stop,noauth,State}
			end;
		_ when State#state.player==undefined->
			{stop,noauth,State};
		_ ->
			gen_fsm:send_event(State#state.player,Action),
			{noreply,State}
	end;
handle({tcp_closed,Socket},State)->
	notify_conn_closed(State#state.player),
	lager:info("connection closed @~p",[Socket]),
	{stop,conn_closed,State};

handle({tcp_error,Socket,Reason},State)->
	lager:info("connection error @~p~n",[Socket]),
	notify_conn_closed(State#state.player),
	{stop,{conn_error,Reason},State};

handle(timeout,State)->
	lager:info("connection timeout,should close the socket~n"),
	notify_conn_closed(State#state.player),
	{stop,conn_timeout,State};

handle(Info,State)->
	lager:info("something not handled@~p",[Info]),
	{noreply,State}.

terminate(_Reason,_State)->
	ok.