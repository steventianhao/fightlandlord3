-module(fll3_tcp).
-behavior(ranch_protocol).
-behavior(gen_server).

-compile([{parse_transform, lager_transform}]).
-export([start_link/4]).
-export([init/6]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

start_link(Ref,Socket,Transport,Opts)->
	ConnRef=make_ref(),
	{ok,Pid}=fll3_player_sup:start_player(ConnRef),
	proc_lib:start_link(?MODULE,init,[Ref,Socket,Transport,Pid,ConnRef,Opts]).
	
init(Ref,Socket,Transport,Player,ConnRef,_Opts=[])->
	ok=proc_lib:init_ack({ok,self()}),
	ok=ranch:accept_ack(Ref),
	ok=Transport:setopts(Socket,[{active,once},{packet,line},{recbuf,512},{packet_size,512}]),
	{ok,Timeout}=application:get_env(fll3,timeout),
	gproc:reg({n,l,{conn,ConnRef}}),
	gen_server:enter_loop(?MODULE,[],{Socket,Transport,Player},Timeout).

init(_Args)-> ignore.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.

handle_call(_Request,_From,State)->
	{noreply,State}.

handle_cast(_Request,State)->
	{noreply,State}.

handle_info({output,Reply},State)->
	{Socket,Transport,_Player}=State,
	Transport:send(Socket,Reply),
	{noreply,State};
handle_info(Info,State)->
	{Socket,Transport,_Player}=State,
	Transport:setopts(Socket,[{active,once}]),
	handle(Info,State).
	
notify_conn_closed(Player)->
	gen_fsm:send_all_state_event(Player,conn_closed).

handle({tcp,Socket,Packet},State)->
	lager:info("get data from connection@~p, data@~p",[Socket,Packet]),
	{_,_,Player}=State,
	Action=fll3_json_action:handle(Packet),
	lager:info("Action got:~p",[Action]),
	case Action of
		badjson-> 
			notify_conn_closed(Player),
			lager:info("get message is invalid jsoin ~p",[Packet]),
			{stop,badjson,State};
		_-> 
			gen_fsm:send_event(Player,Action),
			{noreply,State}
	end;
handle({tcp_closed,Socket},State)->
	{_,_,Player}=State,
	notify_conn_closed(Player),
	lager:info("connection closed @~p",[Socket]),
	{stop,conn_closed,State};

handle({tcp_error,Socket,Reason},State)->
	lager:info("connection error @~p~n",[Socket]),
	{stop,{conn_error,Reason},State};

handle(timeout,State)->
	lager:info("connection timeout,should close the socket~n"),
	{stop,conn_timeout,State};

handle(Info,State)->
	lager:info("something not handled@~p",[Info]),
	{noreply,State}.

terminate(_Reason,_State)->
	ok.