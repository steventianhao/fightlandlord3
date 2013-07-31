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

handle_info(Info,State)->
	{Socket,Transport,_Player}=State,
	Transport:setopts(Socket,[{active,once}]),
	handle(Info,State),
	{noreply,State}.

handle({tcp,Socket,Packet},State)->
	lager:info("get data from socket@~p, data@~p",[Socket,Packet]),
	{Socket,Transport,Player}=State,
	Action=fll3_json_action:handle(Packet),
	lager:info("Action got:~p",[Action]),
	case Action of
		badjson->Transport:close(Socket);
		_->gen_fsm:send_event(Player,Action)
	end;
handle({tcp_closed,Socket},_State)->
	io:format("tcp closed from client@~p~n",[Socket]);
handle(timeout,_State)->
	io:format("timeout,should close the socket~n");
handle(Info,_State)->
	io:format("something not handled@~p~n",[Info]).

terminate(_Reason,_State)->
	ok.