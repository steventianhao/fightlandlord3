-module(fll3_tcp).
-behavior(ranch_protocol).
-behavior(gen_server).

-export([start_link/4]).
-export([init/4]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

start_link(Ref,Socket,Transport,Opts)->
	proc_lib:start_link(?MODULE,init,[Ref,Socket,Transport,Opts]).

init(Ref,Socket,Transport,_Opts=[])->
	ok=proc_lib:init_ack({ok,self()}),
	ok=ranch:accept_ack(Ref),
	ok=Transport:setopts(Socket,[{active,once},{packet,line},{recbuf,512},{packet_size,512}]),
	%% fll3_super:start_child
	{ok,Timeout}=application:get_env(fll3,timeout),
	gen_server:enter_loop(?MODULE,[],{Socket,Transport},Timeout).

init(_Args)-> ignore.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.

handle_call(_Request,_From,State)->
	{noreply,State}.

handle_cast(_Request,State)->
	{noreply,State}.

handle_info(Info,State)->
	{Socket,Transport}=State,
	Transport:setopts(Socket,[{active,once}]),
	handle(Info,State),
	{noreply,State}.

handle({tcp,Socket,Packet},_State)->
	io:format("get data from socket@~p, data@~p~n",[Socket,Packet]);
handle({tcp_closed,Socket},_State)->
	io:format("tcp closed from client@~p~n",[Socket]);
handle(timeout,_State)->
	io:format("timeout,should close the socket~n");
handle(Info,_State)->
	io:format("something not handled@~p~n",[Info]).

terminate(_Reason,_State)->
	ok.