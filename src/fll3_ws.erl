-module(fll3_ws).
-behaviour(cowboy_websocket_handler).

-compile([{parse_transform, lager_transform}]).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(BUF_SIZE,512).
-define(SEPARATOR,"\r\n").

-record(state,{player}).

notify_conn_closed(Player)->
	case Player of
		undefined -> ok;
		Pid-> gen_fsm:send_all_state_event(Pid,conn_closed)
	end.

handleMsg(Msg,Req,State)->
	lager:info("get msg from websocket ~p",[Msg]),
	Action=fll3_json_action:handle(Msg),
	lager:info("Action got:~p",[Action]),
	case Action of
		badjson-> 
			notify_conn_closed(State#state.player),
			lager:info("get message is invalid jsoin ~p",[Msg]),
			{shutdown,Req,State};
		Login={login,_Token} ->
			{ok,Pid}=fll3_player_sup:start_player(),
			case gen_fsm:sync_send_event(Pid,Login) of
				ok-> 
					NewState=State#state{player=Pid},
					{ok,Req,NewState};
				err->
					{shutdown,Req,State}
			end;
		_ when State#state.player==undefined->
			{shutdown,Req,State};
		_ ->
			gen_fsm:send_event(State#state.player,Action),
			{ok,Req,State}
	end.

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	{ok, Req, #state{}}.

websocket_handle({text, Msg}, Req, State) ->
	lager:info("get data from connection#~p",[Msg]),
	handleMsg(Msg,Req,State);
			
websocket_handle(Data, Req, State) ->
	lager:info("unexpected data received ~p",[Data]),
	{ok, Req, State}.


websocket_info({output,Reply}, Req, State) ->
	lager:info("websocket info ~p~p",[Reply,State]),
	Msg=[jiffy:encode(Reply),"\r\n"],
	{reply, {text,Msg},Req, State}.
	
websocket_terminate(Reason, _Req, State) ->
	lager:info("websocket terminate ~p~p",[Reason,State]),
	notify_conn_closed(State#state.player),
	ok.