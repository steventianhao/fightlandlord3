-module(fll3).
-export([start/0,start/1]).

start()->
	start(lager),
	start(gproc),
	start(ranch),
	start(cowboy),
	start(fll3).
	

start(App)->
	V=application:start(App),
	case V of
		ok->ok;
		{error,{already_started,_}}->ok;
		{error,Reason}->error({no_such_app,Reason})
	end.