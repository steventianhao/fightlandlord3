-module(fll3).
-export([start/0,start/1]).

start()->
	start(gproc),
	start(ranch),
	start(fll3).

start(App)->
	V=application:start(App),
	case V of
		ok->ok;
		{error,{already_started,_}}->ok;
		{error,_}->error(no_such_app)
	end.