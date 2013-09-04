-module(fll3_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	WebRoot={directory,{priv_dir,fll3,[<<"www">>]}},
	MIME={mimetypes,{fun mimetypes:path_to_mimes/2,default}},
	Dispatch=cowboy_router:compile([
		{'_',[
			{"/",cowboy_static,[WebRoot,MIME,{file,<<"index.html">>}]},
			{"/[...]",cowboy_static,[WebRoot,MIME]},
			{"/ws",fll3_ws,[]}
		]}
	]),
	{ok,_}=cowboy:start_http(http,100,[{port,7171}],[{env,[{dispatch,Dispatch}]}]),
	{ok,_}=ranch:start_listener(fll3_tcp,100,ranch_tcp,[{port,7070},{max_connections,10000}],fll3_tcp,[]),
    fll3_sup:start_link().
    
stop(_State) ->
    ok.