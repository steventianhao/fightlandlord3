-module(fll3_json_action).
-export([handle/1]).


process({[{<<"kind">>,<<"login">>},{<<"token">>,Token}]})->
	{login,Token};
process({[{<<"kind">>,<<"talk">>},{<<"table">>,Table},{<<"content">>,Content}]}) when is_integer(Table)->
	{talk,Table,Content};
process({[{<<"kind">>,<<"enter_table">>},{<<"table">>,Table}]}) when is_integer(Table) ->
	{enter_table,Table};
process({[{<<"kind">>,<<"exit_table">>},{<<"table">>,Table}]}) when is_integer(Table)->
	{exit_table,Table};
process({[{<<"kind">>,<<"get_ready">>},{<<"table">>,Table}]}) when is_integer(Table)->
	{get_ready,Table};
process(Json)->
	io:format("json is ~p~n",[Json]),
	badjson.

handle(Binary)->
	process(jiffy:decode(Binary)).
