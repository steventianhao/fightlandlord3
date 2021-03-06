-module(utils).
-export([groupBy/2,shuffle/1,fetch_values/1]).

shuffle(List) ->
	randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
	randomize(List);
randomize(T, List) ->
	lists:foldl(fun(_E, Acc) -> randomize(Acc) end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
	D = lists:map(fun(A) -> {random:uniform(), A} end, List),
	{_, D1} = lists:unzip(lists:keysort(1, D)), 
	D1.

groupBy(F, L) -> 
	lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end , dict:new(), [ {F(X), X} || X <- L ]).

fetch_values(Dict)->
	lists:flatmap(fun(E)->{_Key,Value}=E,Value end,dict:to_list(Dict)).