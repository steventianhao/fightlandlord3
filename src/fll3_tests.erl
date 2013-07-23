-module(fll3_tests).
-include_lib("eunit/include/eunit.hrl").

pattern_pair_straight1_test()->
	F3=fll3:new(),
	SL=["D3","C3","D4","C4","D5","C5","D6","C6"],
	?assert(fll3:is_valid(SL,F3)),
	CL=fll3:strlist2cards(SL,F3),
	io:format("~p~n",[CL]),
	?assertMatch({pair_straight,{card,_,{six,"6",6}}},fll3:pattern(CL)).

pattern_pair_straight2_test()->
	F3=fll3:new(),
	SL=["D3","C3","D4","C4","D5","C5"],
	?assert(fll3:is_valid(SL,F3)),
	CL=fll3:strlist2cards(SL,F3),
	io:format("~p~n",[CL]),
	?assertMatch({pair_straight,{card,_,{five,"5",5}}},fll3:pattern(CL)).

pattern_pair_straight3_test()->
	F3=fll3:new(),
	SL=["D3","C3","D4","C4"],
	?assert(fll3:is_valid(SL,F3)),
	CL=fll3:strlist2cards(SL,F3),
	io:format("~p~n",[CL]),
	?assertMatch(other,fll3:pattern(CL)).

pattern_pair_straight4_test()->
	F3=fll3:new(),
	SL=["D3","C3","D4","C4","D5","C5","D6"],
	?assert(fll3:is_valid(SL,F3)),
	CL=fll3:strlist2cards(SL,F3),
	io:format("~p~n",[CL]),
	?assertMatch(other,fll3:pattern(CL)).

pattern_pair_straight5_test()->
	F3=fll3:new(),
	SL=["D3","C3"],
	?assert(fll3:is_valid(SL,F3)),
	CL=fll3:strlist2cards(SL,F3),
	io:format("~p~n",[CL]),
	?assertMatch({pair,{card,_,{three,"3",3}}},fll3:pattern(CL)).