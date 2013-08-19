-module(fll3_rules_tests).
-include_lib("eunit/include/eunit.hrl").

pattern_pair_straight1_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","D4","C4","D5","C5","D6","C6"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	io:format("~p~n",[CL]),
	?assertMatch({pair_straight,{card,_,{six,"6",6}}},fll3_rules:pattern(CL)).

pattern_pair_straight2_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","D4","C4","D5","C5"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	io:format("~p~n",[CL]),
	?assertMatch({pair_straight,{card,_,{five,"5",5}}},fll3_rules:pattern(CL)).

pattern_pair_straight3_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","D4","C4"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	io:format("~p~n",[CL]),
	?assertMatch(other,fll3_rules:pattern(CL)).

pattern_pair_straight4_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","D4","C4","D5","C5","D6"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	io:format("~p~n",[CL]),
	?assertMatch(other,fll3_rules:pattern(CL)).

pattern_pair_straight5_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","D4","C4","D2","C2"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	io:format("~p~n",[CL]),
	?assertMatch(other,fll3_rules:pattern(CL)).

pattern_pair_straight6_test()->
	F3=fll3_rules:new(),
	SL=["DA","CA","DK","CK","D2","C2"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	io:format("~p~n",[CL]),
	?assertMatch(other,fll3_rules:pattern(CL)).

pattern_pair1_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	io:format("~p~n",[CL]),
	?assertMatch({pair,{card,_,{three,"3",3}}},fll3_rules:pattern(CL)).

pattern_pair2_test()->
	F3=fll3_rules:new(),
	SL=["XK","VZ"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch(rocket,fll3_rules:pattern(CL)).

pattern_pair3_test()->
	F3=fll3_rules:new(),
	SL=["XK","VZ","SK"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch(other,fll3_rules:pattern(CL)).

pattern_triple1_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch({triple,{card,_,{three,"3",3}}},fll3_rules:pattern(CL)).

pattern_triple2_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","XK"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch({triple_plus_single,{card,_,{three,"3",3}}},fll3_rules:pattern(CL)).

pattern_triple3_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","XK","VZ"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch(other,fll3_rules:pattern(CL)).

pattern_triple4_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","S2","H2"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch({triple_plus_pair,{card,_,{three,"3",3}}},fll3_rules:pattern(CL)).


pattern_triple5_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","SK","DK"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch({triple_plus_pair,{card,_,{three,"3",3}}},fll3_rules:pattern(CL)).

pattern_triple6_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","XK","VZ"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch(other,fll3_rules:pattern(CL)).

pattern_triple7_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","D4","C4","H4"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch({triple_straight,{card,_,{four,"4",4}}},fll3_rules:pattern(CL)).

pattern_triple8_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","D4","C4","H4","H2","S2"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch({triple_straight_singles,{card,_,{four,"4",4}}},fll3_rules:pattern(CL)).	

pattern_triple9_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","D4","C4","H4","H2","SK"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch({triple_straight_singles,{card,_,{four,"4",4}}},fll3_rules:pattern(CL)).	


pattern_triple10_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","D4","C4","H4","H2","S2","SK","DK"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch({triple_straight_pairs,{card,_,{four,"4",4}}},fll3_rules:pattern(CL)).

pattern_triple11_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","D4","C4","H4","D5","C5","H5","H2","S2","SK"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch({triple_straight_singles,{card,_,{five,"5",5}}},fll3_rules:pattern(CL)).	


pattern_triple12_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","D4","C4","H4","D5","C5","H5","XK","VZ","SK"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch(other,fll3_rules:pattern(CL)).

pattern_triple13_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","D4","C4","H4","H2","S2","XK","VZ"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch(other,fll3_rules:pattern(CL)).

pattern_triple14_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","D4","C4","H4","XK","VZ"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch(other,fll3_rules:pattern(CL)).	

pattern_quad1_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","S3","XK","VZ"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch(other,fll3_rules:pattern(CL)).

pattern_quad2_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","S3","XK","VZ","D4","H4"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch(other,fll3_rules:pattern(CL)).

pattern_quad3_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","S3","D4","H4"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch({quad_plus_singles,{card,_,{three,"3",3}}},fll3_rules:pattern(CL)).


pattern_quad4_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","S3","D4","H4","S5","D5"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch({quad_plus_pairs,{card,_,{three,"3",3}}},fll3_rules:pattern(CL)).


pattern_quad5_test()->
	F3=fll3_rules:new(),
	SL=["D3","C3","H3","S3"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch({bomb,{card,_,{three,"3",3}}},fll3_rules:pattern(CL)).

pattern_single_test()->
	F3=fll3_rules:new(),
	SL=["D3"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch({single,{card,_,{three,"3",3}}},fll3_rules:pattern(CL)).

pattern_single1_test()->
	F3=fll3_rules:new(),
	SL=["D3","D4","D5","D6","D7"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch({straight,{card,_,{seven,"7",7}}},fll3_rules:pattern(CL)).

pattern_single2_test()->
	F3=fll3_rules:new(),
	SL=["D3","D4","D5","D6"],
	?assert(fll3_rules:is_valid(SL,F3)),
	CL=fll3_rules:strlist2cards(SL,F3),
	?assertMatch(other,fll3_rules:pattern(CL)).