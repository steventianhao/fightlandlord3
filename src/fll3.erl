-module(fll3).

-export([new/0,is_valid/2,strlist2cards/2,pattern/1]).

-record(card,{suit,rank}).
-record(joker,{name,token,value}).
-record(fightlandlord3,{ranks,suits,cards,allcardstr,allcardmap}).

all_suits()->
	[{diamond,"D"},{spade,"S"},{club,"C"},{heart,"H"}].

all_ranks()->
	[{two,"2",71},{three,"3",3},{four,"4",4},{five,"5",5},{six,"6",6},{seven,"7",7},{eight,"8",8},{nine,"9",9},{ten,"T",10},{jack,"J",11},{queen,"Q",12},{king,"K",13},{ace,"A",14}].

all_cards(AllSuits,AllRanks)->
	[#joker{name=big,token="X",value=100},#joker{name=little,token="X",value=99}]++ lists:flatmap(fun(Suit)-> lists:map(fun(Rank)->#card{suit=Suit,rank=Rank} end,AllRanks) end ,AllSuits).

cards2map(Cards)-> dict:from_list([{to_str(Card),Card}||Card <- Cards]).

to_str(#joker{name=big})->"XK";
to_str(#joker{name=little})->"VZ";
to_str(#card{suit={_,Srep},rank={_,Rrep,_}})->Srep++Rrep.


is_valid(CardStrList,#fightlandlord3{allcardstr=AllCardStr})->
	sets:is_subset(sets:from_list(CardStrList),sets:from_list(AllCardStr)).

strlist2cards(CardStrList,#fightlandlord3{allcardmap=AllCardMap})->
	[dict:fetch(Card,AllCardMap)||Card<-CardStrList].

cards2strlist(Cards)->
	[to_str(Card)||Card <- Cards].

new()->
	AllRanks=all_ranks(),
	AllSuits=all_suits(),
	AllCards=all_cards(AllSuits,AllRanks),
	AllCardStr=cards2strlist(AllCards),
	AllCardMap=cards2map(AllCards),
	#fightlandlord3{ranks=AllRanks,suits=AllSuits,cards=AllCards,allcardstr=AllCardStr,allcardmap=AllCardMap}.

groupBy(F, L) -> lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end , dict:new(), [ {F(X), X} || X <- L ]).

pattern(Cards)->
	F=fun(#joker{token=Token})->
			Token;
		(#card{rank={_,Name,_}})->
			Name
	end,
	CardsByName=groupBy(F,Cards),
	lists:sort(fun(L1,L2)->length(L1)>length(L2) end,lists:map(fun({_,List})->List end,dict:to_list(CardsByName))).
