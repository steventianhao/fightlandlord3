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

byName(#joker{token=Token})->Token;
byName(#card{rank={_,Name,_}})->Name.

get_value(#joker{value=Value})->Value;
get_value(#card{rank={_,_,Value}})->Value.

is_joker(#joker{})->true;
is_joker(#card{})->false.

compare(C1,C2)->get_value(C1)>get_value(C2).

pattern(Cards)->
	CardsByName=utils:groupBy(fun byName/1,Cards),
	CardsInGroup=lists:sort(fun(L1,L2)->length(L1)>length(L2) end,lists:map(fun({_,List})->List end,dict:to_list(CardsByName))),
	pattern2(CardsInGroup).

pattern2([[C]])->{single,C};
pattern2([[#joker{},#joker{}]])->rocket;
pattern2([[C1,_]])->{pair,C1};
pattern2(Cards=[[_,_],[_,_],[_,_]|_])->
	case lists:all(fun(L)->length(L)==2 end,Cards) of
		true->
			Cs=lists:sort(fun compare/2,[H||[H,_]<-Cards]),
			Top=#card{rank={_,_,V1}}=lists:nth(1,Cs),
			#card{rank={_,_,V2}}=lists:last(Cs),
			LC=length(Cs),
			if 
				(V1-V2+1)==LC -> {pair_straight,Top};
				true ->other
			end;
		false->other
	end;
pattern2([[C1,_,_]])->{triple,C1};
pattern2([[C1,_,_],[_]])->{triple_plus_single,C1};
pattern2([[C1,_,_],[#card{},#card{}]])->{triple_plus_pair,C1};
pattern2(Cards=[[_,_,_],[_,_,_]|_])->
	N3=lists:filtermap(fun(L)->
		case length(L) of 
			3->{true,lists:nth(1,L)};
			_-> false 
		end 
	end,Cards),
	SN3=lists:sort(fun compare/2,N3),
	L3=length(N3),
	Top=#card{rank={_,_,V1}}=lists:nth(1,SN3),
	#card{rank={_,_,V2}}=lists:last(SN3),
	case V1-V2+1 of
		L3->
			LC=length(Cards),
			if 
				L3==LC ->{triple_straight,Top};
				true ->
					N2=lists:filter(fun(L)->length(L)==2 end,Cards),
					AnyJoker=lists:any(fun(L)->is_joker(lists:nth(1,L)) end,N2),
					if
						AnyJoker ->other;
						true ->
							L2=length(N2),
							case L2+L3 of
								LC->
									if 
										L2==L3 ->{triple_straight_pairs,Top};
										L2*2==L3->{triple_straight_singles,Top};
										true-> other
									end;
								_->	
									if
										(LC+L2==L3*2)->{triple_straight_singles,Top};
										true-> other
									end
							end
					end
			end;
		_->other
	end;
pattern2([[C1,_,_,_]])->{bomb,C1};
pattern2([[C1,_,_,_],[_],[_]])->{quad_plus_singles,C1};
pattern2([[C1,_,_,_],[C2,_]])->
	Joker=is_joker(C2),
	if
		Joker->other;
		true->{quad_plus_singles,C1}
	end;
pattern2([[C1,_,_,_],[C2,_],[C3,_]])->
	Joker=is_joker(C2) orelse is_joker(C3),
	if
		Joker-> other;
		true-> {quad_plus_pairs,C1}
	end;
pattern2(Cards=[[_],[_],[_],[_],[_]|_])->
	Cs=lists:sort(fun compare/2,[H||[H]<-Cards]),
	Top=#card{rank={_,_,V1}}=lists:nth(1,Cs),
	#card{rank={_,_,V2}}=lists:last(Cs),
	LC=length(Cs),
	if 
		(V1-V2+1)==LC -> {straight,Top};
		true ->other
	end;
pattern2(_)->other.