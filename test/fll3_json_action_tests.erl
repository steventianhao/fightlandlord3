-module(fll3_json_action_tests).
-include_lib("eunit/include/eunit.hrl").


json2action_test()->
	Login_Json = <<"{\"kind\":\"login\",\"token\":\"wokao\"}">>,
	?assertEqual({login,<<"wokao">>},fll3_json_action:handle(Login_Json)),
	
	Enter_Json = <<"{\"kind\":\"enter_table\",\"table\":1}">>,
	?assertEqual({enter_table,1},fll3_json_action:handle(Enter_Json)),
	
	Exit_Json = <<"{\"kind\":\"exit_table\",\"table\":2}">>,
	?assertEqual({exit_table,2},fll3_json_action:handle(Exit_Json)),
	
	Talk_Json = <<"{\"kind\":\"talk\",\"table\":3,\"content\":\"nimei\"}">>,
	?assertEqual({talk,3,<<"nimei">>},fll3_json_action:handle(Talk_Json)),
	
	Ready_Json = <<"{\"kind\":\"get_ready\",\"table\":3}">>,
	?assertEqual({get_ready,3},fll3_json_action:handle(Ready_Json)),
	
	Lobby_Json= <<"{\"kind\":\"show_lobby\"}">>,
	?assertEqual(show_lobby,fll3_json_action:handle(Lobby_Json)),

	Join_Json= <<"{\"kind\":\"fast_join\"}">>,
	?assertEqual(fast_join,fll3_json_action:handle(Join_Json)),

	Bad_Json = <<"{\"kind\":\"unknown\",\"table\":3}">>,
	?assertEqual(badjson,fll3_json_action:handle(Bad_Json)).

	