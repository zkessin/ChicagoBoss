-module(boss_web_controller_render_test).
-include_lib("eunit/include/eunit.hrl").


expand_action_result_test() ->
    ?assertEqual({render, [], []}, 
		 boss_web_controller_render:expand_action_result(ok)),
    ?assertEqual({render, [], []}, 
		 boss_web_controller_render:expand_action_result(render)),
    ?assertEqual({render, foo, []}, 
		 boss_web_controller_render:expand_action_result({ok, foo})),
    ?assertEqual({render, foo, []}, 
		 boss_web_controller_render:expand_action_result({render, foo})).
    
