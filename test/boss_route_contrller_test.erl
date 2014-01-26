-module(boss_route_contrller_test).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../src/boss/boss_web.hrl").
-import(prop_runner, [gen/2,gen/4]).

props_test_() ->
    gen([
         {code_change, 3}
         
        ],
        boss_router_controller).
