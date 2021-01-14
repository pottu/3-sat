-module(solver_test).

-include_lib("eunit/include/eunit.hrl").

-import(solver, [solve/1, is_instance/1]).

is_instance_test_() -> [
    ?_assertEqual(false, is_instance(atom)),
    ?_assertEqual(false, is_instance(1)),
    ?_assertEqual(false, is_instance("string")),
    ?_assertEqual(false, is_instance([{1,2}])),
    ?_assertEqual(false, is_instance([{1,2,atom}])),
    ?_assertEqual(false, is_instance([{1,2,0}])),
    ?_assertEqual(true, is_instance([])),
    ?_assertEqual(true, is_instance([{1,2,3},{-1,-2,-3}]))
].
    

