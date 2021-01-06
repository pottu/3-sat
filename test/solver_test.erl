-module(solver_test).

-include_lib("eunit/include/eunit.hrl").

-import(solver, [solve/1]).

solver_test() ->
    ?_assertEqual(1, solve(1)).

solver_second_test() ->
    ?_assertEqual(2, solve(2)).
