-module(solver_test).

-include_lib("eunit/include/eunit.hrl").

-import(solver, [solve/1, is_instance/1, bit/2, 
                 to_bool/1, fetch_with_default/3, pad_assignment/2,
                check_clause/2, truth_value/2]).

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
    
solve_test_() -> 
    [
     ?_assertEqual({sat, []}, solve([])),
     ?_assertEqual({sat, [true, false, false]}, solve([{1, 2, 3}])),
     ?_assertEqual({sat, [true, false, false]}, solve([{1,2,3},{-1,-2,-3}])),
     ?_assertEqual(unsat, solve([{1,1,1},{-1,-1,-1}])),
     ?_assertEqual({sat, [false]}, solve([{1, 1, -1}]))
    ].

bit_test_() ->
    [
     ?_assertEqual(1, bit(0, 3)),
     ?_assertEqual(1, bit(1, 3)),
     ?_assertEqual(0, bit(2, 3))
    ].

to_bool_test_() ->
    [
     ?_assertEqual(true, to_bool(1)),
     ?_assertEqual(false, to_bool(0)),
     ?_assertEqual(true, to_bool(-1))
    ].

fetch_with_default_test_() ->
    I = [{1, 2, 3}, {-1, -2, -3}],
    Vars = lists:usort(lists:flatten([[abs(T1),abs(T2),abs(T3)] || {T1,T2,T3} <- I])),
    NrOfVars = length(Vars),
    Bools = [to_bool(bit(B, 0)) || B <- lists:seq(0, NrOfVars-1)],
    Assignment = orddict:from_list(lists:zip(Vars, Bools)),
    [
     ?_assertEqual(false, fetch_with_default(1, Assignment, true)),
     ?_assertEqual(false, fetch_with_default(2, Assignment, true)),
     ?_assertEqual(false, fetch_with_default(3, Assignment, true)),
     ?_assertEqual(true, fetch_with_default(4, Assignment, true))
    ].

pad_assignment_test_() ->
    I = [{1, 2, 3}, {-1, -2, -3}],
    Vars = lists:usort(lists:flatten([[abs(T1),abs(T2),abs(T3)] || {T1,T2,T3} <- I])),
    NrOfVars = length(Vars),
    Bools = [to_bool(bit(B, 0)) || B <- lists:seq(0, NrOfVars-1)],
    Assignment = orddict:from_list(lists:zip(Vars, Bools)),
    Max = lists:max(Vars),
    [
     ?_assertEqual([false, false, false], pad_assignment(Assignment, Max)),
     ?_assertEqual([false, false, false, true], pad_assignment(Assignment, Max+1))
    ].

truth_value_test_() ->
    I = [{1, 2, 3}, {-1, -2, -3}],
    Vars = lists:usort(lists:flatten([[abs(T1),abs(T2),abs(T3)] || {T1,T2,T3} <- I])),
    NrOfVars = length(Vars),
    Bools = [to_bool(bit(B, 0)) || B <- lists:seq(0, NrOfVars-1)],
    Assignment = orddict:from_list(lists:zip(Vars, Bools)),
    [
     ?_assertEqual(false, truth_value(1, Assignment)),
     ?_assertEqual(false, truth_value(2, Assignment)),
     ?_assertEqual(false, truth_value(3, Assignment))
    ].

check_clause_test_() ->
    I = [{1, 2, 3}, {-1, -2, -3}],
    Vars = lists:usort(lists:flatten([[abs(T1),abs(T2),abs(T3)] || {T1,T2,T3} <- I])),
    NrOfVars = length(Vars),
    Bools = [to_bool(bit(B, 0)) || B <- lists:seq(0, NrOfVars-1)],
    Assignment = orddict:from_list(lists:zip(Vars, Bools)),
    [
     ?_assertEqual(false, check_clause({1, 2, 3}, Assignment))
    ].
