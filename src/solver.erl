-module(solver).
-compile(export_all).
-export([is_instance/1, solve/1]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-type clause() :: {integer(), integer(), integer()}.
-type variable() :: pos_integer().
-type instance() :: [clause()].
-type assignment() :: orddict:orddict({variable(), boolean()}).

-spec is_instance(term()) -> boolean().
is_instance([]) -> true;
is_instance([{A, B, C} | Rest]) when is_integer(A) and is_integer(B) and is_integer(C) ->
    (A =/= 0) and (B =/= 0) and (C =/= 0) and is_instance(Rest);
is_instance(_) -> false.

% ---- Solver ----------------------------------------------
-spec solve(instance()) -> {sat, assignment()} | unsat.
% Solve an instance.
solve([]) -> {sat, []};
solve(I) ->
    Vars = lists:usort(lists:flatten([[abs(T1),abs(T2),abs(T3)] || {T1,T2,T3} <- I])),
    Max = lists:max(Vars),
    NrOfVars = length(Vars),
    case solver_loop(I, Vars, NrOfVars) of
      exhausted -> unsat;
      Assignm   -> {sat, pad_assignment(Assignm, Max)}
    end.

-spec pad_assignment(assignment(), variable()) -> [boolean()].
% Extract assigned truth-values and pad any holes due to
% missing variable identifiers.
pad_assignment(Assignm, Max) -> 
  [fetch_with_default(I, Assignm, true) || I <- lists:seq(1, Max)].

-spec solver_loop(instance(), [variable()], pos_integer()) -> assignment() | exhausted.
% Default param. call for solver_loop/5.
solver_loop(Instance, Vars, NrOfVars) ->
  solver_loop(Instance, Vars, NrOfVars, trunc(math:pow(2, NrOfVars)), 0).

-spec solver_loop(instance(), [variable()], pos_integer(), 
                  non_neg_integer(), non_neg_integer()) 
                  -> assignment() | exhausted.
% Loops over assignments until a solution is found or the
% search space is exhausted. An assignment is represented by
% the number N, when seen in binary.  Loops from N to (and
% not including) End.
solver_loop(_, _, _, N, N) -> exhausted;
solver_loop(Instance, Vars, NrOfVars, End, N) ->
  Bools = [to_bool(bit(B, N)) || B <- lists:seq(0, NrOfVars-1)],
  Assignment = orddict:from_list(lists:zip(Vars, Bools)),
  case check(Instance, Assignment) of
    true -> Assignment;
    false -> solver_loop(Instance, Vars, NrOfVars, End, N+1)
  end.

% ---- Parallelised solving --------------------------------
-spec solve(pid(), instance()) -> no_return().
solve(Parent, []) ->
  Parent ! {solution, {sat, []}},
  exit(done);
solve(Parent, I) ->
    Vars = lists:usort(lists:flatten([[abs(T1),abs(T2),abs(T3)] || {T1,T2,T3} <- I])),
    NrOfVars = length(Vars),
    Max = lists:max(Vars),
    case solver_driver(I, Vars, NrOfVars) of
      exhausted -> Parent ! {solution, unsat};
      Assignm   -> Parent ! {solution, {sat, pad_assignment(Assignm, Max)}}
    end,
    exit(done).

-spec solver_driver(instance(), [variable()], pos_integer()) -> assignment() | exhausted.
solver_driver(Instance, Vars, NrOfVars) ->
  NrOfProcesses = 8,
  NrForParallel = 1000,
  NrOfAssignments = trunc(math:pow(2, NrOfVars)),
  ChunkSize = NrOfAssignments div NrOfProcesses,
  case NrOfAssignments > NrForParallel of
    true -> 
      [spawn_link(?MODULE, solver_process, [self(), Instance, Vars, NrOfVars, Start+ChunkSize, Start]) 
      || Start <- lists:seq(0, NrOfAssignments, ChunkSize)],
      wait_for_solution(NrOfProcesses);
    false -> solver_loop(Instance, Vars, NrOfVars)
  end.

-spec wait_for_solution(non_neg_integer()) -> assignment() | exhausted.
wait_for_solution(0) -> exhausted;
wait_for_solution(N) ->
  receive
    exhausted -> wait_for_solution(N-1);
    Assignment -> Assignment
  end.

-spec solver_process(pid(), instance(), [variable()], pos_integer(), 
                     non_neg_integer(), non_neg_integer()) -> no_return().
solver_process(Parent, Instance, Vars, NrOfVars, End, Start) ->
  Parent ! solver_loop(Instance, Vars, NrOfVars, End, Start).

% ---- Helper functions ------------------------------------
-spec bit(non_neg_integer(), integer()) -> 1 | 0.
% Extract a Bit from a Number.
bit(Bit, Number) ->
  (Number bsr Bit) band 1.

-spec to_bool(integer()) -> boolean().
% Integer-to-boolean converter.
to_bool(0) -> false;
to_bool(_) -> true.

-spec fetch_with_default(term(), dict:dict(), term()) -> term().
% Fetch from Dict with Default value if Key does not exist.
fetch_with_default(Key, Dict, Default) ->
  case orddict:find(Key, Dict) of
    {ok, Value} -> Value;
    error -> Default
  end.
    
    


% ---- Checker ---------------------------------------------
-spec check(instance(), assignment()) -> boolean().
% Check if a given assignment satisfies an instance.
check(Instance, A) ->
  lists:all(fun (Clause) -> check_clause(Clause, A) end, Instance).

-spec check_clause(clause(), assignment()) -> boolean().
% Check if a clause is satisfied with the given assignment.
check_clause({T1, T2, T3}, A) ->
  truth_value(T1, A) or truth_value(T2, A) or truth_value(T3, A).

-spec truth_value(integer(), assignment()) -> boolean().
% Get the truth-value of a given term in the current
% assignment.
truth_value(T, A) ->
  Val = orddict:fetch(abs(T), A),
  if 
    T > 0 -> Val;
    T < 0 -> not Val
  end.

%%% --------------------------------------------------------
%%% ---- PROPER TESTS --------------------------------------
%%% --------------------------------------------------------

% ---- Generators ------------------------------------------
clausify([T1,T2,T3|Rest]) -> [{T1,T2,T3} | clausify(Rest)];
clausify(_) -> [].

gen_instance() ->
  ?LET(Terms, list(union([pos_integer(), neg_integer()])), clausify(Terms)).

gen_unsat_instance() ->
  ?LET(Inst, gen_instance(),
    begin
      InstWithBadClauses = [{1,1,1},{-1,-1,-1} | Inst],
      Shuffled = lists:sort([{rand:uniform(), C} || C <- InstWithBadClauses]),
      [C || {_, C} <- Shuffled]
    end
  ).

gen_assignment(I) ->
    Vars = lists:usort(lists:flatten([[abs(T1),abs(T2),abs(T3)] || {T1,T2,T3} <- I])),
    NrOfVars = length(Vars),
    ?LET(Bools, vector(NrOfVars, boolean()),
      orddict:from_list(lists:zip(Vars, Bools))).

% ---- Checker ---------------------------------------------
prop_checker_fail_unsat_instance() ->
  ?FORALL(I, gen_unsat_instance(),
    ?FORALL(A, gen_assignment(I),
      not solver:check(I,A)
    )
  ).

prop_instance_is_instance() ->
  ?FORALL(I, gen_instance(), is_instance(I)).

% ---- Solver ---------------------------------------------
prop_solver_solve_sat_instance() ->
  ?FORALL(I, gen_instance(),
      is_sat(solver:solve(I))
  ).

%% prop_solver_fail_unsat_instance() ->
%%   ?FORALL(I, gen_unsat_instance(),
%%       not is_sat(solver:solve(I))
%%   ).

% ---- Helper functions ---------------------------------------------
prop_to_bool_pos() ->
    ?FORALL(I, pos_integer(), to_bool(I)).

prop_to_bool_neg() ->
    ?FORALL(I, neg_integer(), to_bool(I)).

% ---- Test helpers ---------------------------------------------
is_sat({sat, _}) ->
    true;

is_sat(_) ->
    false.

%%% --------------------------------------------------------
%%% ---- EUNIT TESTS ---------------------------------------
%%% --------------------------------------------------------

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
