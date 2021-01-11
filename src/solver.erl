-module(solver).
-compile(export_all).
-export([is_instance/1, solve/1]).

-type clause() :: {integer(), integer(), integer()}.
-type variable() :: pos_integer().
-type instance() :: [clause()].
-type assignment() :: orddict:orddict({variable(), boolean()}).

-spec is_instance(term()) -> boolean().
% TODO: Implement!
is_instance(Term) -> is_list(Term).

% ---- Solver ----------------------------------------------
-spec solve(instance()) -> {sat, assignment()} | unsat.
% Solve an instance.
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
solve(Parent, I) ->
    Vars = lists:usort(lists:flatten([[abs(T1),abs(T2),abs(T3)] || {T1,T2,T3} <- I])),
    Max = lists:max(Vars),
    NrOfVars = length(Vars),
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
