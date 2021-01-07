-module(solver).
-compile(export_all).
-export([solve/1]).

-type clause() :: {integer(), integer(), integer()}.
-type instance() :: [clause()].
-type assignment() :: orddict:orddict({pos_integer(), boolean()}).

-spec solve(instance()) -> {sat, assignment()} | unsat.
solve(I) ->
    Vars = lists:usort(lists:flatten([[abs(T1),abs(T2),abs(T3)] || {T1,T2,T3} <- I])),
    Max = lists:max(Vars),
    NrOfVars = length(Vars),
    case solver_loop(I, Vars, NrOfVars) of
      exhausted -> unsat;
      Assignm   -> {sat, pad_assignment(Assignm, Max)}
    end.

bit(Bit, Number) ->
  (Number bsr Bit) band 1.

to_bool(1) -> true;
to_bool(0) -> false.

solver_loop(Instance, Vars, NrOfVars) ->
  solver_loop(Instance, Vars, NrOfVars, trunc(math:pow(2, NrOfVars)), 0).

solver_loop(_, _, _, N, N) -> exhausted;
solver_loop(Instance, Vars, NrOfVars, End, N) ->
  Bools = [to_bool(bit(B, N)) || B <- lists:seq(0, NrOfVars-1)],
  Assignment = orddict:from_list(lists:zip(Vars, Bools)),
  case check(Instance, Assignment) of
    true -> Assignment;
    false -> solver_loop(Instance, Vars, NrOfVars, End, N+1)
  end.



fetch_with_default(Key, Dict, Default) ->
  case orddict:find(Key, Dict) of
    {ok, Value} -> Value;
    error -> Default
  end.
    

pad_assignment(Assignm, Max) -> 
  [fetch_with_default(I, Assignm, true) || I <- lists:seq(1, Max)].
    


% ---- Checker ---------------------------------------------
-spec check(instance(), assignment()) -> boolean().
check(Instance, A) ->
  lists:all(fun (Clause) -> check_clause(Clause, A) end, Instance).

-spec check_clause(clause(), assignment()) -> boolean().
check_clause({T1, T2, T3}, A) ->
  truth_value(T1, A) or truth_value(T2, A) or truth_value(T3, A).

-spec truth_value(integer(), assignment()) -> boolean().
truth_value(T, A) ->
  Val = orddict:fetch(abs(T), A),
  if 
    T > 0 -> Val;
    T < 0 -> not Val
  end.
