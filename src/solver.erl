-module(solver).
-compile(export_all).
-export([solve/1]).

-type clause() :: {integer(), integer(), integer()}.
-type instance() :: [clause()].
-type assignment() :: dict:dict(pos_integer(), boolean()).

-spec solve(instance()) -> {sat, assignment()} | unsat.
solve(Instance) ->
    Instance.


% ---- Checker ---------------------------------------------
-spec check(instance(), assignment()) -> boolean().
check(Instance, A) ->
  lists:all(fun (Clause) -> check_clause(Clause, A) end, Instance).

-spec check_clause(clause(), assignment()) -> boolean().
check_clause({T1, T2, T3}, A) ->
  truth_value(T1, A) or truth_value(T2, A) or truth_value(T3, A).

-spec truth_value(integer(), assignment()) -> boolean().
truth_value(T, A) ->
  Val = dict:fetch(abs(T), A),
  if 
    T > 0 -> Val;
    T < 0 -> not Val
  end.
