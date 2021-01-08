-module(prop_solver).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-import(solver, [check/2]).



% ---- Generators ------------------------------------------
clausify([T1,T2,T3|Rest]) -> [{T1,T2,T3} | clausify(Rest)];
clausify(_) -> [].

gen_instance() ->
  ?LET(Terms, list(union([pos_integer(), neg_integer()])), clausify(Terms)).

gen_unsat_instance() ->
  ?LET(Inst, gen_instance(),
    begin
      InstWithBadClauses = [{1,1,1},{-1,-1,-1} | Inst],
      Shuffled = lists:sort([{random:uniform(), C} || C <- InstWithBadClauses]),
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
