-module(prop_solver).

-include_lib("proper/include/proper.hrl").
-import(solver, [check/2, is_instance/1, to_bool/1]).



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
