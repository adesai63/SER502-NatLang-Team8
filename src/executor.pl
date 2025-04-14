% Executor for the language
:- module(executor, [execute/2, run_program_from_file/2, print_output/1]).
:- use_module(parser).

% Execute a parsed program
execute(Program, Output) :-
    init_env(Env),
    execute_program(Program, Env, _, Output).

% Initialize empty environment
init_env([]).

% Execute full program
execute_program(prog(Statements), Env, FinalEnv, Output) :-
    execute_statements(Statements, Env, FinalEnv, [], Output).

% Execute a list of statements
execute_statements([], Env, Env, Output, Output).
execute_statements([Stmt|Rest], Env, FinalEnv, AccOutput, Output) :-
    execute_statement(Stmt, Env, NextEnv, AccOutput, NextOutput),
    execute_statements(Rest, NextEnv, FinalEnv, NextOutput, Output).

% Execute individual statements
execute_statement(var_decl(ID, Value), Env, [var(ID, Val)|Env], Output, Output) :-
    evaluate_value(Value, Env, Val).
execute_statement(var_decl(ID1, ID2), Env, [var(ID1, Val)|Env], Output, Output) :-
    lookup_var(ID2, Env, Val).
execute_statement(assign(ID, Expr), Env, NewEnv, Output, Output) :-
    evaluate_expr(Expr, Env, Val),
    update_var(ID, Val, Env, NewEnv).
execute_statement(output(Expr), Env, Env, Output, [Val|Output]) :-
    evaluate_expr(Expr, Env, Val).
execute_statement(if_stmt(Condition, ThenStmt, ElseStmt), Env, FinalEnv, Input, Output) :-
    evaluate_condition(Condition, Env, Result),
    (Result = true ->
        execute_statements(ThenStmt, Env, FinalEnv, Input, Output)
    ;
        execute_statements(ElseStmt, Env, FinalEnv, Input, Output)
    ).
execute_statement(for_loop(Var, List, Body), Env, FinalEnv, Input, Output) :-
    lookup_var(List, Env, ListVal),
    execute_for_loop(Var, ListVal, Body, Env, FinalEnv, Input, Output).
execute_statement(until_loop(Condition, Body), Env, FinalEnv, Input, Output) :-
    execute_until_loop(Condition, Body, Env, FinalEnv, Input, Output).
execute_statement(ternary(Condition, ThenStmt, ElseStmt), Env, FinalEnv, Input, Output) :-
    evaluate_condition(Condition, Env, Result),
    (Result = true ->
        execute_statement(ThenStmt, Env, FinalEnv, Input, Output)
    ;
        execute_statement(ElseStmt, Env, FinalEnv, Input, Output)
    ).

% Helpers for evaluation
evaluate_value(num(N), _, N).
evaluate_value(str(S), _, S).
evaluate_value(bool(B), _, B).

evaluate_expr(value(V), Env, Val) :-
    evaluate_value(V, Env, Val).
evaluate_expr(var(ID), Env, Val) :-
    lookup_var(ID, Env, Val).
evaluate_expr(op(Op, E1, E2), Env, Result) :-
    evaluate_expr(E1, Env, V1),
    evaluate_expr(E2, Env, V2),
    apply_operator(Op, V1, V2, Result).

evaluate_condition(cond(E1, Op, E2), Env, Result) :-
    evaluate_expr(E1, Env, V1),
    evaluate_expr(E2, Env, V2),
    apply_comparison(Op, V1, V2, Result).

% Operators
apply_operator("plus", A, B, Result) :- Result is A + B.
apply_operator("minus", A, B, Result) :- Result is A - B.
apply_operator("times", A, B, Result) :- Result is A * B.
apply_operator("dividedBy", A, B, Result) :- Result is A / B.

% Comparisons
apply_comparison("AsWellAs", A, B, Result) :- (A = true, B = true) -> Result = true ; Result = false.
apply_comparison("EitherOr", A, B, Result) :- (A = true; B = true) -> Result = true ; Result = false.
apply_comparison("IsNot", A, _, Result) :- A = false -> Result = true ; Result = false.
apply_comparison("IsEqualTo", A, B, Result) :- A = B -> Result = true ; Result = false.
apply_comparison("IsNotEqualTo", A, B, Result) :- A \= B -> Result = true ; Result = false.
apply_comparison("IsGreaterThan", A, B, Result) :- A > B -> Result = true ; Result = false.
apply_comparison("IsLessThan", A, B, Result) :- A < B -> Result = true ; Result = false.
apply_comparison("IsAtLeast", A, B, Result) :- A >= B -> Result = true ; Result = false.
apply_comparison("IsAtMost", A, B, Result) :- A =< B -> Result = true ; Result = false.

% Variable management
lookup_var(ID, Env, Val) :-
    member(var(ID, Val), Env), !.
lookup_var(ID, _, _) :-
    format('ERROR: Variable ~w not found~n', [ID]),
    fail.

update_var(ID, Val, [], [var(ID, Val)]).
update_var(ID, Val, [var(ID, _)|Rest], [var(ID, Val)|Rest]) :- !.
update_var(ID, Val, [Var|Rest], [Var|NewRest]) :-
    update_var(ID, Val, Rest, NewRest).

% Loop execution
execute_for_loop(_, [], _, Env, Env, Output, Output).
execute_for_loop(Var, [Item|Rest], Body, Env, FinalEnv, Input, Output) :-
    update_var(Var, Item, Env, NewEnv),
    execute_statements(Body, NewEnv, TempEnv, Input, TempOutput),
    execute_for_loop(Var, Rest, Body, TempEnv, FinalEnv, TempOutput, Output).

execute_until_loop(Condition, Body, Env, FinalEnv, Input, Output) :-
    evaluate_condition(Condition, Env, Result),
    (Result = true ->
        FinalEnv = Env, Output = Input
    ;
        execute_statements(Body, Env, TempEnv, Input, TempOutput),
        execute_until_loop(Condition, Body, TempEnv, FinalEnv, TempOutput, Output)
    ).

% Main execution function that reads from a file
run_program_from_file(Filename, Output) :-
    parser:parse_file(Filename, AST),  % Explicitly use parser:parse_file/2
    execute(AST, ReverseOutput),
    reverse(ReverseOutput, Output).

% Helper to display output
print_output([]).
print_output([Item|Rest]) :-
    format('~w~n', [Item]),
    print_output(Rest).