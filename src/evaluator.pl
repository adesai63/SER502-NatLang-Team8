% evaluator.pl

:- use_module(library(assoc)).
:- consult(parser).    % loads DCG from parser.pl

%------------------------------------------------------------------------
% Entry point: parse+evaluate
%------------------------------------------------------------------------
run_program(Tokens) :-
    % turn Tokens into a parse tree
    parse_program(Tokens, ParseTree),
    % start with an empty environment
    empty_env(Env),
    % evaluate everything
    evaluate_program(ParseTree, Env, _).

%------------------------------------------------------------------------
% Program / Statement list
%------------------------------------------------------------------------
evaluate_program(program(_, Statements, _), Env, FinalEnv) :-
    evaluate_statements(Statements, Env, FinalEnv).

evaluate_statements([], Env, Env).
evaluate_statements([Stmt|Rest], Env, FinalEnv) :-
    evaluate_statement(Stmt, Env, Env1),
    evaluate_statements(Rest, Env1, FinalEnv).

%------------------------------------------------------------------------
% Individual statements
%------------------------------------------------------------------------
evaluate_statement(declare(Id, Val), Env, NewEnv) :-
    evaluate_value(Val, Env, V),
    set_value(Env, Id, V, NewEnv).

evaluate_statement(declare_alias(Id1, Id2), Env, NewEnv) :-
    get_value(Env, Id2, V),
    set_value(Env, Id1, V, NewEnv).

evaluate_statement(assign(Id, Expr), Env, NewEnv) :-
    evaluate_expression(Expr, Env, V),
    set_value(Env, Id, V, NewEnv).

evaluate_statement(output(Expr), Env, Env) :-
    evaluate_expression(Expr, Env, V),
    format('~w~n', [V]).

evaluate_statement(if(Cond, Then, Else), Env, FinalEnv) :-
    evaluate_condition(Cond, Env, Bool),
    ( Bool == true
    -> evaluate_statements(Then, Env, FinalEnv)
    ;  evaluate_statements(Else, Env, FinalEnv)
    ).

evaluate_statement(for(Item, ListId, Body), Env, FinalEnv) :-
    get_value(Env, ListId, List),
    is_list(List),
    evaluate_for_loop(Item, List, Body, Env, FinalEnv).

evaluate_statement(until(Cond, Body), Env, FinalEnv) :-
    evaluate_until_loop(Cond, Body, Env, FinalEnv).

evaluate_statement(ternary(Cond, Then, Else), Env, FinalEnv) :-
    evaluate_condition(Cond, Env, Bool),
    ( Bool == true
    -> evaluate_statement(Then, Env, FinalEnv)
    ;  evaluate_statement(Else, Env, FinalEnv)
    ).

%------------------------------------------------------------------------
% Loop helpers
%------------------------------------------------------------------------
evaluate_for_loop(_, [], _, Env, Env).
evaluate_for_loop(Item, [H|T], Body, Env, FinalEnv) :-
    % H may be a literal AST; evaluate it first
    evaluate_value(H, Env, HVal),
    set_value(Env, Item, HVal, Env1),
    evaluate_statements(Body, Env1, Env2),
    evaluate_for_loop(Item, T, Body, Env2, FinalEnv).

evaluate_until_loop(Cond, Body, Env, FinalEnv) :-
    evaluate_condition(Cond, Env, Bool),
    ( Bool == true
    -> FinalEnv = Env
    ;  evaluate_statements(Body, Env, Env1),
       evaluate_until_loop(Cond, Body, Env1, FinalEnv)
    ).

%------------------------------------------------------------------------
% Expression evaluation
%------------------------------------------------------------------------
evaluate_expression(number(N), _, N).
evaluate_expression(string(S), _, S).
evaluate_expression(boolean(B), _, B).
evaluate_expression(identifier(Id), Env, V) :-
    get_value(Env, Id, V).

evaluate_expression(operator('plus', L, R), Env, V) :-
    evaluate_expression(L, Env, LV),
    evaluate_expression(R, Env, RV),
    V is LV + RV.
evaluate_expression(operator('minus', L, R), Env, V) :-
    evaluate_expression(L, Env, LV),
    evaluate_expression(R, Env, RV),
    V is LV - RV.
evaluate_expression(operator('times', L, R), Env, V) :-
    evaluate_expression(L, Env, LV),
    evaluate_expression(R, Env, RV),
    V is LV * RV.
evaluate_expression(operator('dividedBy', L, R), Env, V) :-
    evaluate_expression(L, Env, LV),
    evaluate_expression(R, Env, RV),
    RV =\= 0,
    V is LV / RV.

evaluate_expression(or(L, R), Env, V) :-
    evaluate_expression(L, Env, LV),
    ( LV == true -> V = true ; evaluate_expression(R, Env, V) ).
evaluate_expression(and(L, R), Env, V) :-
    evaluate_expression(L, Env, LV),
    ( LV == false -> V = false ; evaluate_expression(R, Env, V) ).

evaluate_expression(compare(Op, L, R), Env, V) :-
    evaluate_expression(L, Env, LV),
    evaluate_expression(R, Env, RV),
    evaluate_comparison(Op, LV, RV, V).
evaluate_expression(Expr, Env, Val) :-
    Expr =.. [_, Sub],
    evaluate_expression(Sub, Env, Val).

%------------------------------------------------------------------------
% Conditions & comparisons
%------------------------------------------------------------------------
evaluate_condition(condition(L,Op,R), Env, Bool) :-
    evaluate_expression(L, Env, LV),
    evaluate_expression(R, Env, RV),
    evaluate_comparison(Op, LV, RV, Bool).

evaluate_comparison('IsEqualTo',    L, R, true)  :- L == R.
evaluate_comparison('IsEqualTo',    L, R, false) :- L \== R.
evaluate_comparison('IsNotEqualTo', L, R, true)  :- L \== R.
evaluate_comparison('IsNotEqualTo', L, R, false) :- L == R.
evaluate_comparison('IsGreaterThan',L, R, true)  :- L > R.
evaluate_comparison('IsGreaterThan',L, R, false) :- L =< R.
evaluate_comparison('IsLessThan',   L, R, true)  :- L < R.
evaluate_comparison('IsLessThan',   L, R, false) :- L >= R.
evaluate_comparison('IsAtLeast',    L, R, true)  :- L >= R.
evaluate_comparison('IsAtLeast',    L, R, false) :- L < R.
evaluate_comparison('IsAtMost',     L, R, true)  :- L =< R.
evaluate_comparison('IsAtMost',     L, R, false) :- L > R.
evaluate_comparison('IsNot',        L, R, true)  :- L \== R.
evaluate_comparison('IsNot',        L, R, false) :- L == R.

%------------------------------------------------------------------------
% Literal values & environment
%------------------------------------------------------------------------
evaluate_value(number(N), _, N).
evaluate_value(string(S), _, S).
evaluate_value(boolean(true),  _, true).
evaluate_value(boolean(false), _, false).
evaluate_value(list(L), _, L) :- is_list(L).
evaluate_value(identifier(Id), Env, V) :- get_value(Env, Id, V).

empty_env(Env) :- empty_assoc(Env).

get_value(Env, Id, Value) :-
    atom(Id),
    get_assoc(Id, Env, Value).

set_value(Env, Id, Value, NewEnv) :-
    atom(Id),
    put_assoc(Id, Env, Value, NewEnv).