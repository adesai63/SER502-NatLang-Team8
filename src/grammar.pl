% DCG Grammar for the language
:- module(grammar, [program/3]).

% Grammar rules
program(prog(Statements)) -->
    [id('Hi')], [punct('!')], optional_newline,
    statements(Statements),
    [id('Bye')], [punct('!')].

optional_newline --> [newline] | [].
optional_newlines --> [newline], optional_newlines | [].

statements([Statement|Rest]) -->
    statement(Statement), optional_newlines, statements(Rest).
statements([]) --> [].

statement(var_decl(ID, Value)) -->
    [id('LetsSay')], [id(ID)], [id('is')], value(Value), [punct('.')].
statement(var_decl(ID1, ID2)) -->
    [id('LetsSay')], [id(ID1)], [id('isAlso')], [id(ID2)], [punct('.')].
statement(assign(ID, Expr)) -->
    [id(ID)], [id('is')], expression(Expr), [punct('.')].
statement(output(Expr)) -->
    [id('Show')], expression(Expr), [punct('.')].
statement(if_stmt(Condition, ThenStmt, ElseStmt)) -->
    [id('When')], condition(Condition), optional_newline,
    [id('Then')], optional_newline, statements(ThenStmt), optional_newline,
    [id('Otherwise')], optional_newline, statements(ElseStmt), optional_newline,
    [id('ThenStop')], [punct('.')].
statement(for_loop(Var, List, Body)) -->
    [id('ForAll')], [id(Var)], [id('in')], [id(List)], [punct(':')], optional_newline,
    statements(Body), optional_newline,
    [id('StopNow')], [punct('.')].
statement(until_loop(Condition, Body)) -->
    [id('Until')], condition(Condition), [punct(':')], optional_newline,
    statements(Body), optional_newline,
    [id('NowStop')], [punct('.')].
statement(ternary(Condition, ThenStmt, ElseStmt)) -->
    [id('When')], condition(Condition), [id('Then')], 
    statement(ThenStmt), [id('Otherwise')], 
    statement(ElseStmt), [id('ThenStop')], [punct('.')].

expression(value(V)) --> value(V).
expression(var(ID)) --> [id(ID)].
expression(op(Op, E1, E2)) -->
    expression(E1), [op(Op)], expression(E2).

value(num(N)) --> [num(N)].
value(str(S)) --> [str(S)].
value(bool(B)) --> [bool(B)].

condition(cond(E1, Op, E2)) -->
    expression(E1), [op(Op)], expression(E2).