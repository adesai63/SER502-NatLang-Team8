:- use_module(library(dcg/basics)).

% Program structure
program(ast(Program)) --> ["Hi!"], newlines, statements(Statements), ["Bye!"], {Program = Statements}.

% Statements
statements([Stmt|Rest]) --> statement(Stmt), newlines, statements(Rest).
statements([]) --> [].

% Individual statements
statement(Stmt) --> 
    var_decl(Stmt) | assignment(Stmt) | output_stmt(Stmt) | 
    if_stmt(Stmt) | for_loop(Stmt) | until_loop(Stmt) | 
    ternary_stmt(Stmt).

% Variable declaration
var_decl(var_decl(Id, Val)) --> 
    [atom('LetsSay')], [atom(Id)], [atom('is')], value(Val), [.] .

var_decl(var_alias(Id1, Id2)) --> 
    [atom('LetsSay')], [atom(Id1)], [atom('isAlso')], [atom(Id2)], [.] .

% Assignment
assignment(assign(Id, Expr)) --> 
    [atom(Id)], [atom('is')], expression(Expr), [.] .

% Expressions
expression(Expr) --> 
    term(T), expression_tail(T, Expr).

expression_tail(Acc, Expr) -->
    [atom(Op)], term(T),
    {op_mapping(Op, ActualOp), Expr1 =.. [ActualOp, Acc, T]},
    expression_tail(Expr1, Expr).
expression_tail(Acc, Acc) --> [].

term(T) --> value(T).
term(atom(Id)) --> [atom(Id)].

% Values
value(number(N)) --> [number(N)].
value(string(S)) --> [string(S)].
value(boolean(B)) --> [boolean(B)].

% Control structures
if_stmt(if(Cond, Then, Else)) -->
    [atom('When')], condition(Cond), newlines,
    [atom('Then')], newlines, statements(Then),
    [atom('Otherwise')], newlines, statements(Else),
    [atom('ThenStop')].

until_loop(until(Cond, Body)) -->
    [atom('Until')], condition(Cond), [:], newlines,
    statements(Body), [atom('NowStop')].

for_loop(for(Iter, Body)) -->
    [atom('ForAll')], [atom(Iter)], [atom('in')], [atom('range')], [:], newlines,
    statements(Body), [atom('StopNow')].

% Output statement
output_stmt(show(Exprs)) --> 
    [atom('Show')], expression_list(Exprs), [.] .

expression_list([E|Es]) --> expression(E), ("," -> expression_list(Es); {Es = []}).

% Condition parsing
condition(Cond) -->
    expression(Left), [atom(Op)], expression(Right),
    {op_mapping(Op, ActualOp), Cond =.. [ActualOp, Left, Right]}.

% Operator mapping
op_mapping(plus, +).
op_mapping(minus, -).
op_mapping(times, *).
op_mapping(dividedBy, /).
op_mapping('IsGreaterThan', >).
op_mapping('IsLessThan', <).
op_mapping('IsEqualTo', ==).
% Add other operators as needed

% Whitespace handling
newlines --> [newline], newlines.
newlines --> [].
