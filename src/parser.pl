:- use_module(library(dcg/basics)).

program(program(hi, Statements, bye)) --> 
    ['Hi!'], newlines, 
    statements(Statements), 
    ['Bye!'], newlines.

% Statements
statements([S|Ss]) --> statement(S), newlines, statements(Ss).
statements([]) --> [].

statement(declare(Id, Val)) --> 
    ['LetsSay'], identifier(Id), ['is'], value(Val), ['.'].

statement(declare_alias(Id1, Id2)) --> 
    ['LetsSay'], identifier(Id1), ['isAlso'], identifier(Id2), ['.'].

statement(assign(Id, Expr)) --> 
    identifier(Id), ['is'], expression(Expr), ['.'].

statement(output(Expr)) --> ['Show'], expression(Expr), ['.'].

statement(if(Cond, Then, Else)) --> 
    ['When'], condition(Cond), newlines,
    ['Then'], newlines, 
    statements(Then),
    ['Otherwise'], newlines,
    statements(Else),
    ['ThenStop'].

statement(for(Item, List, Body)) --> 
    ['ForAll'], identifier(Item), ['in'], identifier(List), [':'], newlines,
    statements(Body),
    ['StopNow'].

statement(until(Cond, Body)) --> 
    ['Until'], condition(Cond), [':'], newlines,
    statements(Body),
    ['NowStop'].

statement(ternary(Cond, Then, Else)) --> 
    ['When'], condition(Cond), ['Then'], statement(Then),
    ['Otherwise'], statement(Else), ['ThenStop'], ['.'].

% Expressions with full operator precedence
expression(Expr) --> logical_or(Expr).

logical_or(or(Left, Right)) --> 
    logical_and(Left), ['EitherOr'], logical_or(Right).
logical_or(Expr) --> logical_and(Expr).

logical_and(and(Left, Right)) --> 
    comparison(Left), ['AsWellAs'], logical_and(Right).
logical_and(Expr) --> comparison(Expr).

comparison(compare(Op, Left, Right)) --> 
    additive(Left), comparison_op(Op), additive(Right).
comparison(Expr) --> additive(Expr).


additive(operator('plus', Left, Right)) --> 
    multiplicative(Left), ['plus'], additive(Right).
additive(operator('minus', Left, Right)) --> 
    multiplicative(Left), ['minus'], additive(Right).
additive(Expr) --> multiplicative(Expr).

multiplicative(operator('times', Left, Right)) --> 
    primary(Left), ['times'], multiplicative(Right).
multiplicative(operator('dividedBy', Left, Right)) --> 
    primary(Left), ['dividedBy'], multiplicative(Right).
multiplicative(Expr) --> primary(Expr).

primary(number(N)) --> [N], {number(N)}.
primary(string(S)) --> [S], {string(S)}.
primary(boolean(true)) --> ['true'].
primary(boolean(false)) --> ['false'].
primary(identifier(Id)) --> identifier(Id).
primary(Expr) --> ['('], expression(Expr), [')'].

% Conditions
condition(condition(Left, Op, Right)) --> 
    expression(Left), comparison_op(Op), expression(Right).


% Values
value(number(N)) --> [N], {number(N)}.
value(string(S)) --> [S], {string(S)}.
value(boolean(true)) --> ['true'].
value(boolean(false)) --> ['false'].
value(list(L)) --> [L], {is_list(L)}.

% Terminals
newlines --> ['\n'], newlines.
newlines --> [].

identifier(Id) --> [Id], {\+ reserved_word(Id), atom(Id)}.

comparison_op('IsEqualTo') --> ['IsEqualTo'].
comparison_op('IsNotEqualTo') --> ['IsNotEqualTo'].
comparison_op('IsGreaterThan') --> ['IsGreaterThan'].
comparison_op('IsLessThan') --> ['IsLessThan'].
comparison_op('IsAtLeast') --> ['IsAtLeast'].
comparison_op('IsAtMost') --> ['IsAtMost'].
comparison_op('IsNot') --> ['IsNot'].

% Reserved words
reserved_word('Hi!').
reserved_word('Bye!').
reserved_word('LetsSay').
reserved_word('is').
reserved_word('isAlso').
reserved_word('Show').
reserved_word('When').
reserved_word('Then').
reserved_word('Otherwise').
reserved_word('ThenStop').
reserved_word('ForAll').
reserved_word('in').
reserved_word('StopNow').
reserved_word('Until').
reserved_word('NowStop').
reserved_word('true').
reserved_word('false').
reserved_word('plus').
reserved_word('minus').
reserved_word('times').
reserved_word('dividedBy').
reserved_word('AsWellAs').
reserved_word('EitherOr').

% Helper predicates
parse_program(Tokens, ParseTree) :-
    phrase(program(ParseTree), Tokens).