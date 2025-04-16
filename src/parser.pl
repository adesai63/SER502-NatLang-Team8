% Complete, corrected DCG for the custom language
:- use_module(library(dcg/basics)).
% :- use_rendering(svgtree).

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

additive(binop('plus', Left, Right)) --> 
    multiplicative(Left), ['plus'], additive(Right).
additive(binop('minus', Left, Right)) --> 
    multiplicative(Left), ['minus'], additive(Right).
additive(Expr) --> multiplicative(Expr).

multiplicative(binop('times', Left, Right)) --> 
    primary(Left), ['times'], multiplicative(Right).
multiplicative(binop('dividedBy', Left, Right)) --> 
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

% Values (what a variable can be declared as)
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

% Example test cases
test_parser :-
    % Test 1: Basic variable declaration and output
    writeln("Test 1: Basic variable declaration and output"),
    Tokens1 = ['Hi!','\n',
              'LetsSay', x, 'is', 5, '.', '\n',
              'Show', x, '.', '\n',
              'Bye!','\n'],
    parse_program(Tokens1, Parse1),
    writeln(Parse1),
    nl,
    
    % Test 2: Variable assignment and arithmetic
    writeln("Test 2: Variable assignment and arithmetic"),
    Tokens2 = ['Hi!','\n',
              'LetsSay', counter, 'is', 1, '.', '\n',
              'counter', 'is', counter, 'plus', 10, '.', '\n',
              'Show', counter, '.', '\n',
              'Bye!','\n'],
    parse_program(Tokens2, Parse2),
    writeln(Parse2),
    nl,
    
    % Test 3: If-Else statement
    writeln("Test 3: If-Else statement"),
    Tokens3 = ['Hi!','\n',
              'LetsSay', x, 'is', 10, '.', '\n',
              'When', x, 'IsGreaterThan', 5, '\n',
              'Then', '\n',
              'Show', '"x is large"', '.', '\n',
              'Otherwise', '\n',
              'Show', '"x is small"', '.', '\n',
              'ThenStop', '\n',
              'Bye!','\n'],
    parse_program(Tokens3, Parse3),
    writeln(Parse3),
    nl,
    
    % Test 4: For loop
    writeln("Test 4: For loop"),
    Tokens4 = ['Hi!','\n',
              'LetsSay', numbers, 'is', [1, 2, 3], '.', '\n',
              'ForAll', item, 'in', numbers, ':', '\n',
              'Show', item, '.', '\n',
              'StopNow', '\n',
              'Bye!','\n'],
    parse_program(Tokens4, Parse4),
    writeln(Parse4),
    nl,
    
    % Test 5: Until loop
    writeln("Test 5: Until loop"),
    Tokens5 = ['Hi!','\n',
              'LetsSay', counter, 'is', 0, '.', '\n',
              'Until', counter, 'IsEqualTo', 5, ':', '\n',
              'counter', 'is', counter, 'plus', 1, '.', '\n',
              'Show', counter, '.', '\n',
              'NowStop', '\n',
              'Bye!','\n'],
    parse_program(Tokens5, Parse5),
    writeln(Parse5),
    nl,
    
    % Test 6: Nested expressions with operator precedence
    writeln("Test 6: Nested expressions with operator precedence"),
    Tokens6 = ['Hi!','\n',
              'Show', 2, 'plus', 3, 'times', 4, 'dividedBy', '(', 1, 'plus', 1, ')', '.', '\n',
              'Bye!','\n'],
    parse_program(Tokens6, Parse6),
    writeln(Parse6),
    nl,
    
    % Test 7: Logical operators
    writeln("Test 7: Logical operators"),
    Tokens7 = ['Hi!','\n',
              'LetsSay', a, 'is', 'true', '.', '\n',
              'LetsSay', b, 'is', 'false', '.', '\n',
              'When', a, 'AsWellAs', b, 'IsNot', 'true', '\n',
              'Then', '\n',
              'Show', '"Logic works!"', '.', '\n',
              'Otherwise', '\n',
              'Show', '"Unexpected logic"', '.', '\n',
              'ThenStop', '\n',
              'Bye!','\n'],
    parse_program(Tokens7, Parse7),
    writeln(Parse7),
    nl,
    
    % Test 8: Aliases
    writeln("Test 8: Aliases"),
    Tokens8 = ['Hi!','\n',
              'LetsSay', original, 'is', 42, '.', '\n',
              'LetsSay', alias, 'isAlso', original, '.', '\n',
              'Show', alias, '.', '\n',
              'Bye!','\n'],
    parse_program(Tokens8, Parse8),
    writeln(Parse8),
    nl,
    
    % Test 9: Ternary expression
    writeln("Test 9: Ternary expression"),
    Tokens9 = ['Hi!','\n',
              'LetsSay', x, 'is', 10, '.', '\n',
              'When', x, 'IsGreaterThan', 5, 'Then', 'Show', '"big"', '.', 'Otherwise', 'Show', '"small"', '.', 'ThenStop', '.', '\n',
              'Bye!','\n'],
    parse_program(Tokens9, Parse9),
    writeln(Parse9),
    nl,
    
    % Test 10: Complex program (sum of numbers 1 to 10)
    writeln("Test 10: Complex program (sum of numbers 1 to 10)"),
    Tokens10 = ['Hi!','\n',
              'LetsSay', max, 'is', 10, '.', '\n',
              'LetsSay', sum, 'is', 0, '.', '\n',
              'LetsSay', i, 'is', 1, '.', '\n',
              'Until', i, 'IsGreaterThan', max, ':', '\n',
              'sum', 'is', sum, 'plus', i, '.', '\n',
              'i', 'is', i, 'plus', 1, '.', '\n',
              'NowStop', '\n',
              'Show', '"The sum is: "', '.', '\n',
              'Show', sum, '.', '\n',
              'Bye!','\n'],
    parse_program(Tokens10, Parse10),
    writeln(Parse10),
    nl,
    
    % Test 11: All comparison operators
    writeln("Test 11: All comparison operators"),
    Tokens11 = ['Hi!','\n',
              'LetsSay', a, 'is', 5, '.', '\n',
              'LetsSay', b, 'is', 10, '.', '\n',
              'When', a, 'IsEqualTo', a, '\n',
              'Then', '\n',
              'Show', '"Equal test passed"', '.', '\n',
              'Otherwise', '\n',
              'Show', '"Equal test failed"', '.', '\n',
              'ThenStop', '\n',
              'When', a, 'IsNotEqualTo', b, '\n',
              'Then', '\n',
              'Show', '"Not equal test passed"', '.', '\n',
              'Otherwise', '\n',
              'Show', '"Not equal test failed"', '.', '\n',
              'ThenStop', '\n',
              'When', b, 'IsGreaterThan', a, '\n',
              'Then', '\n',
              'Show', '"Greater than test passed"', '.', '\n',
              'Otherwise', '\n',
              'Show', '"Greater than test failed"', '.', '\n',
              'ThenStop', '\n',
              'When', a, 'IsLessThan', b, '\n',
              'Then', '\n',
              'Show', '"Less than test passed"', '.', '\n',
              'Otherwise', '\n',
              'Show', '"Less than test failed"', '.', '\n',
              'ThenStop', '\n',
              'When', a, 'IsAtLeast', a, '\n',
              'Then', '\n',
              'Show', '"At least test passed"', '.', '\n',
              'Otherwise', '\n',
              'Show', '"At least test failed"', '.', '\n',
              'ThenStop', '\n',
              'When', b, 'IsAtMost', b, '\n',
              'Then', '\n',
              'Show', '"At most test passed"', '.', '\n',
              'Otherwise', '\n',
              'Show', '"At most test failed"', '.', '\n',
              'ThenStop', '\n',
              'When', a, 'IsNot', b, '\n',
              'Then', '\n',
              'Show', '"Is not test passed"', '.', '\n',
              'Otherwise', '\n',
              'Show', '"Is not test failed"', '.', '\n',
              'ThenStop', '\n',
              'Bye!','\n'],
    parse_program(Tokens11, Parse11),
    writeln(Parse11),
    nl,
    
    % Test 12: Nested structures
    writeln("Test 12: Nested structures"),
    Tokens12 = ['Hi!','\n',
              'LetsSay', x, 'is', 5, '.', '\n',
              'When', x, 'IsGreaterThan', 0, '\n',
              'Then', '\n',
              'When', x, 'IsLessThan', 10, '\n',
              'Then', '\n',
              'Show', '"x is between 0 and 10"', '.', '\n',
              'Otherwise', '\n',
              'Show', '"x is greater than or equal to 10"', '.', '\n',
              'ThenStop', '\n',
              'Otherwise', '\n',
              'Show', '"x is less than or equal to 0"', '.', '\n',
              'ThenStop', '\n',
              'Bye!','\n'],
    parse_program(Tokens12, Parse12),
    writeln(Parse12).

% Run tests automatically in SWISH
:- initialization(test_parser).