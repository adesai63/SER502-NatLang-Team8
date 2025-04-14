% Program Structure
program --> ['Hi!'], newline, statements, ['Bye!'], {Program = Statements}.

% Statements
statements([Stmt|Rest]) --> statement(Stmt), newline, statements(Rest).
statements([]) --> [].

% Statement with dot termination
statement --> statement_core, ['.'].

% Core statements
statement_core --> var_declaration.
statement_core --> assignment.
statement_core --> output_stmt.
statement_core --> if_stmt.
statement_core --> for_loop.
statement_core --> until_loop.
statement_core --> ternary_stmt.

% Variable declaration
var_declaration --> ['LetsSay'], identifier, ['is'], value.
var_declaration --> ['LetsSay'], identifier, ['isAlso'], identifier.

% Assignment
assignment --> identifier, ['is'], expression.

% Expressions with no left recursion
expression(Expr) --> term(T), expression_tail(T, Expr).
expression_tail(Acc, Expr) -->
    arithmetic_operator(Op), term(T),
    { Expr1 =.. [Op, Acc, T] },
    expression_tail(Expr1, Expr).
expression_tail(Acc, Acc) --> [].

term --> value.
term --> identifier.

% Value types
value --> number.
value --> string.
value --> boolean.

% Operators
arithmetic_operator(plus) --> ['plus'].
arithmetic_operator(minus) --> ['minus'].
arithmetic_operator(times) --> ['times'].
arithmetic_operator(dividedBy) --> ['dividedBy'].

% Conditions
condition --> expression(Left), comparison_operator(Op), expression(Right),
              { Cond =.. [Op, Left, Right] }.

comparison_operator(asWellAs) --> ['AsWellAs'].
comparison_operator(eitherOr) --> ['EitherOr'].
comparison_operator(isNot) --> ['IsNot'].
comparison_operator(isEqualTo) --> ['IsEqualTo'].
comparison_operator(isNotEqualTo) --> ['IsNotEqualTo'].
comparison_operator(isGreaterThan) --> ['IsGreaterThan'].
comparison_operator(isLessThan) --> ['IsLessThan'].
comparison_operator(isAtLeast) --> ['IsAtLeast'].
comparison_operator(isAtMost) --> ['IsAtMost'].

% Ternary statement
ternary_stmt --> ['When'], condition, ['Then'], statement, ['Otherwise'], statement, ['ThenStop'].

% If statement
if_stmt --> ['When'], condition, newline,
            ['Then'], newline,
            statement, newline,
            ['Otherwise'], newline,
            statement, newline,
            ['ThenStop'].

% For loop (Count from ... to ... as ...)
for_loop --> ['Count', 'from'], number, ['to'], number, ['as'], identifier.

% Until loop
until_loop --> ['Until'], condition, [:], newline,
               statement, newline,
               ['NowStop'].

% Output statement (multiple expressions)
output_stmt --> ['Show'], expression_list.

expression_list --> expression(_), expression_list_tail.
expression_list_tail --> [',' ], expression(_), expression_list_tail.
expression_list_tail --> [].

% Basic elements
identifier --> [X], { atom(X), atom_codes(X, Codes), identifier_check(Codes) }.
number --> [X], { number(X) }.
string --> [X], { string(X) }.
boolean --> ['true'].
boolean --> ['false'].
newline --> [newline].

% Identifier validation
identifier_check([H|T]) :- code_type(H, alpha), identifier_rest(T).
identifier_rest([]).
identifier_rest([H|T]) :- (code_type(H, alnum); H = 95), identifier_rest(T).
