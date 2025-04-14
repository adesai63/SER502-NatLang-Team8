% Tokenizer for the language
:- module(tokenizer, [tokenize/2]).

tokenize(Chars, Tokens) :-
    phrase(tokens(Tokens), Chars).

tokens([]) --> [].
tokens([T|Ts]) --> space, token(T), !, tokens(Ts).

% Handle whitespace but not newlines
space --> [C], {char_type(C, space), C \= '\n'}, space.
space --> [].

% Token types
token(id(ID)) --> identifier(IDChars), { atom_chars(ID, IDChars) }.
token(num(N)) --> number(N).
token(str(S)) --> "\"", string_contents(SChars), "\"", { atom_chars(S, SChars) }.
token(bool(B)) --> boolean(B).
token(op(Op)) --> operator(OpChars), { atom_chars(Op, OpChars) }.
token(punct(P)) --> [P], { member(P, ".:(){}[]") }.
token(newline) --> "\n".

% Identifier: starts with letter, continues with letters and digits
identifier([C|Cs]) --> 
    [C], { char_type(C, alpha) }, 
    identifier_rest(Cs).

identifier_rest([C|Cs]) --> 
    [C], { char_type(C, alnum) }, 
    identifier_rest(Cs).
identifier_rest([]) --> [].

% Numbers: integers or decimals
number(N) --> 
    int_part(I), 
    (decimal_part(D) -> { atom_concat(I, D, S), atom_number(S, N) } ; { atom_number(I, N) }).

int_part(I) --> 
    digits(Ds), { atom_chars(I, Ds) }.

decimal_part(D) --> 
    ".", digits(Ds), { atom_chars(Temp, Ds), atom_concat('.', Temp, D) }.

digits([D|Ds]) --> 
    [D], { char_type(D, digit) }, 
    digits(Ds).
digits([D]) --> 
    [D], { char_type(D, digit) }.

% String contents: any chars except quote
string_contents([]) --> [].
string_contents([C|Cs]) --> 
    [C], { C \= '"' }, 
    string_contents(Cs).

% Boolean values
boolean(true) --> "true".
boolean(false) --> "false".

% Operators
operator("plus") --> "plus".
operator("minus") --> "minus".
operator("times") --> "times".
operator("dividedBy") --> "dividedBy".
operator("AsWellAs") --> "AsWellAs".
operator("EitherOr") --> "EitherOr".
operator("IsNot") --> "IsNot".
operator("IsEqualTo") --> "IsEqualTo".
operator("IsNotEqualTo") --> "IsNotEqualTo".
operator("IsGreaterThan") --> "IsGreaterThan".
operator("IsLessThan") --> "IsLessThan".
operator("IsAtLeast") --> "IsAtLeast".
operator("IsAtMost") --> "IsAtMost".