% Parser for the language
:- module(parser, [parse/2, parse_file/2]).
:- use_module(tokenizer).
:- use_module(grammar).

% Parse a string into an AST
parse(ProgramText, AST) :-
    string_chars(ProgramText, Chars),
    tokenize(Chars, Tokens),
    (phrase(program(AST), Tokens) -> 
        true 
    ; 
        write('Error: Failed to parse program'), nl,
        fail
    ).

% Parse a file into an AST
parse_file(Filename, AST) :-
    read_file_to_string(Filename, ProgramText, []),
    parse(ProgramText, AST).