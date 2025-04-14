:- use_module(library(readutil)).
:- ensure_loaded(tokenizer).
:- ensure_loaded(parser).
:- ensure_loaded(grammar).

run_program :-
    writeln('Language Parser'),
    writeln('Enter a filename to parse or "exit" to quit:'),
    read_line_to_string(user_input, Input),
    (Input = "exit" -> 
        writeln('Goodbye!') 
    ; 
        process_file(Input),
        run_program
    ).

process_file(Filename) :-
    exists_file(Filename),
    !,
    read_file_to_string(Filename, Content, []),
    tokenize(Content, Tokens),
    (phrase(program(AST), Tokens) ->
        writeln('Parse successful! Abstract Syntax Tree:'),
        print_ast(AST)
    ; writeln('Parse error!')).

process_file(Filename) :-
    format('Error: File "~w" not found.~n', [Filename]).

print_ast([]).
print_ast([H|T]) :- writeln(H), print_ast(T).
