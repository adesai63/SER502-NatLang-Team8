% Main file to load all components and provide an easy interface
:- use_module(tokenizer).
:- use_module(grammar).
:- use_module(parser).
:- use_module(executor).

% Debugging version to see what's happening
debug_run(Filename) :-
    write('Reading file: '), write(Filename), nl,
    catch(read_file_to_string(Filename, ProgramText, []), 
          Error, 
          (write('Error reading file: '), write(Error), nl, fail)),
    write('File read successfully. Length: '), string_length(ProgramText, Len), write(Len), nl,
    write('Program content:'), nl,
    write('-------------------'), nl,
    write(ProgramText), nl,
    write('-------------------'), nl,
    write('Tokenizing...'), nl,
    string_chars(ProgramText, Chars),
    
    % Try tokenizing with extra debugging
    (catch(
        tokenize(Chars, Tokens),
        Error2,
        (write('Tokenizing exception: '), write(Error2), nl, fail)
     ) -> 
        write('Tokenizing successful. Token count: '), length(Tokens, TokenCount), write(TokenCount), nl,
        write('First 10 tokens: '), nl,
        (length(Tokens, TokenLen),
         (TokenLen > 10 -> ToPrint = 10; ToPrint = TokenLen),
         first_n(Tokens, ToPrint, TokensToShow),
         print_tokens(TokensToShow)),
        
        write('Parsing...'), nl,
        (phrase(program(AST), Tokens) -> 
            write('Parsing successful.'), nl,
            write('Executing...'), nl,
            (execute(AST, ReverseOutput) -> 
                write('Execution successful.'), nl,
                reverse(ReverseOutput, Output),
                write('*** OUTPUT ***'), nl,
                print_output(Output)
            ;
                write('Execution failed.'), nl
            )
        ;
            write('Parsing failed.'), nl,
            write('First 20 tokens: '), nl,
            (length(Tokens, TokenLen2),
             (TokenLen2 > 20 -> ToPrint2 = 20; ToPrint2 = TokenLen2),
             first_n(Tokens, ToPrint2, TokensToShow2),
             print_tokens(TokensToShow2))
        )
    ;
        write('Tokenizing failed. Checking first 50 characters:'), nl,
        (length(Chars, CharsLen),
         (CharsLen > 50 -> CharsToShow = 50; CharsToShow = CharsLen),
         first_n(Chars, CharsToShow, FirstChars),
         atom_chars(FirstCharsAtom, FirstChars),
         write(FirstCharsAtom), nl)
    ).

% Run a program from a file and print the output
run(Filename) :-
    write('Starting program execution...'), nl,
    debug_run(Filename).

% Helper to get first N elements of a list
first_n(_, 0, []) :- !.
first_n([], _, []) :- !.
first_n([X|Xs], N, [X|Ys]) :- N > 0, N1 is N - 1, first_n(Xs, N1, Ys).

% Print tokens in a more readable format
print_tokens([]).
print_tokens([Token|Rest]) :-
    write('  '), write(Token), nl,
    print_tokens(Rest).