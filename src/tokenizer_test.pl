:- use_module(tokenizer).

test_tokenizer :-
    write('Testing tokenizer on a simple string...'), nl,
    String = "LetsSay x is 5.",
    string_chars(String, Chars),
    (tokenize(Chars, Tokens) -> 
        write('Success! Tokens: '), write(Tokens), nl
    ;
        write('Failed to tokenize simple string.'), nl
    ).

test_string_literal :-
    write('Testing tokenizer on a string with quotes...'), nl,
    String = "LetsSay message is \"Hello World\".",
    string_chars(String, Chars),
    (tokenize(Chars, Tokens) -> 
        write('Success! Tokens: '), write(Tokens), nl
    ;
        write('Failed to tokenize string with quotes.'), nl
    ).

test_from_file(Filename) :-
    write('Testing tokenizer on file content...'), nl,
    read_file_to_string(Filename, Content, []),
    write('File content (first 50 chars): '), 
    string_length(Content, Len),
    (Len > 50 -> Prefix = 50; Prefix = Len),
    sub_string(Content, 0, Prefix, _, Substr),
    write(Substr), write('...'), nl,
    
    string_chars(Content, Chars),
    (tokenize(Chars, Tokens) -> 
        write('Success! Token count: '), length(Tokens, Count), write(Count), nl,
        write('First 5 tokens: '), 
        (Count > 5 -> N = 5; N = Count),
        prefix(Tokens, N, FirstTokens),
        write(FirstTokens), nl
    ;
        write('Failed to tokenize file content.'), nl
    ).

prefix(List, N, Prefix) :-
    length(Prefix, N),
    append(Prefix, _, List).