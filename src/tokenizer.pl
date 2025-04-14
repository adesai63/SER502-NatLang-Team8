:- use_module(library(pcre)).

% Tokenization rule
tokenize(Input, Tokens) :-
    split_string(Input, "\n", "", Lines),
    maplist(tokenize_line, Lines, TokenLines),
    append(TokenLines, Tokens).

tokenize_line(Line, Tokens) :-
    % Use a more basic regex to handle word and punctuation splitting.
    re_split('\\s+|([\\W_]+)', Line, Parts),
    exclude(=(""), Parts, CleanParts),
    maplist(classify_token, CleanParts, Tokens).

classify_token(Raw, Token) :-
    atom_string(Atom, Raw),
    (atom_number(Atom, Number) -> Token = number(Number);
    Atom == "true" -> Token = boolean(true);
    Atom == "false" -> Token = boolean(false);
    sub_atom(Atom, 0, 1, _, '"') -> 
        sub_atom(Atom, 1, _, 1, Content),
        Token = string(Content);
    Token = atom(Atom)).
