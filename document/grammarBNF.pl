%Program Structure

program ::= "Hi!" NEWLINE statements "Bye!" NEWLINE

%Statements
statements ::= statement NEWLINE statements
             | ε

statement  ::= var_decl "."
             | assignment "."
             | output_stmt "."
             | if_stmt
             | for_loop
             | until_loop
             | ternary_stmt "."

%Variable 
var_decl   ::= "LetsSay" IDENTIFIER "is" value
             | "LetsSay" IDENTIFIER "isAlso" IDENTIFIER

assignment ::= IDENTIFIER "is" expression

%Output
output_stmt ::= "Show" expression

%Control Structures
if_stmt    ::= "When" condition NEWLINE
               "Then" NEWLINE statements
               "Otherwise" NEWLINE statements
               "ThenStop"

for_loop   ::= "ForAll" IDENTIFIER "in" IDENTIFIER ":" NEWLINE
               statements
               "StopNow"

until_loop ::= "Until" condition ":" NEWLINE
               statements
               "NowStop"

ternary_stmt ::= "When" condition "Then" statement "Otherwise" statement "ThenStop"

%Expressions with Precedence
expression ::= logical_or

logical_or ::= logical_and
             | logical_and "EitherOr" logical_or

logical_and ::= comparison
              | comparison "AsWellAs" logical_and

comparison ::= additive
             | additive comparison_operator additive

additive   ::= multiplicative
             | multiplicative "plus" additive
             | multiplicative "minus" additive

multiplicative ::= primary
                 | primary "times" multiplicative
                 | primary "dividedBy" multiplicative

primary    ::= NUMBER
             | STRING
             | BOOLEAN
             | IDENTIFIER
             | "(" expression ")"

%Values 
value      ::= NUMBER
             | STRING
             | BOOLEAN
             | LIST

%Conditions
condition ::= expression comparison_operator expression

%Operators
comparison_operator ::= "IsEqualTo" 
                      | "IsNotEqualTo"
                      | "IsGreaterThan"
                      | "IsLessThan"
                      | "IsAtLeast"
                      | "IsAtMost"
                      | "IsNot"

%Terminals
IDENTIFIER ::= [a-zA-Z][a-zA-Z0-9]*
NUMBER     ::= [0-9]+ ("." [0-9]+)?
STRING     ::= "\"" chars "\""
chars      ::= char | char chars
char       ::= [a-zA-Z0-9 ]
BOOLEAN    ::= "true" | "false"
LIST       ::= "[" (value ("," value)*)? "]"
NEWLINE    ::= "\n"
