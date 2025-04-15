program ::= "Hi!" NEWLINE statements "Bye!" NEWLINE

statements ::= statement | statement NEWLINE statements

statement ::= (var_decl | assignment | output_stmt | if_stmt | for_loop | until_loop | ternary_stmt) "."

var_decl ::= "LetsSay" IDENTIFIER "is" value
           | "LetsSay" IDENTIFIER "isAlso" IDENTIFIER

assignment ::= IDENTIFIER "is" expression

expression ::= term
             | expression ("plus" | "minus") term

term ::= factor
       | term ("times" | "dividedBy") factor

factor ::= value | IDENTIFIER | "(" expression ")"

value ::= NUMBER | STRING | BOOLEAN

condition ::= expression comparison_operator expression
comparison_operator ::= "IsEqualTo" | "IsNotEqualTo" | "IsGreaterThan" | "IsLessThan" | "IsAtLeast" | "IsAtMost"

ternary_stmt ::= "When" condition "Then" statement "Otherwise" statement "ThenStop"

if_stmt ::= "When" condition NEWLINE
            "Then" NEWLINE statement NEWLINE 
            "Otherwise" NEWLINE statement NEWLINE 
            "ThenStop"

for_loop ::= "ForAll" IDENTIFIER "in" IDENTIFIER ":" NEWLINE
             statement NEWLINE 
             "StopNow"

until_loop ::= "Until" condition ":" NEWLINE
               statement NEWLINE 
               "NowStop"

output_stmt ::= "Show" expression

IDENTIFIER ::= [a-zA-Z][a-zA-Z0-9]*
NUMBER ::= [0-9]+ ("." [0-9]+)?
STRING ::= "\" ([^"\n\\] | "\\" [\\"nrt])* "\""
BOOLEAN ::= "true" | "false"
NEWLINE ::= "\n"
