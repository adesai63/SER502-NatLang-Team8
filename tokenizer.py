import re

TOKEN_SPEC = [
    ('HI', r'Hi!'),
    ('BYE', r'Bye!'),
    ('NEWLINE', r'\n'),
    ('DOT', r'\.'),
    ('COLON', r':'),
    ('LETS_SAY', r'LetsSay'),
    ('IS', r'is'),
    ('IS_ALSO', r'isAlso'),
    ('SHOW', r'Show'),
    ('WHEN', r'When'),
    ('OTHERWISE', r'Otherwise'),
    ('THEN_STOP', r'ThenStop'),
    ('FOR_ALL', r'ForAll'),
    ('IN', r'in'),
    ('STOP_NOW', r'StopNow'),
    ('UNTIL', r'Until'),
    ('NOW_STOP', r'NowStop'),
    ('THEN', r'Then'),
    ('PLUS', r'plus'),
    ('MINUS', r'minus'),
    ('TIMES', r'times'),
    ('DIVIDED_BY', r'dividedBy'),
    ('AS_WELL_AS', r'AsWellAs'),
    ('EITHER_OR', r'EitherOr'),
    ('IS_NOT', r'IsNot'),
    ('IS_EQUAL_TO', r'IsEqualTo'),
    ('IS_NOT_EQUAL_TO', r'IsNotEqualTo'),
    ('IS_GREATER_THAN', r'IsGreaterThan'),
    ('IS_LESS_THAN', r'IsLessThan'),
    ('IS_AT_LEAST', r'IsAtLeast'),
    ('IS_AT_MOST', r'IsAtMost'),
    ('BOOLEAN', r'true|false'),
    ('IDENTIFIER', r'[a-zA-Z][a-zA-Z0-9]*'),
    ('NUMBER', r'[0-9]+(?:\.[0-9]+)?'),
    ('STRING', r'"[^"\\]*(?:\\.[^"\\]*)*"'),
    ('SKIP', r'[ \t]+'),
    ('MISMATCH', r'.'),
]

def tokenize(code):
    tokens = []
    token_regex = '|'.join(f'(?P<{pair[0]}>{pair[1]})' for pair in TOKEN_SPEC)
    print (token_regex)
    
    for mo in re.finditer(token_regex, code):
        kind = mo.lastgroup
        value = mo.group()
        
        if kind == 'NEWLINE':
            tokens.append('\n')
        elif kind == 'SKIP':
            continue
        elif kind == 'MISMATCH':
            raise RuntimeError(f'Unexpected character: {value}')
        elif kind == 'STRING':
            tokens.append(value)  # Keep quotes for strings
        else:
            tokens.append(value)
    
    return tokens

# Example usage
if __name__ == "__main__":
    # Read the sample program
    with open('program.txt', 'r') as f:
        code = f.read()
    
    # Tokenize and print the result
    tokens = tokenize(code)
    print("Token Stream:")
    print(tokens)
