import re
import sys
import os

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
    ('THEN_STOP', r'ThenStop'),
    ('THEN', r'Then'),
    ('OTHERWISE', r'Otherwise'),
    ('FOR_ALL', r'ForAll'),
    ('IN', r'in'),
    ('STOP_NOW', r'StopNow'),
    ('UNTIL', r'Until'),
    ('NOW_STOP', r'NowStop'),
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
            tokens.append(value)
        else:
            tokens.append(value)
    
    return tokens

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python tokenizer.py <InputFileName>")
        sys.exit(1)
    
    input_file = sys.argv[1]

    with open(input_file, 'r') as f:
        inputCode = f.read()

    tokens = tokenize(inputCode)

    base_name = os.path.basename(input_file)
    filename_without_ext, _ = os.path.splitext(base_name)
    output_file_name = filename_without_ext + ".txt"

    output_dir = os.path.join("tokens")
    os.makedirs(output_dir, exist_ok=True)

    output_path = os.path.join(output_dir, output_file_name)

    with open(output_path, 'w') as f:
        f.write(str(tokens))
    
    print("Tokens written to:", output_path)