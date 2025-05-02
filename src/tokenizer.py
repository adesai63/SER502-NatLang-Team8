import re
import sys
import os

TOKEN_SPEC = [
    ('HI',               r'Hi!'),
    ('BYE',              r'Bye!'),
    ('NEWLINE',          r'\n'),
    ('DOT',              r'\.'),
    ('COLON',            r':'),
    ('LPAR',             r'\('),
    ('RPAR',             r'\)'),
    ('LBRACKET',         r'\['),
    ('RBRACKET',         r'\]'),
    ('COMMA',            r','),

    ('EITHER_OR',        r'EitherOr'),
    ('AS_WELL_AS',       r'AsWellAs'),
    ('DIVIDED_BY',       r'dividedBy'),
    ('TIMES',            r'times'),
    ('PLUS',             r'plus'),
    ('MINUS',            r'minus'),
    ('IS_NOT_EQUAL_TO',  r'IsNotEqualTo'),
    ('IS_EQUAL_TO',      r'IsEqualTo'),
    ('IS_GREATER_THAN',  r'IsGreaterThan'),
    ('IS_LESS_THAN',     r'IsLessThan'),
    ('IS_AT_LEAST',      r'IsAtLeast'),
    ('IS_AT_MOST',       r'IsAtMost'),
    ('IS_NOT',           r'IsNot'),
    ('IS_ALSO',          r'isAlso'),
    ('IS',               r'is'),
    ('LETS_SAY',         r'LetsSay'),
    ('SHOW',             r'Show'),
    ('WHEN',             r'When'),
    ('THEN_STOP',        r'ThenStop'),
    ('THEN',             r'Then'),
    ('OTHERWISE',        r'Otherwise'),
    ('FOR_ALL',          r'ForAll'),
    ('IN',               r'in'),
    ('STOP_NOW',         r'StopNow'),
    ('UNTIL',            r'Until'),
    ('NOW_STOP',         r'NowStop'),
    ('BOOLEAN',          r'(?:true|false)'),

    ('IDENTIFIER',       r'[a-zA-Z][a-zA-Z0-9]*'),
    ('NUMBER',           r'[0-9]+(?:\.[0-9]+)?'),
    ('STRING',           r'"[^"\\]*(?:\\.[^"\\]*)*"'),

    ('SKIP',             r'[ \t]+'),
    ('MISMATCH',         r'.'),
]

def tokenize(code):
    tokens = []
    pattern = '|'.join(f'(?P<{name}>{pat})' for name, pat in TOKEN_SPEC)
    for mo in re.finditer(pattern, code):
        kind, val = mo.lastgroup, mo.group()
        if kind == 'NEWLINE':
            tokens.append('\n')
        elif kind == 'SKIP':
            continue
        elif kind == 'MISMATCH':
            raise RuntimeError(f'Unexpected character: {val}')
        else:
            tokens.append(val)
    return tokens

def main():
    if len(sys.argv) != 2:
        print("Usage: python tokenizer.py <InputFileName>")
        sys.exit(1)

    src = sys.argv[1]
    code = open(src).read()
    toks = tokenize(code)

    base = os.path.splitext(os.path.basename(src))[0]
    out_dir = "tokens"
    os.makedirs(out_dir, exist_ok=True)
    out_path = os.path.join(out_dir, f"{base}.txt")

    literal_pats = {
        pat for nm, pat in TOKEN_SPEC
        if nm not in {'IDENTIFIER','NUMBER','STRING','SKIP','NEWLINE','MISMATCH'}
    }

    with open(out_path, 'w') as f:
        f.write('[')
        for i, t in enumerate(toks):
            if t == '\n':
                s = "'\\n'"
            elif re.fullmatch(r'[0-9]+(?:\.[0-9]+)?', t):
                s = t
            elif re.fullmatch(r'[a-zA-Z][a-zA-Z0-9]*', t) and t not in literal_pats:
                s = t
            else:
                s = f"'{t}'"
            f.write(s)
            if i < len(toks) - 1:
                f.write(', ')
        f.write('].\n')

    print("Tokens written to:", out_path)

if __name__ == "__main__":
    main()
