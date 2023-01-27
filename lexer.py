from preprocessor import *

def is_alphanumeric(c):
    return c.isalpha() or c.isdigit() or c in "_"

def is_hex_char(c):
    return c in "0123456789abcdefABCDEF"

reserved_words = {
    "true", "false",
    "not", "and", "or",
    "for", "if", "else", "while", "break", "continue", "return",
    "asm", 
    "def", "struct",
    "ubyte", "byte", "word", "uword", "long", "ulong", "boolean",
    "switch", "case",
    "fast", "shared",
    "const", "poke", "peek"
}

operators_arithmetic = [
    "+", "-", "*", "/", "%", "**",
    "!", "&", "|", "^", "~", "<<", ">>", ">>>", ">><", "<<>",
]

operators_comparison = [
    "==", "<", ">", ">=", "<=", "!=", "<>",
]

operators_assignment = [
    "=", "+=", "-=", "*=", "/=", "&=", "|=", "^=", "!=", "<<=", ">>=", ">>>=", ">><=", "<<>=",
]

operators = [
    "(", ")", "[", "]", "{", "}", ".", ",", ":",
] + operators_arithmetic + operators_comparison + operators_assignment

def is_operator(buf):
    buf = "".join(buf)
    return buf in operators

def is_ident(buf):
    if buf:
        if buf[0].isalpha():
            if all(map(is_alphanumeric, buf[1:])):
                return True
    return False

def is_space(buf):
    buf = "".join(buf)
    return buf.isspace()

def is_numeric(buf):
    buf = "".join(buf)
    return buf.isnumeric() or is_hex(buf) or is_binary(buf)

def is_decimal(buf):
    buf = "".join(buf)
    return buf.isnumeric()

def is_hex(buf):
    buf = "".join(buf)

    if len(buf) == 1 and buf.startswith("0"): return True
    if len(buf) == 2 and buf.startswith("0x"): return True
    if len(buf) > 2:
        if buf.startswith("0x") and all(map(is_hex_char, buf[2:])):
            return True
    return False

def is_binary(buf):
    buf = "".join(buf)

    if len(buf) == 1 and buf.startswith("0"): return True
    if len(buf) == 2 and buf.startswith("0b"): return True
    if len(buf) > 2:
        if buf.startswith("0b") and all(map(is_hex_char, buf[2:])):
            return True
    return False

class Name:
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return "__NAME__[%s]" % self.value

class Lexer:
    def __init__(self, filename):
        self.filename = filename

    def scan(self):
        with open(self.filename, "r") as f:
            text = f.read()

        text = self.preprocess(text)
        return list(self.next(text))

    def preprocess(self, text):
        text = text.split("\n")
        text = preprocess_remove_comments(text)
        text = preprocess_remove_empty_lines(text)
        text = preprocess_remove_trailing_spaces(text)
        text = preprocess_replace_indent_dedent(text)
        text = preprocess_literal_strings(text)
        #text = preprocess_idents(text)
        #text = preprocess_literal_0x(text)
        #text = preprocess_remove_empty_lines(text)
        #text = preprocess_ident_to_reserved_words(text)
        
        text = [("START", None)] + text + [("END", None)]
        return text

    def next(self, text):
        for item in text:
            if type(item) == tuple:
                yield item
                continue
        
            buf = []
            for c in list(item) + ["\0"]:
                if c and is_space(buf + [c]):
                    buf = []
                    continue
                elif is_space(buf):
                    buf = [c] if c else []
                    continue

                if c and is_operator(buf + [c]):
                    buf.append(c)
                elif c and is_ident(buf + [c]):
                    buf.append(c)
                elif c and is_numeric(buf + [c]):
                    buf.append(c)
                #elif c and is_hex(buf + [c]):
                #    buf.append(c)
                #elif c and is_decimal(buf + [c]):
                #    buf.append(c)

                elif is_operator(buf):
                    yield ("".join(buf), None)
                    buf = [c] if c else []
                
                elif is_ident(buf):
                    ident = "".join(buf)
                    if ident.lower() == "true":
                        yield ("NUMERIC", 1)
                    elif ident.upper() == "false":
                        yield ("NUMERIC", 0)
                    elif ident.lower() in reserved_words:
                        if ident.upper() in ("UBYTE", "UWORD"):
                            ident = ident[1:]
                        yield (ident.upper(), None)
                    else:
                        yield ("IDENT", ident)
                    buf = [c] if c else []

                elif is_numeric(buf):
                    n = "".join(buf)
                    if n.startswith("0x"):                        
                        yield ("NUMERIC", int(n, 16))
                    elif n.startswith("0b"):                        
                        yield ("NUMERIC", int(n, 2))
                    elif n.isnumeric():
                        yield ("NUMERIC", int(n, 10))
                    else:
                        yield ("ERROR", "syntax error in numeric: %s" % n)
                        return
                    buf = [c] if c else []


if __name__ == "__main__":
    text = """abc 0 0x0
    > >> >>> >>< >= >>= >>>= >><= 
# Foo Bar
abd#
abc#foobar   $123
  abc
"h""m"
    abc    
0x1230x456 0xff       0 0x0x   0123  
car0xdead 0b1111
    for
      abc xyz
abc
    "hello#wor'ld"    

"""
    lex = Lexer(text)
    for t in lex._next():
        print(t)
