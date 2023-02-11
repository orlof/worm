import functools
from preprocessor import *
from nodes import *

def is_alphanumeric(c):
    return c.isalpha() or c.isdigit() or c in "_"

def is_hex_char(c):
    return c in "0123456789abcdefABCDEF"

reserved_words = {
    "true", "false",
    "not", "and", "or",
    "for", "if", "else", "while", "break", "continue", "return",
    "asm", "end",
    "def", "struct",
    "byte", "word", "int", "long", "string",
    "switch", "case",
    "fast", "shared",
    "const", "poke", "peek",
    "debug", "data", "inline", "goto", "origin"
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

def cmp_const(left, right):
    if left.name in right.idents:
        return -1
    if right.name in left.idents:
        return 1
    return 0

class Lexer:
    def __init__(self):
        self.depth = []

    def scan_text(self, text):
        text = self.preprocess(text)

        constants=AstList()
        for v in text:
            if type(v) == tuple:
                token, value = v
                if token == "CONST":
                    name, value = value
                    node = AstNode(name=name, value=value, idents=[n[1] for n in Lexer().scan_text(value)[0] if n[0] == "IDENT"])
                    constants.append(node)

        constants.sort(key=functools.cmp_to_key(cmp_const))

        text = [v for v in text if v[0] != "CONST"]

        const = {c.name: c.value for c in constants}
        results = []
        next_version = text
        for round in range(5):
            version = list(self.next(next_version))
            next_version = []
            for token, value in version:
                if token == "IDENT" and value in const:
                    next_version.append(const[value])
                else:
                    next_version.append((token, value))
            if version == next_version:
                return next_version, constants
        raise SyntaxError()

    def scan_file(self, filename):
        with open(filename, "r") as f:
            text = f.read()

        return self.scan_text(text)

    def preprocess(self, text):
        text = text.split("\n")
        text = preprocess_constants(text)
        text = preprocess_asm_blocks(text)
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
                if not self.depth or item[0] not in ("INDENT", "DEDENT"):
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
                    if buf in ["(", "[", "{"]:
                        self.depth += buf
                    if buf in [")", "]", "}"]:
                        if not self.depth or c != self.depth.pop():
                            raise SyntaxError("Mismatch () [] or {}")
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

    # for t in Lexer().scan_text("f(\"huuhaa\", 5) + automatic"):
    for t in Lexer().scan_file("examples.worm"):
        print(t)
