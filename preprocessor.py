import re


def transform_asm(src):
    left_margin = min((len(line) - len(line.lstrip())) for line in src if not line.strip().startswith(";"))

    for index, line in enumerate(src):
        if line.strip().startswith(";") and (len(line) - len(line.lstrip()) < left_margin):
            src[index] = line.strip().replace(";", "//", 1)
        else:
            src[index] = line[left_margin:].rstrip().replace(";", "//", 1)
    return src

def preprocess_backslash_indent_dedent(text):
    # NOT IMPLEMENTED
    buf = []
    bs = False
    depth = 0

    for line in text:
        if not bs:
            if type(line) == str and line.endswith("\\"):
                bs = True
                buf.append(line[:-1])
                continue
            else:
                if depth > 0:
                    if type(line) == tuple and line[0] == "DEDENT":
                        depth -= 1
                        continue
                buf.append(line)
                continue
        else:
            if type(line) == tuple and line[0] == "INDENT":
                bs = False
                depth += 1
                continue
            else:
                raise SyntaxError()

    return buf

def preprocess_asm_blocks(text):
    buf = []
    asm = False
    asm_buf = []

    for line in text:
        if type(line) == tuple:
            buf.append(line)
            continue
        if asm:
            if line.strip().lower() == "end asm":
                asm = False
                buf.append(("ASM_BLOCK", transform_asm(asm_buf)))
                buf.append(line)
            else:
                asm_buf.append(line)
        else:
            if line.strip().lower() == "asm":
                asm = True
                asm_buf = []
            buf.append(line)
    return buf


def preprocess_constants(text):
    buf = []

    for line_nr, line in enumerate(text):
        if type(line) == tuple:
            buf.append(line)
            continue
        r = re.search("^\s*(?:const|CONST)\s*([a-zA-Z_][a-zA-Z_0-9]*)\s*=\s*(.*)\s*$", line)
        if r is not None:
            name = r.group(1)
            value = r.group(2)
            q = ""
            for i, c in enumerate(value):
                if q:
                    if c==q:
                        q = ""
                elif c in "\"'":
                    q = c
                elif c == ";":
                    value = value[:index].strip()
                    break
            buf.append(("CONST", (name, value)))
        else:
            buf.append(line)
    return buf

def preprocess_literal_strings(text):
    buf = []
    for line in text:
        if type(line) == tuple:
            buf.append(line)
            continue

        found = ""
        buf_line = []
        for c in line:
            if not found and c in "\"\'":
                # start quote
                found = c
                if buf_line:
                    buf.append("".join(buf_line))
                buf_line = []
                continue
            elif c == found:
                # end quote
                found = ""
                buf.append(("LITERAL", "".join(buf_line)))
                buf_line = []
                continue
            buf_line.append(c)
        if buf_line:
            if found:
                buf.append(("LITERAL", "".join(buf_line)))
            else:
                buf.append("".join(buf_line))
    return buf

def preprocess_replace_indent_dedent(text):
    indents = [0]
    buf = []
    for line in text:
        if type(line) == tuple:
            buf.append(line)
            continue
        lstrip_line = line.lstrip()
        indent = len(line) - len(lstrip_line)
        if indent > indents[-1]:
            indents.append(indent)
            buf.append(("INDENT", None))
        while indent < indents[-1]:
            buf.append(("DEDENT", None))
            indents.pop()
        if indent != indents[-1]:
            buf.append(("ERROR", "Unknown dedent"))
            return buf
        buf.append(lstrip_line)
    while indents.pop():
        buf.append(("DEDENT", None))
    return buf

def preprocess_remove_trailing_spaces(text):
    buf = []
    for line in text:
        if type(line) == tuple:
            buf.append(line)
            continue
        buf.append(line.rstrip())
    return buf

def preprocess_remove_empty_lines(text):
    buf = []
    for line in text:
        if type(line) == tuple:
            buf.append(line)
            continue
        if not line or line.isspace():
            continue
        buf.append(line)
    return buf

def preprocess_remove_comments(text):
    buf = []
    for line in text:
        if type(line) == tuple:
            buf.append(line)
            continue
        quote = ""
        buf_line = []
        for c in line:
            if not quote:
                if c in "\"\'":
                    quote = c
                elif c == ";":
                    break
            elif quote == c:
                quote = ""
            buf_line.append(c)
        buf.append("".join(buf_line))
    return buf

