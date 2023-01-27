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
                    buf.append(("STR", "".join(buf_line)))
                buf_line = []
                continue
            elif c == found:
                # end quote
                found = ""
                buf.append(("STR", "".join(buf_line)))
                buf_line = []
                continue
            buf_line.append(c)
        if buf_line:
            if found:
                buf.append(("STR", "".join(buf_line)))
            else:
                buf.append("".join(buf_line))
    return buf

def preprocess_replace_indent_dedent(text):
    indents = [0]
    buf = []
    for line in text:
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
        quote = ""
        buf_line = []
        for c in line:
            if not quote:
                if c in "\"\'":
                    quote = c
                elif c == "#":
                    break                    
            elif quote == c:
                quote = ""
            buf_line.append(c)
        buf.append("".join(buf_line))
    return buf

