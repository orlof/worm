from dotwiz import DotWiz

from lexer import Lexer, operators_arithmetic, operators_comparison
from types import SimpleNamespace

ZP_W0 = 251

def byte_data_lines(name, data):
    rows = ["%s:" % name]
    data = ["0x{:02x}".format(x) for x in data]
    for index in range(0, len(data), 16):
        rows.append("    %s" % ", ".join(data[index:index+16]))
    return rows

class Node(DotWiz):
    def __str__(self):
        return "%s[%s]" % (self.__class__.__name__, len(self))

    def __repr__(self):
        return "%s[%s]" % (self.__class__.__name__, len(self))

    def tree(self, indent=0):
        main = " " * indent
        sub = " " * (indent + 2)

        subtree = []
        for key, value in self.items():
            if isinstance(value, (Node, NodeList)):
                subtree.append("%s%s:\n%s" % (sub, key, value.tree(indent + 2)))
            else:
                subtree.append("%s%s: %s" % (sub, key, value))

        return "%s%s\n%s" % (sub, self.__class__.__name__, "\n".join(subtree))

class CompiledNode(Node):
    pass

class AstNode(Node):
    def optimize(self, constants, used_constants=set()):
        if self.type == "CONST":
            if self.ident in used_constants:
                raise SyntaxError("Circular CONST definition: %s" % self.ident)
            used_constants = set(list(used_constants) + [self.ident])
        
        while self.type == "IDENT" and self.value in constants:
            if self.value in used_constants:
                raise SyntaxError("Circular CONST definition: %s" % self.value)
            used_constants = set(list(used_constants) + [self.value])
            value = constants[self.value].value
            self.clear()
            self.update(value)

        for k, v in self.copy().items():
            if isinstance(v, (AstNode, AstNodeList)):
                v.optimize(constants, used_constants)
        self._optimize(constants)
    
    def _optimize(self, constants):
        if self["type"] in ["+", "-", "*", "%", "**", "&", "|", "^", "<<", ">>"] + operators_comparison:
            op = self["type"]
            if self["left"]["type"] == "NUMERIC" and self["right"]["type"] == "NUMERIC":
                value = eval('self["left"]["value"] %s self["right"]["value"]' % op)
                self.clear()
                self["type"] = "NUMERIC"
                self["value"] = value
        if self["type"] == "/":
            if self["left"]["type"] == "NUMERIC" and self["right"]["type"] == "NUMERIC":
                value = self["left"]["value"] // self["right"]["value"]
                self.clear()
                self["type"] = "NUMERIC"
                self["value"] = value
        if self["type"] == "!":
            if self["value"]["type"] == "NUMERIC":
                value = ~self["value"]["value"]
                self.clear()
                self["type"] = "NUMERIC"
                self.value = value
        if self.type == "UMINUS":
            if self.value.type == "NUMERIC":
                value = -self.value.value
                self.clear()
                self.type = "NUMERIC"
                self.value = value
        # ">>>", ">><", "<<>" cannot be optimised without knowing the width

class NodeList(list):
    type = "LIST"

    def __str__(self):
        return "%s[%s]" % (self.__class__.__name__, len(self))

    def __repr__(self):
        return "%s[%s]" % (self.__class__.__name__, len(self))

    def tree(self, indent=0):
        main = " " * indent
        sub = " " * (indent + 2)

        subtree = []
        for index, value in enumerate(self):
            if isinstance(value, (Node, NodeList)):
                subtree.append("%s%3d:\n%s" % (sub, index, value.tree(indent + 4)))
            else:
                subtree.append("%s%3d: %s" % (sub, index, value))

        return "%s%s\n%s" % (sub, self.__class__.__name__, "\n".join(subtree))

    def optimize(self, constants, used_constants):
        for v in self:
            if isinstance(v, (Node, NodeList)):
                v.optimize(constants, used_constants)

AstNodeList = NodeList

class Parser:
    def __init__(self, filename):
        with open(filename, "r") as f:
            self.lexer = Lexer(f.read())

        self.advance = self.lexer.advance
        self.constants = {}
        self.variables = []
        self.src_code = []
        self.compiled_code = []

    @property
    def token(self):
        return self.lexer.token

    @property
    def value(self):
        return self.lexer.value

    def eval_type(self, node):
        if node.type == "IDENT":
            return self.variables[node.value].type
        elif node.type == "NUMERIC":
            if 0 <= node.value <= 255:
                return "BYTE"
            if 0 <= node.value <= 65535:
                return "WORD"
            if -32768 <= node.value <= 32767:
                return "INT"
            return "LONG"
        elif node.type == "SUBSCRIPTION":
            return self.eval_type(node.left)
        elif node.type in (operators_arithmetic + operators_comparison):
            left = self.eval_type(node.left)
            right = self.eval_type(node.right)
            if "LONG" in (left, right):
                return "LONG"
            if "INT" in (left, right) and "WORD" in (left, right):
                return "LONG"
            if "WORD" in (left, right):
                return "WORD"
            if "INT" in (left, right):
                return "INT"
            return "BYTE"
        else:
            raise SyntaxError("Unknown assignment target")

    def assignment_type(self, node):
        if node.type == "IDENT":
            return self.variables[node.value].type
        elif node.type == "SUBSCRIPTION":
            return self.assignment_type(node.left)
        else:
            raise SyntaxError("Unknown assignment target")

    def line_iterator(self):
        line_nr = 0
        for node in self.compiled_code.copy():
            for line in node.src:
                yield DotWiz(nr=line_nr, src=line)
                line_nr += 1

    def block_iterator(self, lines):
        block = []
        for line in lines:
            if line.src.startswith("    "):
                block.append(line)
            elif "{" in line.src or "}" in line.src:
                block.append(line)
            else:
                yield block
                block = [line]
        if block:
            yield block

    def block_optimizer(self):
        all_lines = list(self.line_iterator())

        # optimise lda #, pha, pla
        prev_line = ""
        for block in self.block_iterator(all_lines):
            stack = []
            for line in block:
                if line.src.startswith("    pha"):
                    stack.append((prev_line, line))
                if line.src.startswith("    pla") and stack:
                    lda_line, pha_line = stack.pop()

                    if lda_line.src.startswith("    lda #"):
                        line.src = "%s // optimized" % lda_line.src
                        lda_line.src = "    // %s" % lda_line.src[4:]
                        pha_line.src = "    // %s" % pha_line.src[4:]

                if not line.src.startswith("    //"):
                    prev_line = line

        # optimise pha, pla
        for block in self.block_iterator(all_lines):
            for line in block:
                for next_line in all_lines[line.nr + 1:]:
                    if next_line.src.startswith("    //"):
                        continue
                    break

                if line.src.startswith("    pha") and next_line.src.startswith("    pla"):
                    line.src = "    // %s" % line.src[4:]
                    next_line.src = "    // %s" % next_line.src[4:]

        return all_lines

    def compile_variable(self, node):
        for i in node.value:
            if i.type == "IDENT":
                if node.type == "BYTE":
                    # byte a
                    yield CompiledNode(type=node.type, ident=i.value, src=[
                        "%s: .byte 0" % i.value
                    ])

                elif node.type == "WORD":
                    # word a
                    yield CompiledNode(type=node.type, ident=i.value, src=[
                        "%s: .word 0" % i.value
                    ])
                else:
                    raise SyntaxError("Unknown data type %s" % node.type)
            elif i.type == "SUBSCRIPTION":
                if i.left.type != "IDENT" or i.right.type != "NUMERIC":
                    raise SyntaxError("Syntax error in definition")
                if not (0 <= i.right.value <= 65535):
                    raise SyntaxError("Out of bounds")

                if node.type == "BYTE":
                    # byte[10]
                    yield CompiledNode(type=node.type, ident=i.left.value, size=i.right.value, 
                        index_type="BYTE" if i.right.value <= 256 else "WORD", 
                        src=byte_data_lines(i.left.value, [0]*i.right.value)
                    )
                elif node.type == "WORD":
                    # word[10]
                    yield CompiledNode(type=node.type, ident=i.left.value, size=i.right.value, 
                        index_type="BYTE" if i.right.value <= 256 else "WORD",
                        src=
                            byte_data_lines("%s_lo" % i.left.value, [0]*i.right.value) +
                            byte_data_lines("%s_hi" % i.left.value, [0]*i.right.value)
                    )
                else:
                    raise SyntaxError("Unknown data type %s" % node.type)
                
            elif i.type == "=":
                if i.left.type != "IDENT":
                    raise SyntaxError("Syntax error in definition")
                if i.right.type == "NUMERIC":
                    if node.type == "BYTE":
                        # byte a=1
                        if not (0 <= i.right.value <= 255):
                            raise SyntaxError("Out of bounds: %s %d" % (i.left.value, i.right.value))
                        yield CompiledNode(type=node.type, ident=i.left.value, src=[
                            "%s: .byte %d" % (i.left.value, i.right.value)
                        ])
                    elif node.type == "WORD":
                        # word a=1
                        if not (0 <= i.right.value <= 65535):
                            raise SyntaxError("Out of bounds: %s %d" % (i.left.value, i.right.value))
                        yield CompiledNode(type=node.type, ident=i.left.value, src=[
                            "%s: .word %d" % (i.left.value, i.right.value)
                        ])
                    else:
                        raise SyntaxError("Unknown data type %s" % node.type)
                elif i.right.type == "ARRAY":
                    if node.type == "BYTE":
                        # byte a=[1,2]
                        values = [x.value for x in i.right.value]
                        if not all([0 <= v <= 255 for v in values]):
                            raise SyntaxError("Out of bounds: %s" % (i.left.value))
                        yield CompiledNode(type=node.type, ident=i.left.value, size=len(values), 
                            index_type="BYTE" if len(values) <= 256 else "WORD", 
                            src=byte_data_lines(i.left.value, values),
                        )
                    elif node.type == "WORD":
                        # word a=[1,2]
                        values = [x.value for x in i.right.value]
                        if not all([0 <= v <= 65535 for v in values]):
                            raise SyntaxError("Out of bounds: %s" % (i.left.value))
                        yield CompiledNode(type=node.type, ident=i.left.value, size=len(values), 
                            index_type="BYTE" if len(values) <= 256 else "WORD",
                            src=
                                byte_data_lines("%s_lo" % i.left.value, map(lambda x: (x & 0xff), values)) + 
                                byte_data_lines("%s_hi" % i.left.value, map(lambda x: (x >> 8) & 0xff, values))
                        )
                    else:
                        raise SyntaxError("Unknown data type %s" % node.type)
                else:
                    raise SyntaxError("Syntax error in definition")

    def compile_expr(self, node, out_type):
        if node.type == "NUMERIC":
            if out_type == "ANY":
                out_type = "WORD" if node.value > 255 else "BYTE"

            if out_type == "BYTE":
                # 1
                return CompiledNode(type=out_type, src=[
                    "    // byte literal %d to stack" % node.value,
                    "    lda #%d" % (node.value & 0xff),
                    "    pha",
                ])
            elif out_type == "WORD":
                # 256
                return CompiledNode(type=out_type, src=[
                    "    // word literal %d to stack" % node.value,
                    "    lda #%d" % ((node.value >> 8) & 0xff),
                    "    pha",
                    "    lda #%d" % (node.value & 0xff),
                    "    pha",
                ])
            else:
                raise SyntaxError("Invalid type: %s" + out_type)

        elif node.type == "IDENT":
            inode = self.variables[node.value]
            if out_type == "ANY":
                out_type = inode.type

            if out_type == "BYTE":
                # color
                return CompiledNode(type=out_type, src=[
                    "    // push byte value of %s to stack" % inode.ident,
                    "    lda %s" % inode.ident,
                    "    pha",
                ])
            elif out_type == "WORD":
                # addr
                if inode.type == "BYTE":
                    return CompiledNode(type=out_type, src=[
                        "    // push word value of %s to stack" % inode.ident,
                        "    lda #0",
                        "    pha",
                        "    lda %s" % inode.ident,
                        "    pha",
                    ])
                elif inode.type == "WORD":
                    return CompiledNode(type=out_type, src=[
                        "    // cword to stack",
                        "    lda %s+1" % inode.ident,
                        "    pha",
                        "    lda %s" % inode.ident,
                        "    pha",
                    ])
            else:
                raise SyntaxError("Invalid type: %s" + out_type)

        elif node.type == "SUBSCRIPTION":
            if node.left.type != "IDENT":
                raise SyntaxError("Cannot index: %s" % node.left.type)

            cvar = self.variables[node.left.value]
            right = self.compile_expr(node.right, cvar.index_type)

            if cvar.type == "BYTE" and cvar.index_type == "BYTE":
                if out_type == "BYTE":
                    return CompiledNode(type=out_type, src=
                        right.src +
                        [
                            "    pla",
                            "    tay",
                            "    lda %s,y" % cvar.ident,
                            "    pha"
                        ]
                    )
                elif out_type == "WORD":
                    return CompiledNode(type=out_type, src=
                        right.src +
                        [
                            "    pla",
                            "    tay",
                            "    lda #0",
                            "    pha",
                            "    lda %s,y" % cvar.ident,
                            "    pha"
                        ]
                    )
            if cvar.type == "BYTE" and cvar.index_type == "WORD":
                if out_type == "BYTE":
                    return CompiledNode(type=out_type, src=
                        right.src +
                        [
                            "    pla",
                            "    clc",
                            "    adc #<%s_lo" % cvar.ident,
                            "    sta ZP_W0",

                            "    pla",
                            "    adc #>%s_lo" % cvar.ident,
                            "    sta ZP_W0+1",

                            "    ldy #0",
                            "    lda (ZP_W0),y",
                            "    pha",
                        ]
                    )
                elif out_type == "WORD":
                    return CompiledNode(type=out_type, src=
                        right.src +
                        [
                            "    pla",
                            "    clc",
                            "    adc #<%s_lo" % cvar.ident,
                            "    sta ZP_W0",

                            "    pla",
                            "    adc #>%s_lo" % cvar.ident,
                            "    sta ZP_W0+1",

                            "    ldy #0",
                            "    lda (ZP_W0),y",
                            "    pha",
                        ]
                    )
            if cvar.type == "WORD" and cvar.index_type == "BYTE":
                if out_type == "BYTE":
                    return CompiledNode(type=out_type, src=
                        right.src +
                        [
                            "    pla",
                            "    tay",

                            "    ldy %s_lo,y" % cvar.ident,
                            "    pha",
                        ]
                    )
                elif out_type == "WORD":
                    return CompiledNode(type=out_type, src=
                        right.src +
                        [
                            "    pla",
                            "    tay",

                            "    lda %s_hi,y" % cvar.ident,
                            "    pha",

                            "    lda %s_lo,y" % cvar.ident,
                            "    pha",
                        ]
                    )
            if cvar.type == "WORD" and cvar.index_type == "WORD":
                if out_type == "BYTE":
                    return CompiledNode(type=out_type, src=
                        right.src +
                        [
                            "    pla",
                            "    clc",
                            "    adc #<%s_lo" % cvar.ident,
                            "    sta ZP_W0",

                            "    pla",
                            "    adc #>%s_lo" % cvar.ident,
                            "    sta ZP_W0+1",

                            "    ldy #0",
                            "    lda (ZP_W0),y",
                            "    pha",
                        ]
                    )
                elif out_type == "WORD":
                    return CompiledNode(type=out_type, src=
                        right.src +
                        [
                            "    pla",
                            "    clc",
                            "    adc #<%s_lo" % cvar.ident,
                            "    sta ZP_W0",

                            "    pla",
                            "    adc #>%s_lo" % cvar.ident,
                            "    sta ZP_W0+1",

                            "    ldy #0",
                            "    lda (ZP_W0),y",
                            "    tax",

                            "    lda #%d" % ((cvar.size >> 8) & 0xff),
                            "    clc",
                            "    adc ZP_W0+1",
                            "    sta ZP_W0+1",
                            "    ldy #%d" % (cvar.size & 0xff),
                            "    lda (ZP_W0),y",
                            "    pha",
                            "    txa",
                            "    pha",
                        ]
                    )

        elif node.type == "AND_LIST":
            if len(node.value) == 1:
                return self.compile_expr(node.value[0], out_type)

        elif node.type == "<":
            _type = self.eval_type(node)
            left = self.compile_expr(node.left, _type)
            right = self.compile_expr(node.right, _type)
            if _type == "BYTE":
                return CompiledNode(type=_type, src=
                    [   "    // eval left side of <"] +
                    left.src + 
                    [   "    // eval right side of <"] +
                    right.src +
                    [
                        "less_than: {",
                        "    // byte l < r",
                        "    pla",
                        "    sta ZP_W0",
                        "    pla",
                        "    cmp ZP_W0",
                        "    bcc ret_true",
                        "    lda #0",
                        "    jmp exit",
                        "ret_true:",
                        "    lda #1",
                        "exit:",
                        "    pha",
                        "}"
                    ]
                )



        elif node.type == "+":
            left = self.compile_expr(node.left, out_type)
            right = self.compile_expr(node.right, out_type)
            if out_type == "BYTE":
                return CompiledNode(type=out_type, src=
                    [   "    // eval left side of +"] +
                    left.src + 
                    [   "    // eval right side of +"] +
                    right.src +
                    [
                        "    // byte l + r",
                        "    pla",
                        "    sta ZP_W0",
                        "    pla",
                        "    clc",
                        "    adc ZP_W0",
                        "    pha",
                    ])
            elif out_type == "WORD":
                return CompiledNode(type=out_type, src=
                    [   "    // eval left side of +"] +
                    left.src + 
                    [   "    // eval right side of +"] +
                    right.src +
                    [
                        "    // word l + r",
                        "    pla",
                        "    sta ZP_W0",
                        "    pla",
                        "    sta ZP_W0+1",

                        "    clc",
                        "    pla",
                        "    adc ZP_W0",
                        "    tax",
                        "    pla",
                        "    adc ZP_W0+1",
                        "    pha",
                        "    txa",
                        "    pha",
                    ])
            else:
                raise SyntaxError("Invalid type: %s" + out_type)

        else:
            raise SyntaxError("Unknown code node: %s" % node.type)        

    def compile_code(self, node):
        if node.type == "POKE":
            if node.addr.type == "NUMERIC":
                return CompiledNode(type="POKE", src=
                    self.compile_expr(node.value, "BYTE").src +
                    [
                        "    // POKE literal, expr",
                        "    pla",
                        "    sta %s" % node.addr.value,
                    ]
                )
            else:
                return CompiledNode(type="POKE", src=
                    self.compile_expr(node.value, "BYTE") +
                    self.compile_expr(node.addr, "WORD") +
                    [
                        "    // POKE expr, expr",
                        "    pla",
                        "    sta ZP_W0",
                        "    pla",
                        "    sta ZP_W0+1",
                        "    ldx #0",
                        "    pla",
                        "    sta (ZP_W0,x)",
                    ]
                )
        elif node.type == "START":
            return CompiledNode(type="WHILE", src=
                [
                    ".const ZP_W0 = $fb",
                    "*=2048",
                    ".byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061",
                    "*=2061",
                ]
            )
           
        elif node.type == "WHILE":
            suite_code = self.compile_code(node.suite)
            if node.expr_type == "BYTE":
                return CompiledNode(type="WHILE", src=
                    [
                        "while: {",
                        "expr:",
                    ] +
                    self.compile_expr(node.expr, node.expr_type).src +
                    [
                        "    // WHILE",
                        "    pla",
                        "    bne suite",
                        "    jmp exit",
                        "suite:",
                    ] +
                    self.compile_code(node.suite).src +
                    [
                        "    jmp expr",
                        "exit:",
                        "}",
                    ]
                )
            elif node.expr_type == "WORD":
                return CompiledNode(type="WHILE", src=
                    [
                        "while: {",
                        "expr:",
                    ] +
                    self.compile_expr(node.expr, node.expr_type) +
                    [
                        "    // WHILE",
                        "    pla",
                        "    bne suite",
                        "    pla",
                        "    bne suite",
                        "    jmp exit",
                        "suite:",
                    ] +
                    self.compile_code(node.suite) +
                    [
                        "    jmp expr"
                        "exit:",
                        "}",
                    ]
                )

        elif node.type == "LIST":
            nodes = []
            for n in node:
                nodes += self.compile_code(n).src
            return CompiledNode(type="LIST", src=
                nodes
            )

        elif node.type == "=":
            # a,b = 1,2 (calculate right side)
            nodes = []
            for lnode, rnode in zip(node.left, node.right):
                ltype = self.assignment_type(lnode)
                nodes.append(self.compile_expr(rnode, ltype))

            for lnode in node.left[::-1]:
                if lnode.type == "IDENT":
                    cvar = self.variables[lnode.value]
                    if cvar.type == "BYTE":
                        nodes.append(CompiledNode(type=cvar.type, src=[
                            "    // assign to byte ident",
                            "    pla",
                            "    sta %s" % cvar.ident,
                        ]))
                    elif cvar.type == "WORD":
                        nodes.append(CompiledNode(type=cvar.type, src=[
                            "    // assign to word ident",
                            "    pla",
                            "    sta %s" % cvar.ident,
                            "    pla",
                            "    sta %s+1" % cvar.ident,
                        ]))
                    else:
                        raise SyntaxError("Unknown ident type: %s" % ident.type)

                elif lnode.type == "SUBSCRIPTION":
                    cvar = self.variables[lnode.left.value]
                    if cvar.type == "BYTE" and cvar.index_type == "BYTE":
                        nodes.append(self.compile_expr(lnode.right, cvar.index_type))
                        nodes.append(CompiledNode(type=cvar.type, src=[
                            "    // byte[byte]=byte",
                            "    pla",
                            "    tax",
                            "    pla",
                            "    sta %s,x" % cvar.ident,
                        ]))
                    elif cvar.type == "BYTE" and cvar.index_type == "WORD":
                        nodes.append(self.compile_expr(lnode.right, cvar.index_type))
                        nodes.append(CompiledNode(type=cvar.type, src=[
                            "    // byte[word]=byte",
                            "    pla",
                            "    clc",
                            "    adc #<%s" % cvar.ident,
                            "    sta ZP_W0",

                            "    pla",
                            "    adc #>%s" % cvar.ident,
                            "    sta ZP_W0+1",

                            "    pla",
                            "    ldy #0",
                            "    sta (ZP_W0),y"
                        ]))
                    elif cvar.type == "WORD" and cvar.index_type == "BYTE":
                        nodes.append(self.compile_expr(lnode.right, cvar.index_type))
                        nodes.append(CompiledNode(type=cvar.type, src=[
                            "    // word[byte]=word",
                            "    pla",
                            "    tax",
                            "    pla",
                            "    sta %s_lo,x" % cvar.ident,
                            "    pla",
                            "    sta %s_hi,x" % cvar.ident,
                        ]))
                    elif cvar.type == "WORD" and cvar.index_type == "WORD":
                        nodes.append(self.compile_expr(lnode.right, cvar.index_type))
                        nodes.append(CompiledNode(type=cvar.type, src=[
                            "    // word[word]=word",
                            "    pla",
                            "    clc",
                            "    adc #<%s_lo" % cvar.ident,
                            "    sta ZP_W0",

                            "    pla",
                            "    adc #>%s_lo" % cvar.ident,
                            "    sta ZP_W0+1",

                            "    pla",
                            "    ldy #0",
                            "    sta (ZP_W0),y",

                            "    lda #%d" % ((cvar.size >> 8) & 0xff),
                            # "    clc",
                            "    adc ZP_W0+1",
                            "    sta ZP_W0+1",

                            "    pla",
                            "    ldy #%d" % (cvar.size & 0xff),
                            "    sta (ZP_W0),y"
                        ]))
            code = []
            for n in nodes:
                code += n.src
            return CompiledNode(type="=", src=
                code
            )
    
    def compile(self):
        variables = {}
        for node in self.variables:
            for cnode in self.compile_variable(node):
                variables[cnode.ident] = cnode
        self.variables = variables

        code_segment = []
        for node in self.src_code:
            self.compiled_code.append(self.compile_code(node))

    def parse_suite(self):
        if self.token != "INDENT":
            raise SyntaxError("Suite without indent")
        self.advance()
        suite = AstNodeList()
        while self.token != "DEDENT":
            suite.append(self.parse_stmt())
        self.advance()
        return suite        

    def parse_stmt(self):
        if self.token == "POKE":
            self.advance()
            addr = self.expr()
            if self.token != ",":
                raise SyntaxError("No comma in POKE")
            self.advance()
            value = self.expr()
            return AstNode(type="POKE", addr=addr, value=value)

        elif self.token == "WHILE":
            self.advance()

            expr_type = "BYTE"
            if self.token in ["BYTE", "WORD"]:
                expr_type = self.token
                self.advance()

            expr = self.expr()
            if self.token != ":":
                raise SyntaxError("No : after while expression")
            self.advance()
            if self.token == "INDENT":
                return AstNode(type="WHILE", expr_type=expr_type, expr=expr, suite=self.parse_suite())
            else:
                return AstNode(type="WHILE", expr_type=expr_type, expr=expr, suite=self.parse_stmt())

        else:
            return self.assignment()
            #print("Not processed: %s, %s" % (self.token, self.value))
            #self.advance()

    def parse(self):
        while True:
            if self.token == "START":
                self.advance()
                self.src_code.append(AstNode(type="START"))
                continue
            if self.token == "END":
                return

            if self.token in ("BYTE", "WORD", "WORD"):
                token = self.token
                self.advance()
                self.variables.append(AstNode(type=token, value=self.simple_assignment_list()))

            elif self.token == "CONST":
                self.advance()
                if self.token == "IDENT":
                    ident = self.value
                    self.advance()
                    if self.token != "=":
                        raise SyntaxError("Malformed CONST")
                    self.advance()
                    self.constants[ident] = AstNode(type="CONST", ident=ident, value=self.expr())
                else:
                    raise SyntaxError()
            else:
                self.src_code.append(self.parse_stmt())

    def simple_assignment_list(self):
        # a=1,b=2
        nodes = AstNodeList()
        while True:
            nodes.append(self.simple_assignment())

            if self.token != ",":
                return nodes
            self.advance()

    def simple_assignment(self):
        # a=1
        left = self.expr()

        if self.token == "=":
            self.advance()

            right = self.expr()
            left = AstNode(type="=", left=left, right=right)

        return left

    def assignment(self):
        # a,b=1,2
        left = self.expr_list()

        if self.token in [
            "+=", "-=", "*=", "/=", "&=", "|=", "^=", "!=", 
            "<<=", ">>=", ">>>=", ">><=", "<<>="]:
            self.advance()

            right = self.expr_list(len(left))
            left = AstNode(type="=", left=left, right=right)

            return left

        while self.token == "=":
            self.advance()

            right = self.expr_list(len(left))
            left = AstNode(type="=", left=left, right=right)

        return left

    def expr_list(self, length=None):
        nodes = AstNodeList()
        while True:
            nodes.append(self.expr())

            if length is None:
                if self.token != ",":
                    return nodes
            else:
                if length == len(nodes):
                    return nodes
                elif self.token != ",":
                    raise SyntaxError("Wrong number of right side elements")                                        

            self.advance()

    def expr(self):
        return self.comp_or()

    def comp_or(self):
        left = self.comp_and()

        while self.token == "OR":
            self.advance()

            right = self.comp_and()
            left = AstNode(type="OR", left=left, right=right)

        return left

    def comp_and(self):
        left = self.comp_not()

        while self.token == "AND":
            self.advance()

            right = self.comp_not()
            left = AstNode(type="AND", left=left, right=right)

        return left

    def comp_not(self):
        if self.token == "NOT":
            self.advance()
            return AstNode(type="NOT", value=self.expr())

        return self.comp()

    def comp(self):
        nodes = AstNodeList()
        left = self.addsub()

        while self.token in ("==", "<", ">", ">=", "<=", "!=", "<>"):
            op = self.token
            self.advance()

            right = self.addsub()

            nodes.append(AstNode(type=op, left=left, right=right))

            left = right

        if not nodes:
            return left
        else:
            return AstNode(type="AND_LIST", value=nodes)

    def addsub(self):
        left = self.muldiv()

        while self.token in ("+", "-"):
            op = self.token
            self.advance()
            right = self.muldiv()

            left = AstNode(type=op, left=left, right=right)

        return left

    def muldiv(self):
        left = self.unary_minusplus()

        while self.token in ("*", "/"):
            op = self.token
            self.advance()
            right = self.unary_minusplus()

            left = AstNode(type=op, left=left, right=right)

        return left

    def unary_minusplus(self):
        if self.token == "-":
            self.advance()
            return AstNode(type="UMINUS", value=self.expr())
        if self.token == "+":
            self.advance()
            return self.expr()

        return self.exp()

    def exp(self):
        left = self.subscription_or_call()

        while self.token == "**":
            self.advance()
            right = self.exp()
            left = AstNode(type="**", left=left, right=right)

        return left

    def subscription_or_call(self):
        left = self.atom()

        while self.token in ("[", "(", "."):
            op = self.token
            self.advance()

            if op == "[":
                right = self.expr()
                left = AstNode(type="SUBSCRIPTION", left=left, right=right)

                if self.token != "]":
                    raise SyntaxError("invalid syntax")

                self.advance()

            if op == "(":
                if self.token == ")":
                    left = AstNode(type="CALL", ident=left, args=AstNodeList())
                else:
                    left = AstNode(type="CALL", ident=left, args=self.arglist_comma())

                if self.token != ")":
                    raise SyntaxError("invalid syntax")

                self.advance()

            if op == ".":
                if self.token != "NAME":
                    raise SyntaxError("invalid syntax")

                left = AstNode(type="REFERENCE", left=left, right=self.token)
                self.advance()

        return left

    def atom(self):
        if self.token == "(":
            self.advance()
            if self.token == ")":
                raise SyntaxError("Empty parenthesis")
            else:
                node = self.expr()

                if self.token != ")":
                    raise SyntaxError("invalid syntax")

            return node

        if self.token == "[":
            self.advance()
            if self.token == "]":
                node = AstNode(type="ARRAY", value=AstNodeList())
            else:
                node = AstNode(type="ARRAY", value=self.expr_list())

                if self.token != "]":
                    raise SyntaxError("invalid syntax")

            self.advance()
            return node

        if self.token in ("NUMERIC", "STRING", "IDENT"):
            token = self.token
            value = self.value
            self.advance()
            return AstNode(type=token, value=value)

        raise SyntaxError("invalid syntax")


if __name__ == "__main__":
    p = Parser("examples.worm")
    p.parse()
    for k, v in p.constants.items():
        v.optimize(p.constants)

    for c in p.variables:
        c.optimize(p.constants)

    for c in p.src_code:
        c.optimize(p.constants)

    # for k, v in p.constants.items():
    #     print(v.tree())
    # for v in p.variables:
    #     print(v.tree())
    print("===AST CODE===")
    for v in p.src_code:
        print(v.tree())
    
    p.compile()
    print("===COMPILED VARIABLES===")
    for k, v in p.variables.items():
        print(v.tree())

    code = p.block_optimizer()

    print("===SOURCE CODE===")
    with open("test.asm", "w") as f:
        f.write("// code segment\n")
        for line in code:
            # print("%4d %s" % (line.nr, line.src))
            f.write("%s\n" % line.src)

        f.write("// variable segment\n")
        for ident, node in p.variables.items():
            f.write("\n".join(node.src))
