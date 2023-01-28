from dotwiz import DotWiz

from lexer import Lexer, operators_arithmetic, operators_comparison
from nodes import *
from parser import Parser, AstNode, AstList

ZP_W0 = 251

def format_data(name, data):
    rows = ["%s:" % name]
    data = ["0x{:02x}".format(x) for x in data]
    for index in range(0, len(data), 16):
        rows.append("    %s" % ", ".join(data[index:index+16]))
    return rows

class Compiler:
    def __init__(self, shared, ast, local):
        self.shared = shared
        self.local = local
        self.ast = ast

        self.names = {}
        self.code = []

    def compile(self):
        # COMPILE MAIN
        self.names = self.shared.copy()
        self.names.update(self.local)
        for node in self.ast:
            src = self.compile_code(node)
            self.code += src

        # COMPILE FUNCTIONS
        # TODO

        # COMPILE VARIABLES
        # TODO

        return self.code

    def compile_code(self, node):
        if node.type == "POKE":
            node.src = 
                self.compile_expr(node.value) +
                self.compile_expr(node.addr) +
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
        elif node.type == "PASS":
            return CodeNode(type="PASS", src=[])

        elif node.type == "START":
            return CodeNode(type="WHILE", src=
                [
                    ".const ZP_W0 = $fb",
                    "*=2048",
                    ".byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061",
                    "*=2061",
                ]
            )
        
        elif node.type == "END":
            return CodeNode(type="END", src=
                [
                    "// THE END",
                ]
            )
           
        elif node.type == "WHILE":
            return CodeNode(type="WHILE", src=
                [
                    "{",
                    "expr:",
                ] +
                self.compile_expr(node.expr).src +
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

        elif node.type == "LIST":
            nodes = []
            for n in node:
                nodes += self.compile_code(n).src
            return CodeNode(type="LIST", src=
                nodes
            )

        elif node.type == "=":
            # a,b = 1,2 (calculate right side)
            nodes = []
            for rnode in node.right:
                nodes.append(self.compile_expr(rnode))

            for lnode in node.left[::-1]:
                if lnode.type == "IDENT":
                    cvar = self.names[lnode.value]
                    if cvar.type == "BYTE":
                        nodes.append(CodeNode(type=cvar.type, src=[
                            "    // assign to byte ident",
                            "    pla",
                            "    sta %s" % cvar.ident,
                        ]))
                    elif cvar.type == "WORD":
                        nodes.append(CodeNode(type=cvar.type, src=[
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
                        nodes.append(self.compile_expr(lnode.right))
                        nodes.append(CodeNode(type=cvar.type, src=[
                            "    // byte[byte]=byte",
                            "    pla",
                            "    tax",
                            "    pla",
                            "    sta %s,x" % cvar.ident,
                        ]))
                    elif cvar.type == "BYTE" and cvar.index_type == "WORD":
                        nodes.append(self.compile_expr(lnode.right))
                        nodes.append(CodeNode(type=cvar.type, src=[
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
                        nodes.append(self.compile_expr(lnode.right))
                        nodes.append(CodeNode(type=cvar.type, src=[
                            "    // word[byte]=word",
                            "    pla",
                            "    tax",
                            "    pla",
                            "    sta %s_lo,x" % cvar.ident,
                            "    pla",
                            "    sta %s_hi,x" % cvar.ident,
                        ]))
                    elif cvar.type == "WORD" and cvar.index_type == "WORD":
                        nodes.append(self.compile_expr(lnode.right))
                        nodes.append(CodeNode(type=cvar.type, src=[
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
            return CodeNode(type="=", src=
                code
            )

    def compile_variable(self, node):
        for i in node.value:
            if i.type == "IDENT":
                if node.type == "BYTE":
                    # byte a
                    yield CodeNode(type=node.type, ident=i.value, src=[
                        "%s: .byte 0" % i.value
                    ])

                elif node.type == "WORD":
                    # word a
                    yield CodeNode(type=node.type, ident=i.value, src=[
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
                    yield CodeNode(type=node.type, ident=i.left.value, size=i.right.value, 
                        index_type="BYTE" if i.right.value <= 256 else "WORD", 
                        src=format_data(i.left.value, [0]*i.right.value)
                    )
                elif node.type == "WORD":
                    # word[10]
                    yield CodeNode(type=node.type, ident=i.left.value, size=i.right.value, 
                        index_type="BYTE" if i.right.value <= 256 else "WORD",
                        src=
                            format_data("%s_lo" % i.left.value, [0]*i.right.value) +
                            format_data("%s_hi" % i.left.value, [0]*i.right.value)
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
                        yield CodeNode(type=node.type, ident=i.left.value, src=[
                            "%s: .byte %d" % (i.left.value, i.right.value)
                        ])
                    elif node.type == "WORD":
                        # word a=1
                        if not (0 <= i.right.value <= 65535):
                            raise SyntaxError("Out of bounds: %s %d" % (i.left.value, i.right.value))
                        yield CodeNode(type=node.type, ident=i.left.value, src=[
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
                        yield CodeNode(type=node.type, ident=i.left.value, size=len(values), 
                            index_type="BYTE" if len(values) <= 256 else "WORD", 
                            src=format_data(i.left.value, values),
                        )
                    elif node.type == "WORD":
                        # word a=[1,2]
                        values = [x.value for x in i.right.value]
                        if not all([0 <= v <= 65535 for v in values]):
                            raise SyntaxError("Out of bounds: %s" % (i.left.value))
                        yield CodeNode(type=node.type, ident=i.left.value, size=len(values), 
                            index_type="BYTE" if len(values) <= 256 else "WORD",
                            src=
                                format_data("%s_lo" % i.left.value, map(lambda x: (x & 0xff), values)) + 
                                format_data("%s_hi" % i.left.value, map(lambda x: (x >> 8) & 0xff, values))
                        )
                    else:
                        raise SyntaxError("Unknown data type %s" % node.type)
                else:
                    raise SyntaxError("Syntax error in definition")

    def compile_expr(self, node):
        if node.type == "NUMERIC":
            if node.return_type == "BYTE":
                return CodeNode(type=node.return_type, src=[
                    "    // byte literal %d to stack" % node.value,
                    "    lda #%d" % (node.value & 0xff),
                    "    pha",
                ])
            elif node.return_type == "WORD":
                # 256
                return CodeNode(type=out_type, src=[
                    "    // word literal %d to stack" % node.value,
                    "    lda #%d" % ((node.value >> 8) & 0xff),
                    "    pha",
                    "    lda #%d" % (node.value & 0xff),
                    "    pha",
                ])
            else:
                raise SyntaxError("Invalid type: %s" + out_type)

        elif node.type == "IDENT":
            inode = self.names[node.value]

            if node.return_type == "BYTE":
                # color
                return CodeNode(type=node.return_type, src=[
                    "    // push byte value of %s to stack" % inode.name,
                    "    lda %s" % inode.name,
                    "    pha",
                ])
            elif node.return_type == "WORD":
                return CodeNode(type=out_type, src=[
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
            right = self.compile_expr(node.right)

            if cvar.type == "BYTE" and cvar.index_type == "BYTE":
                return CodeNode(type=cvar.type, src=
                    right.src +
                    [
                        "    pla",
                        "    tay",
                        "    lda %s,y" % cvar.ident,
                        "    pha"
                    ]
                )
            if cvar.type == "BYTE" and cvar.index_type == "WORD":
                return CodeNode(type=out_type, src=
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
                return CodeNode(type=out_type, src=
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
                return CodeNode(type=out_type, src=
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
                return self.compile_expr(node.value[0])
            else:
                raise NotImplemented()

        elif node.type == "<":
            left = self.compile_expr(node.left)
            right = self.compile_expr(node.right)

            if left.return_type == "BYTE":
                return CodeNode(type=_type, src=
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
            else:
                raise NotImplemented()

        elif node.type == "+":
            left = self.compile_expr(node.left)
            right = self.compile_expr(node.right)
            if node.left.return_type == "BYTE":
                return CodeNode(type=node.type, src=
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
            elif node.left.return_type == "WORD":
                return CodeNode(type=out_type, src=
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
                raise NotImplemented()

        else:
            raise NotImplemented()      

