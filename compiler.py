from dotwiz import DotWiz

from lexer import Lexer, operators_arithmetic, operators_comparison
from nodes import *
from parser import Parser, AstNode, AstList

ZP_W0 = 251

def format_data(name, data):
    rows = ["%s:" % name]
    data = ["0x{:02x}".format(x).replace("0x", "$") for x in data]
    for index in range(0, len(data), 16):
        rows.append("    .byte %s" % ", ".join(data[index:index+16]))
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
            a=self.compile_code(node)
            self.code += a

        # COMPILE FUNCTIONS
        for name, node in self.shared.items():
            if node.type == "DEF_FUN":
                self.names = self.shared.copy()
                self.names.update(node.local)
                self.code += self.compile_function(node)

        # COMPILE VARIABLES
        for name, node in self.shared.items():
            if node.type == "DEF_VAR":
                self.code += self.compile_variable(node)

        for name, node in self.local.items():
            self.code += self.compile_variable(node)

        return self.code

    def compile_function(self, ast):
        code = [ 
            "%s: {" % ast.name,
            "// CODE" 
        ]
        
        for node in ast.body:
            code += self.compile_code(node)

        code += [ 
            "    rts",
            "// VARIABLES"
        ]

        for name, node in ast.local.items():
            code += self.compile_variable(node)
        
        code += [ "}" ]

        return code

    def compile_code(self, node):
        if node.type == "LIST":
            nodes = []
            for n in node:
                nodes += self.compile_code(n)
            return nodes

        elif node.type == "CBYTE":
            if node.value.return_type == "WORD":
                return (
                    self.compile_expr(node.value) +
                    [
                        "    pla",
                        "    tax",
                        "    pla",
                        "    txa",
                        "    pha",
                    ]
                )
            else:
                raise NotImplementedError()
        
        elif node.type == "CWORD":
            if node.value.return_type == "BYTE":
                return (
                    self.compile_expr(node.value) +
                    [
                        "    pla",
                        "    tax",
                        "    lda #0",
                        "    pha",
                        "    txa",
                        "    pha",
                    ]
                )
            else:
                raise NotImplementedError()
        
        elif node.type == "CALL":
            code = []
            for n in range(len(node.args)):
                arg = node.args[n]
                arg_def = self.shared[node.name].args[n]
                code += self.compile_expr(arg)
                if arg.return_type == "BYTE":
                    code += [
                        "    pla",
                        "    sta %s.%s" % (node.name, arg_def.name),
                    ]
                elif arg.return_type == "WORD":
                    code += [
                        "    pla",
                        "    sta %s.%s" % (node.name, arg_def.name),
                        "    pla",
                        "    sta %s.%s+1" % (node.name, arg_def.name),
                    ]
                else:
                    raise NotImplementedError()

            code += [
                "    jsr %s" % node.name
            ]

            return code

        elif node.type == "RETURN":
            if self.names["_RETURN_"].return_type == "BYTE":
                return (
                    self.compile_expr(node.value) +
                    [
                        "    pla",
                        "    sta _RETURN_",
                        "    rts"
                    ]
                )
            elif self.names["_RETURN_"].return_type == "WORD":
                return (
                    self.compile_expr(node.value) +
                    [
                        "    pla",
                        "    sta _RETURN_",
                        "    pla",
                        "    sta _RETURN_+1",
                        "    rts"
                    ]
                )
            else:
                raise NotImplementedError()
                

        elif node.type == "POKE":
            return (
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
            return []

        elif node.type == "START":
            return (
                [
                    ".const ZP_W0 = $fb",
                    "*=2048",
                    ".byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061",
                    "*=2061",
                ]
            )
        
        elif node.type == "END":
            return (
                [
                    "    rts",
                    "// MAIN END",
                    "",
                ]
            )
           
        elif node.type == "WHILE":
            return (
                [
                    "{",
                    "expr:",
                ] +
                self.compile_expr(node.expr) +
                [
                    "    // WHILE",
                    "    pla",
                    "    bne suite",
                    "    jmp exit",
                    "suite:",
                ] +
                self.compile_code(node.suite) +
                [
                    "    jmp expr",
                    "exit:",
                    "}",
                ]
            )

        elif node.type == "=":
            # a,b = 1,2 (calculate right side)
            nodes = []
            for rnode in node.right:
                nodes += self.compile_expr(rnode)

            for lnode in node.left[::-1]:
                if lnode.type == "IDENT":
                    cvar = self.names[lnode.value]
                    if cvar.return_type == "BYTE":
                        nodes += [
                            "    // assign to byte ident",
                            "    pla",
                            "    sta %s" % cvar.name,
                        ]
                    elif cvar.return_type == "WORD":
                        nodes += [
                            "    // assign to word ident",
                            "    pla",
                            "    sta %s" % cvar.ident,
                            "    pla",
                            "    sta %s+1" % cvar.ident,
                        ]
                    else:
                        raise SyntaxError("Unknown ident type: %s" % cvar.type)

                elif lnode.type == "SUBSCRIPTION":
                    cvar = self.names[lnode.left.value]
                    if cvar.return_type == "BYTE" and cvar.index_type == "BYTE":
                        nodes += self.compile_expr(lnode.right)
                        nodes += [
                            "    // byte[byte]=byte",
                            "    pla",
                            "    tax",
                            "    pla",
                            "    sta %s,x" % cvar.name,
                        ]
                    elif cvar.return_type == "BYTE" and cvar.index_type == "WORD":
                        nodes += self.compile_expr(lnode.right)
                        nodes += [
                            "    // byte[word]=byte",
                            "    pla",
                            "    clc",
                            "    adc #<%s" % cvar.name,
                            "    sta ZP_W0",

                            "    pla",
                            "    adc #>%s" % cvar.name,
                            "    sta ZP_W0+1",

                            "    pla",
                            "    ldy #0",
                            "    sta (ZP_W0),y"
                        ]
                    elif cvar.return_type == "WORD" and cvar.index_type == "BYTE":
                        nodes += self.compile_expr(lnode.right)
                        nodes += [
                            "    // word[byte]=word",
                            "    pla",
                            "    tax",
                            "    pla",
                            "    sta %s_lo,x" % cvar.name,
                            "    pla",
                            "    sta %s_hi,x" % cvar.name,
                        ]
                    elif cvar.type == "WORD" and cvar.index_type == "WORD":
                        nodes += self.compile_expr(lnode.right)
                        nodes += [
                            "    // word[word]=word",
                            "    pla",
                            "    clc",
                            "    adc #<%s_lo" % cvar.name,
                            "    sta ZP_W0",

                            "    pla",
                            "    adc #>%s_lo" % cvar.name,
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
                        ]
            return nodes

    def compile_variable(self, node):
        if node.return_type == "BYTE":
            # byte a
            return list(format_data(node.name, node.initializer))
        elif node.type == "WORD":
            # word a
            return (
                ["%s:" % node.name] +
                format_data("%s_lo" % node.name, map(lambda x: (x & 0xff), node.initializer)) +
                format_data("%s_hi" % node.name, map(lambda x: (x >> 8) & 0xff, node.initializer))
            )
        else:
            raise NotImplementedError()

    def compile_expr(self, node):
        if node.type == "NUMERIC":
            if node.return_type == "BYTE":
                return [
                    "    // byte literal %d to stack" % node.value,
                    "    lda #%d" % (node.value & 0xff),
                    "    pha",
                ]
            elif node.return_type == "WORD":
                # 256
                return [
                    "    // word literal %d to stack" % node.value,
                    "    lda #%d" % ((node.value >> 8) & 0xff),
                    "    pha",
                    "    lda #%d" % (node.value & 0xff),
                    "    pha",
                ]
            else:
                raise SyntaxError("Invalid type: %s" + out_type)

        elif node.type == "IDENT":
            inode = self.names[node.value]

            if inode.return_type == "BYTE":
                # color
                return [
                    "    // push byte value of %s to stack" % inode.name,
                    "    lda %s" % inode.name,
                    "    pha",
                ]
            elif inode.return_type == "WORD":
                return [
                    "    // cword to stack",
                    "    lda %s+1" % inode.name,
                    "    pha",
                    "    lda %s" % inode.name,
                    "    pha",
                ]
            else:
                raise SyntaxError("Invalid type: %s" + out_type)

        elif node.type == "CALL":
            code = []
            for n in range(len(node.args)):
                arg = node.args[n]
                arg_def = self.shared[node.name].args[n]
                code += self.compile_expr(arg)
                if arg.return_type == "BYTE":
                    code += [
                        "    pla",
                        "    sta %s.%s" % (node.name, arg_def.name),
                    ]
                elif arg.return_type == "WORD":
                    code += [
                        "    pla",
                        "    sta %s.%s" % (node.name, arg_def.name),
                        "    pla",
                        "    sta %s.%s+1" % (node.name, arg_def.name),
                    ]
                else:
                    raise NotImplementedError()

            code += [
                "    jsr %s" % node.name
            ]

            if node.return_type == "BYTE":
                code += [
                    "    lda %s._RETURN_" % node.name,
                    "    pha"
                ]
            elif node.return_type == "WORD":
                code += [
                    "    lda %s._RETURN_+1" % node.name,
                    "    pha"
                    "    lda %s._RETURN_" % node.name,
                    "    pha"
                ]
            else:
                raise NotImplementedError()

            return code

        elif node.type == "SUBSCRIPTION":
            if node.left.type != "IDENT":
                raise SyntaxError("Cannot index: %s" % node.left.type)

            cvar = self.names[node.left.value]
            right = self.compile_expr(node.right)

            if cvar.type == "BYTE" and cvar.index_type == "BYTE":
                return (
                    right +
                    [
                        "    pla",
                        "    tay",
                        "    lda %s,y" % cvar.name,
                        "    pha"
                    ]
                )
            if cvar.type == "BYTE" and cvar.index_type == "WORD":
                return (
                    right +
                    [
                        "    pla",
                        "    clc",
                        "    adc #<%s_lo" % cvar.name,
                        "    sta ZP_W0",

                        "    pla",
                        "    adc #>%s_lo" % cvar.name,
                        "    sta ZP_W0+1",

                        "    ldy #0",
                        "    lda (ZP_W0),y",
                        "    pha",
                    ]
                )
            if cvar.type == "WORD" and cvar.index_type == "BYTE":
                return (
                    right +
                    [
                        "    pla",
                        "    tay",

                        "    lda %s_hi,y" % cvar.name,
                        "    pha",

                        "    lda %s_lo,y" % cvar.name,
                        "    pha",
                    ]
                )
            if cvar.type == "WORD" and cvar.index_type == "WORD":
                return (
                    right +
                    [
                        "    pla",
                        "    clc",
                        "    adc #<%s_lo" % cvar.name,
                        "    sta ZP_W0",

                        "    pla",
                        "    adc #>%s_lo" % cvar.name,
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

            if node.left.return_type == "BYTE":
                return (
                    [   "    // eval left side of <"] +
                    left + 
                    [   "    // eval right side of <"] +
                    right +
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
                return (
                    [   "    // eval left side of +"] +
                    left + 
                    [   "    // eval right side of +"] +
                    right +
                    [
                        "    // byte l + r",
                        "    pla",
                        "    sta ZP_W0",
                        "    pla",
                        "    clc",
                        "    adc ZP_W0",
                        "    pha",
                    ]
                )
            elif node.left.return_type == "WORD":
                return (
                    [   "    // eval left side of +"] +
                    left + 
                    [   "    // eval right side of +"] +
                    right +
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

