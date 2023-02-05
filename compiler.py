from dotwiz import DotWiz

from lexer import Lexer, operators_arithmetic, operators_comparison
from nodes import *
from parser import Parser, AstNode, AstList

ZP_W0 = 251

def format_data(name, data):
    if type(data) == int:
        data = [data]
    rows = ["%s:" % name]
    data = ["0x{:02x}".format(x).replace("0x", "$") for x in data]
    for index in range(0, len(data), 16):
        rows.append("    .byte %s" % ", ".join(data[index:index+16]))
    return rows

class Compiler:
    def __init__(self, shared, literals, ast, local):
        self.shared = shared
        self.literals = literals
        self.local = local
        self.ast = ast

        self.names = {}
        self.code = []

        self.library = {}

    def load(self, name):
        if name not in self.library:
            self.library[name] = LIBRARY[name]
            for line in LIBRARY[name]:
                token = line.split()
                if len(token) >= 2:
                    if token[0] == "jsr":
                        if token[1].startswith("LIB_"):
                            self.load(token[1])

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

        # ADD MACROS
        self.code += ["// MACROS"]
        self.code += MACRO

        # ADD LIBRARIES
        self.code += ["// LIBRARIES"]
        for line in self.code:
            token = line.split()
            if len(token) >= 2:
                if token[0] == "jsr":
                    if token[1].startswith("LIB_"):
                        self.load(token[1])
        for lib in self.library.values():
            self.code += lib

        # COMPILE LITERALS
        self.code += ["// LITERALS"]
        self.code += [".encoding \"petscii_mixed\""]

        for node in self.literals.values():
            self.code += self.compile_literal(node)

        # COMPILE VARIABLES
        self.code += [""]
        self.code += ["// SHARED"]
        for name, node in self.shared.items():
            if node.type == "DEF_VAR":
                self.code += self.compile_variable(node)

        self.code += [""]
        self.code += ["// MAIN LOCALS"]
        for name, node in self.local.items():
            self.code += self.compile_variable(node)

        return self.code

    def compile_function(self, func):
        code = [
            "%s: {" % func.name,
            "// CODE"
        ]

        for node in func.ast:
            code += self.compile_code(node)

        code += [
            "    rts",
            "// VARIABLES"
        ]

        for node in func.local.values():
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
            def_args = self.shared[node.name].args
            if len(def_args) != len(node.args):
                raise SyntaxError("%s expects %d arguments, got %d" % (node.name, len(def_args), len(node.args)))

            code = []
            for call_arg, def_arg in zip(node.args, def_args):
                code += self.compile_expr(call_arg)
                if call_arg.return_type == "BYTE":
                    code += [
                        "    pla",
                        "    sta %s.%s" % (node.name, def_arg.name),
                    ]
                elif call_arg.return_type == "WORD":
                    code += [
                        "    pla",
                        "    sta %s.%s" % (node.name, def_arg.name),
                        "    pla",
                        "    sta %s.%s+1" % (node.name, def_arg.name),
                    ]
                elif call_arg.return_type == "STRING":
                    code += [
                        "    lda #%d" % def_arg.capacity,
                        "    sta ZP_B0",
                        "    LOAD(%s.%s, ZP_W0)" % (node.name, def_arg.name),
                        "    jsr LIB_STRING_PULL",
                    ]
                else:
                    raise NotImplementedError()

            code += [
                "    jsr %s" % node.name
            ]

            return code

        elif node.type == "RETURN":
            return (
                self.compile_expr(node.value) +
                [
                    "    rts"
                ]
            )
            # vdef = self.names["_RETURN_"]
            # if vdef.return_type == "BYTE":
            #     return (
            #         self.compile_expr(node.value) +
            #         [
            #             "    pla",
            #             "    sta _RETURN_",
            #             "    rts"
            #         ]
            #     )
            # elif vdef.return_type == "WORD":
            #     return (
            #         self.compile_expr(node.value) +
            #         [
            #             "    pla",
            #             "    sta _RETURN_",
            #             "    pla",
            #             "    sta _RETURN_+1",
            #             "    rts"
            #         ]
            #     )
            # elif vdef.return_type == "STRING":
            #     self.library["LIB_STRING_PULL"] = LIBRARY["LIB_STRING_PULL"]

            #     return (
            #         self.compile_expr(node.value) +
            #         [
            #             "    lda #%d" % vdef.capacity,
            #             "    sta ZP_B0",
            #             "    LOAD(_RETURN_, ZP_W0)",
            #             "    jsr LIB_STRING_PULL",
            #         ]
            #     )
            # else:
            #     raise NotImplementedError()

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
                    "    ldy #0",
                    "    pla",
                    "    sta (ZP_W0),y",
                ]
            )

        elif node.type == "DEBUG":
            return (
                self.compile_expr(node.value) +
                [
                    "    // DEBUG",
                    "    jsr LIB_DEBUG",
                ]
            )
        elif node.type == "PASS":
            return []

        elif node.type == "PEEK":
            code = [ "// BYTE %s" % node.type ]
            code += self.compile_expr(node.addr)
            code += [
                "    // BYTE PEEK",
                "    pla",
                "    sta ZP_W0",
                "    pla",
                "    sta ZP_W0+1",
                "    ldy #0",
                "    lda (ZP_W0),y",
                "    pha",
            ]

        elif node.type == "PASS":
            return []

        elif node.type == "START":
            return (
                [
                    "// CONSTANTS",
                    ".const ZP_DW = $02",
                    ".const ZP_W0 = $06",
                    ".const ZP_W1 = $08",
                    ".const ZP_B0 = $10",
                    ".const ZP_B1 = $11",
                    ".const STACK = $12",
                    ".const KERNEL_CHROUT = $ffd2",
                    "",
                    "// PREAMBLE",
                    "*=2048",
                    ".byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061",
                    "*=2061",
                    "    sei",
                    "    dec 1",
                    "    cli",
                    "    LOAD($cfff, STACK)",
                    "",
                    "    // PROGRAM CODE",
                    "",
                ]
            )

        elif node.type == "END":
            return (
                [
                    "",
                    "    // POSTAMBLE",
                    "    sei",
                    "    inc 1",
                    "    cli",
                    "    rts",
                    "// END",
                    "",
                ]
            )

        elif node.type == "WHILE":
            code = []
            code += [
                "{ // WHILE",
                "expr:",
            ]

            if node.expr.type == "NUMERIC" and node.expr.value == 1:
                code += [ "suite:" ]
            else:
                code += self.compile_expr(node.expr)
                code += [
                    "    // WHILE",
                    "    pla",
                    "    bne suite",
                    "    jmp exit",
                    "suite:",
                ]

            code += self.compile_code(node.suite)
            code += [
                "    jmp expr",
                "exit:",
                "}",
            ]

            return code

        elif node.type == "IF":
            code = []
            code += [
                "{ // IF",
            ]

            for index, branch in enumerate(node.branches):
                code += [
                    "!expr:",
                ]
                code += self.compile_expr(branch.expr)
                code += [
                    "    pla",
                    "    beq !expr+",
                    "    jmp suite%d" % index,
                    ""
                ]

            code += [
                "// ELSE",
                "!expr:"
            ]

            if "_else" in node:
                code += self.compile_code(node._else.body)

            code += [
                "    jmp exit"
            ]

            for index, branch in enumerate(node.branches):
                code += [
                    "suite%d:" % index,
                ]
                code += self.compile_code(branch.body)
                code += [
                    "    jmp exit",
                    ""
                ]

            code += [
                "exit:",
                "}"
            ]

            return code

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
                            "    sta %s" % cvar.name,
                            "    pla",
                            "    sta %s+1" % cvar.name,
                        ]
                    elif cvar.return_type == "STRING":
                        nodes += [
                            "    // assign string",
                            "    lda #%d" % cvar.capacity,
                            "    sta ZP_B0",
                            "    LOAD(%s, ZP_W0)" % cvar.name,
                            "    jsr LIB_STRING_PULL",
                        ]
                    else:
                        raise NotImplementedError()

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

    def compile_literal(self, node):
        assert node.type == "LITERAL"

        return (
            ["%s:" % node.md5]+
            ["    .byte %d" % len(node.value)] +
            ["    .text \"%s\"" % node.value]
        )

    def compile_variable(self, node):
        assert node.type == "DEF_VAR"

        if node.return_type == "BYTE":
            # byte a
            return list(format_data(node.name, node.initializer))
        elif node.return_type == "WORD":
            # word a
            return (
                ["%s:" % node.name] +
                format_data("%s_lo" % node.name, map(lambda x: (x & 0xff), node.initializer)) +
                format_data("%s_hi" % node.name, map(lambda x: (x >> 8) & 0xff, node.initializer))
            )
        elif node.return_type == "STRING":
            result = ["%s:" % node.name]
            for element in node.initializer:
                result += ["    .byte %d" % element[0]]
                result += ["    .text \"%s\"" % element[1]]
            return result
        else:
            raise NotImplementedError()

    def compile_expr(self, node):
        if node.type == "NUMERIC":
            if node.return_type == "BYTE":
                return [
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

        elif node.type == "LITERAL":
            return [
                "    LOAD(%s, ZP_W0)" % node.md5,
                "    jsr LIB_STRING_PUSH",
            ]

        elif node.type == "IDENT":
            ident = self.names[node.value]

            if ident.return_type == "BYTE":
                # color
                return [
                    "    // push byte value of %s to stack" % ident.name,
                    "    lda %s" % ident.name,
                    "    pha",
                ]
            elif ident.return_type == "WORD":
                return [
                    "    // cword to stack",
                    "    lda %s+1" % ident.name,
                    "    pha",
                    "    lda %s" % ident.name,
                    "    pha",
                ]
            elif ident.return_type == "STRING":
                if ident.size == 1:
                    return [
                        "    // string to stack",
                        "    LOAD(%s, ZP_W0)" % ident.name,
                        "    jsr LIB_STRING_PUSH",
                    ]
                else:
                    pass
            else:
                raise NotImplementedError()

        elif node.type == "CSTRING":
            if node.value.return_type == "BYTE":
                return (
                    self.compile_expr(node.value) +
                    [
                        "    pla",
                        "    jsr LIB_CSTRING_BYTE",
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
            def_args = self.shared[node.name].args
            if len(def_args) != len(node.args):
                raise SyntaxError("%s expects %d arguments, got %d" % (node.name, len(def_args), len(node.args)))

            code = []
            for call_arg, def_arg in zip(node.args, def_args):
                code += self.compile_expr(call_arg)
                if call_arg.return_type == "BYTE":
                    code += [
                        "    pla",
                        "    sta %s.%s" % (node.name, def_arg.name),
                    ]
                elif call_arg.return_type == "WORD":
                    code += [
                        "    pla",
                        "    sta %s.%s" % (node.name, def_arg.name),
                        "    pla",
                        "    sta %s.%s+1" % (node.name, def_arg.name),
                    ]
                elif call_arg.return_type == "STRING":
                    code += [
                        "    lda #%d" % def_arg.capacity,
                        "    sta ZP_B0",
                        "    LOAD(%s.%s, ZP_W0)" % (node.name, def_arg.name),
                        "    jsr LIB_STRING_PULL",
                    ]
                else:
                    raise NotImplementedError()

            code += [
                "    jsr %s" % node.name
            ]

            # if node.return_type == "BYTE":
            #     code += [
            #         "    lda %s._RETURN_" % node.name,
            #         "    pha"
            #     ]
            # elif node.return_type == "WORD":
            #     code += [
            #         "    lda %s._RETURN_+1" % node.name,
            #         "    pha"
            #         "    lda %s._RETURN_" % node.name,
            #         "    pha"
            #     ]
            # elif node.return_type == "STRING":
            #     self.library["LIB_STRING_PUSH"] = LIBRARY["LIB_STRING_PUSH"]
            #     code += [
            #         "    LOAD(%s._RETURN_, ZP_W0)" % node.name,
            #         "    jsr LIB_STRING_PUSH",
            #     ]
            # else:
            #     raise NotImplementedError()

            return code

        elif node.type == "SUBSCRIPTION":
            if node.left.type == "IDENT":
                cvar = self.names[node.left.value]

                if cvar.return_type == "STRING":
                    if cvar.size == 1:
                        code = [ "    // STRING %s" % node.type ]
                        code += self.compile_expr(node.left)
                        code += self.compile_expr(node.right)
                        code += [
                            "    pla",
                            "    jsr LIB_STRING_SUBSCRIPTION",
                        ]
                        return code
                    else:
                        # STRING ARRAY INDEX IS ALWAYS WORD
                        code = [ "    // STRING[] %s" % node.type ]
                        code += self.compile_expr(node.right)
                        code += [
                            "    pla",
                            "    sta ZP_W0",
                            "    pla",
                            "    sta ZP_W0+1",

                            "    lda #0     // capacity as 16 bit number",
                            "    sta ZP_W1+1",
                            "    lda #%d" % cvar.capacity,
                            "    sta ZP_W1",
                            "    inc ZP_W1  // one byte for size",
                            "    jsr LIB_MUL16",
                            "    clc",
                            "    lda #<%s" % cvar.name,
                            "    adc ZP_DW",
                            "    sta ZP_W0",
                            "    lda #>%s" % cvar.name,
                            "    adc ZP_DW+1",
                            "    sta ZP_W0+1",
                            "    jsr LIB_STRING_PUSH",
                        ]
                        return code

                if cvar.return_type == "BYTE" and cvar.index_type == "BYTE":
                    return (
                        self.compile_expr(node.right) +
                        [
                            "    pla",
                            "    tay",
                            "    lda %s,y" % cvar.name,
                            "    pha"
                        ]
                    )
                if cvar.return_type == "BYTE" and cvar.index_type == "WORD":
                    return (
                        self.compile_expr(node.right) +
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
                if cvar.return_type == "WORD" and cvar.index_type == "BYTE":
                    return (
                        self.compile_expr(node.right) +
                        [
                            "    pla",
                            "    tay",

                            "    lda %s_hi,y" % cvar.name,
                            "    pha",

                            "    lda %s_lo,y" % cvar.name,
                            "    pha",
                        ]
                    )
                if cvar.return_type == "WORD" and cvar.index_type == "WORD":
                    return (
                        self.compile_expr(node.right) +
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
            elif node.right.return_type == "STRING":
                code = [ "    // STRING %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    jsr LIB_STRING_SUBSCRIPTION",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == "AND_LIST":
            if len(node.value) == 1:
                return self.compile_expr(node.value[0])
            else:
                raise NotImplemented()

        elif node.type == "<":
            if node.left.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_W0",
                    "    pla",
                    "    cmp ZP_W0",
                    "    bcc true",
                    "    lda #0",
                    "    jmp exit",
                    "true:",
                    "    lda #1",
                    "exit:",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplemented()

        elif node.type == "AND":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += [
                    "    pla",
                    "    bne right",
                    "    jmp true",
                    "right:",
                ]
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    bne true",
                    "    lda #0",
                    "    jmp exit",
                    "true:",
                    "    lda #1",
                    "exit:",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == "OR":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += [
                    "    pla",
                    "    beq right",
                    "    jmp true",
                    "right:",
                ]
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    bne true",
                    "    lda #0",
                    "    jmp exit",
                    "true:",
                    "    lda #1",
                    "exit:",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == "NOT":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.value)
                code += [
                    "    pla",
                    "    bne true",
                    "    lda #0",
                    "    jmp exit",
                    "true:",
                    "    lda #1"
                    "exit:",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == "+":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    // byte l + r",
                    "    pla",
                    "    sta ZP_W0",
                    "    pla",
                    "    clc",
                    "    adc ZP_W0",
                    "    pha",
                    "}"
                ]
                return code

            elif node.return_type == "WORD":
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

            elif node.return_type == "STRING":
                code = [ "{ // STRING %s" % node.type ]
                code += self.compile_expr(node.right)
                code += self.compile_expr(node.left)
                code += [
                    "    jsr LIB_STRING_MERGE",
                    "}"
                ]
                return code
            else:
                raise NotImplemented()

        elif node.type == "-":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_W0",
                    "    pla",
                    "    sec",
                    "    sbc ZP_W0",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == "*":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_B0",
                    "    pla",
                    "    sta ZP_B1",

                    "    jsr LIB_MUL8",

                    "    lda ZP_W0",
                    "    pha",
                    "}",
                ]
                return code
            if node.return_type == "WORD":
                code = [ "{ // WORD %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_W0",
                    "    pla",
                    "    sta ZP_W0+1",
                    "    pla",
                    "    sta ZP_W1",
                    "    pla",
                    "    sta ZP_W1+1",

                    "    jsr LIB_MUL16",

                    "    lda ZP_DW",
                    "    pha",
                    "    lda ZP_DW+1",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == "/":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_W0+1",
                    "    pla",
                    "    sta ZP_W0",

                    "//  ZP_W0 / ZP_W0+1",
                    "    lda #0",
                    "    ldx #8",
                    "    asl ZP_W0",
                    "l1:",
                    "    rol",
                    "    cmp ZP_W0+1",
                    "    bcc l2",
                    "    sbc ZP_W0+1",
                    "l2:",
                    "    rol ZP_W0",
                    "    dex",
                    "    bne l1",
                    "// ZP_W0 quotient, ZP_W0+1 remainder",

                    "    lda ZP_W0"
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == "%":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_W0+1",
                    "    pla",
                    "    sta ZP_W0",

                    "//  ZP_W0 / ZP_W0+1",
                    "    lda #0",
                    "    ldx #8",
                    "    asl ZP_W0",
                    "l1:",
                    "    rol",
                    "    cmp ZP_W0+1",
                    "    bcc l2",
                    "    sbc ZP_W0+1",
                    "l2:",
                    "    rol ZP_W0",
                    "    dex",
                    "    bne l1",
                    "// ZP_W0 quotient, ZP_W0+1 remainder",

                    "    lda ZP_W0+1"
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == "&":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_W0",
                    "    pla",
                    "    and ZP_W0",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == "|":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_W0",
                    "    pla",
                    "    ora ZP_W0",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == "^":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_W0",
                    "    pla",
                    "    eor ZP_W0",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == "~":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    eor #$ff",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == "<<":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    beq exit"
                    "    tax",
                    "    pla",
                    "    cpx #0",
                    "    beq exit",
                    "loop:",
                    "    asl",
                    "    dex",
                    "    bne loop",
                    "exit:",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == ">>":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    tax",
                    "    pla",
                    "    cpx #0",
                    "    beq exit",
                    "loop:",
                    "    lsr",
                    "    dex",
                    "    bne loop",
                    "exit:",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == ">>>":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    tax",
                    "    pla",
                    "    cpx #0",
                    "    beq exit",
                    "loop:",
                    "    cmp #$80",
                    "    ror",
                    "    dex",
                    "    bne loop",
                    "exit:",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == ">><":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    tax",
                    "    pla",
                    "    cpx #0",
                    "    beq exit",
                    "loop:",
                    "    tay",
                    "    ror",
                    "    tya",
                    "    ror",
                    "    dex",
                    "    bne loop",
                    "exit:",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.type == "<<>":
            if node.return_type == "BYTE":
                code = [ "{ // BYTE %s" % node.type ]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    tax",
                    "    pla",
                    "    cpx #0",
                    "    beq exit",
                    "loop:",
                    "    tay",
                    "    rol",
                    "    tya",
                    "    rol",
                    "    dex",
                    "    bne loop",
                    "exit:",
                    "    pha",
                    "}",
                ]
                return code
            else:
                raise NotImplementedError()

        else:
            raise NotImplementedError()

MACRO = \
"""
.macro LOAD(Addr, ZP) {
    lda #<Addr
    sta ZP
    lda #>Addr
    sta ZP+1
}

.macro INC16(Addr) {
    inc Addr
    bne !+
    inc Addr+1
!:
}

.macro ADD16_BYTE(Addr, Value) {
    clc
    lda #Value
    adc Addr
    sta Addr
    bcc exit
    inc Addr+1
exit:
}

.macro SUB16_BYTE(Addr, Value) {
    sec
    lda #Value
    sbc Addr
    sta Addr
    bcs exit
    dec Addr+1
exit:
}

.macro DEC16(Addr) {
    lda Addr
    bne !+
    dec Addr+1
!:
    dec Addr
}
""".split("\n")

LIBRARY = {
"LIB_DEBUG":
"""
LIB_DEBUG: {
    INC16(STACK)
    ldy #0
    lda (STACK),y
    tax
!:
    iny
    lda (STACK),y
    jsr KERNEL_CHROUT
    dex
    bne !-

    clc
    tya
    adc STACK
    sta STACK
    bcc !+
    inc STACK+1
!:
    lda #13
    jsr KERNEL_CHROUT

    rts
}
""".split("\n"),

"LIB_STRING_SUBSCRIPTION":
"""
LIB_STRING_SUBSCRIPTION: {
    tay
    iny
    iny
    lda (STACK),y
    pha
    jsr LIB_STRING_POP
    pla
    ldy #0
    sta (STACK),y
    DEC16(STACK)
    lda #1
    sta (STACK),y
    DEC16(STACK)
    rts
}
""".split("\n"),

"LIB_MUL16":
"""
LIB_MUL16: {
    lda #0          // Initialize RESULT to 0
    sta ZP_DW+2
    ldx #16         // There are 16 bits in ZP_W1
L1:
    lsr ZP_W1+1     // Get low bit of ZP_W1
    ror ZP_W1
    bcc L2          // 0 or 1?
    tay             // If 1, add ZP_W0 (hi byte of RESULT is in A)
    clc
    lda ZP_W0
    adc ZP_DW+2
    sta ZP_DW+2
    tya
    adc ZP_W0+1
L2:
    ror             // "Stairstep" shift
    ror ZP_DW+2
    ror ZP_DW+1
    ror ZP_DW
    dex
    bne L1
    sta ZP_DW+3
    rts
}
""".split("\n"),

"LIB_MUL8":
"""
LIB_MUL8: {
    lda #0          // Initialize RESULT to 0
    ldx #8          // There are 8 bits in ZP_B1
L1
    lsr ZP_B2       // Get low bit of ZP_B1
    bcc L2          // 0 or 1?
    clc             // If 1, add ZP_B0
    adc ZP_B1
L2
    ror             // "Stairstep" shift (catching carry from add)
    ror ZP_W0
    dex
    bne L1
    sta ZP_W0+1
    rts
}
""".split("\n"),

"LIB_STRING_MERGE":
"""
LIB_STRING_MERGE: {
    INC16(STACK)

    ldy #0
    lda (STACK),y   // left size
    tay
    iny
    clc
    adc (STACK),y   // right size
    tax
!:
    dey
    lda (STACK),y
    iny
    sta (STACK),y
    dey
    bne !-

    iny
    txa
    sta (STACK),y
    rts
}
""".split("\n"),

"LIB_STRING_POP":
"""
LIB_STRING_POP: {
    ldy #1
    lda (STACK),y
    clc
    adc #1
    adc STACK
    sta STACK
    bcc exit
    inc STACK+1
exit:
    rts
}
""".split("\n"),

"LIB_STRING_PUSH":
"""
LIB_STRING_PUSH: {
    ldy #0
    sec
    lda STACK
    sbc (ZP_W0),y
    sta STACK
    bcs !+
    dec STACK+1
!:

    lda (ZP_W0),y
    tay
loop:
    lda (ZP_W0),y
    sta (STACK),y
    dey
    bpl loop

    DEC16(STACK)
    rts
}
""".split("\n"),


"LIB_CSTRING_BYTE":
"""
LIB_CSTRING_BYTE: {
    ldx #$ff                // Prepare for subtraction
    sec
!:                          // Count how many 100s
    inx
    sbc #100
    bcs !-
    adc #100
    stx ZP_B0

    ldx #$ff                // Prepare for subtraction
    sec
!:                          // Count how many 10s
    inx
    sbc #10
    bcs !-
    adc #10

    // ZP_B0 = 100s, x=10s, a=1s

    ldy #0                  // store 1s
    ora #$30                // petscii '0'
    sta (STACK),y

    DEC16(STACK)
    txa                     // exit if 10s and 100s are zero
    ora ZP_B0
    beq exit_1

    txa                     // store 10s
    ora #$30
    sta (STACK),y

    DEC16(STACK)
    lda ZP_B0
    beq exit_2

    ora #$30
    sta (STACK),y
    DEC16(STACK)

exit_3:
    lda #3
    .byte $2c
exit_2:
    lda #2
    .byte $2c
exit_1:
    lda #1
exit:
    sta (STACK),y
    DEC16(STACK)
    rts
}
""".split("\n"),

"LIB_STRING_PULL":
"""
LIB_STRING_PULL: {
    INC16(STACK)

    ldy #0
    lda (STACK),y   // size
    cmp ZP_B0
    bcc !+          // size <= capacity
    lda ZP_B0       // capacity

!:
    sta (ZP_W0),y   // write size
    tay
    beq !+
loop:
    lda (STACK),y
    sta (ZP_W0),y
    dey
    bne loop

!:
    ldy #0
    clc
    lda STACK
    adc (STACK),y
    sta STACK
    bcc !+
    inc STACK+1
!:

    rts
}
""".split("\n"),

}
