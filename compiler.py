from nodes import *

def format_data(name, data):
    if type(data) == int:
        data = [data]
    rows = ["%s:" % name]
    data = ["0x{:02x}".format(x).replace("0x", "$") for x in data]
    for index in range(0, len(data), 16):
        rows.append("    .byte %s" % ", ".join(data[index:index+16]))
    return rows


class Compiler:
    def __init__(self, names):
        self.names = names
        self.library = AstNode()
        self.macro = AstNode()

    @staticmethod
    def compile_dataset(dataset):
        code = []
        if "name" in dataset:
            code += [
                "%s:" % dataset.name.value
            ]
        for data in dataset.value:
            if data.type == "BYTE":
                byte = []
                for e in data.value:
                    if e.node in ("NUMERIC", "IDENT"):
                        byte.append(str(e.value))
                    elif e.node == "LITERAL":
                        if byte:
                            code += [
                                "    .byte %s" % ", ".join(byte)
                            ]
                        byte = []
                        code += [
                            "    .text \"%s\"" % e.value
                        ]
                if byte:
                    code += [
                        "    .byte %s" % ", ".join(byte)
                    ]
            elif data.type == "WORD":
                word = []
                for e in data.value:
                    if e.node in ("NUMERIC", "IDENT"):
                        word.append(str(e.value))
                if word:
                    code += [
                        "    .word %s" % ", ".join(word)
                    ]
        return code

    def compile_function(self, func):
        self.func_name = func.name
        self.names.update(func.vars)
        self.names.update(func.data)

        code = [
            "",
        ]
        for node in func.code:
            asm = self.compile_code(node)
            code += asm

        code += ["    rts"]

        # COMPILE VARIABLES
        if func.vars:
            code += [""]
            # code += ["    // VARIABLES"]
            for var in func.vars.values():
                code += self.compile_variable(var)

        # COMPILE DATA
        if func.data:
            code += [""]
            # code += ["    // DATA"]
            for data in func.data:
                code += self.compile_dataset(data)

        return code

    def compile2(self):
        # COMPILE MAIN
        self.names = self.shared.copy()
        self.names.update(self.local)
        self.func_name = "main"
        for node in self.ast:
            a=self.compile_code(node)
            self.code += a

        # COMPILE FUNCTIONS
        for name, node in self.shared.items():
            if node.type == "DEF_FUN":
                self.func_name = node.name
                self.names = self.shared.copy()
                self.names.update(node.local)
                self.code += self.compile_function(node)

        # ADD MACROS AND LIBRARIES
        self.include_macros(self.code)
        self.include_libraries(self.code)

        if self.macro:
            self.code += ["// MACROS"]
            for macro in self.macro.values():
                self.code += macro

        if self.library:
            self.code += ["// LIBRARIES"]
            for lib in self.library.values():
                self.code += lib

        # COMPILE LITERALS
        self.code += ["// LITERALS"]
        self.code += [".encoding \"petscii_upper\""]

        for node in self.literals.values():
            self.code += self.compile_literal(node)

        # COMPILE DATA
        self.code += ["// DATA"]
        for data in self.data:
            self.code += self.compile_dataset(data)

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

    def compile_code(self, node):
        if node.node == "LIST":
            assert False
            nodes = []
            for n in node:
                nodes += self.compile_code(n)
            return nodes

        elif node.node == "START":
            return [
            ]

        elif node.node == "END":
            return [
            ]

        elif node.node == "ASM":
            code = ["{"] + node.value + ["}"]
            return code

        elif node.node == "LABEL":
            code = [
                "%s:" % node.name,
            ]
            return code

        elif node.node == "GOTO":
            code = [
                "    jmp %s" % node.value,
            ]
            return code

        elif node.node == "ORIGIN":
            code = [
                "*=%d" % node.value,
            ]
            return code

        elif node.node == "CALL":
            left_names = node.ns.get_names() if "ns" in node else self.names
            def_args = left_names[node.name].args
            if len(def_args) != len(node.args):
                raise SyntaxError("%s expects %d arguments, got %d" % (node.name, len(def_args), len(node.args)))

            code = []
            for call_arg, def_arg in zip(node.args, def_args):
                code += self.compile_expr(call_arg)
                if call_arg.type == "BYTE":
                    code += [
                        "    pla",
                        "    sta %s.%s" % (node.name, def_arg.name),
                    ]
                elif call_arg.type == "WORD":
                    code += [
                        "    pla",
                        "    sta %s.%s" % (node.name, def_arg.name),
                        "    pla",
                        "    sta %s.%s+1" % (node.name, def_arg.name),
                    ]
                elif call_arg.type == "STRING":
                    code += [
                        "    lda #%d" % def_arg.capacity,
                        "    STR_PULL_TO(%s.%s)" % (node.name, def_arg.name),
                    ]
                else:
                    raise NotImplementedError()

            if "ns" in node:
                code += [
                    "    jsr %s.%s" % (node.ns.name, node.name)
                ]
            else:
                code += [
                    "    jsr %s" % node.name
                ]

            if self.names[node.name].type == "STRING":
                code += [
                    "    jsr LIB_STRING_POP",
                ]

            return code

        elif node.node == "RETURN":
            code = self.compile_expr(node.value)
            if self.names[self.func_name].type == "BYTE":
                code += [
                    "    pla",
                    "    sta %s" % self.func_name,
                    "    rts",
                ]
            elif self.names[self.func_name].type == "WORD":
                code += [
                    "pla",
                    "sta %s" % self.func_name,
                    "pla",
                    "sta %s+1" % self.func_name,
                    "rts",
                ]
            return code

        elif node.node == "POKE":
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

        elif node.node == "DEBUG":
            return (
                self.compile_expr(node.value) +
                [
                    "    jsr LIB_DEBUG",
                ]
            )

        elif node.node == "PASS":
            return []

        elif node.node == "DATASET":
            return self.compile_dataset(node)

        elif node.node == "PEEK":
            code = [ "// BYTE %s" % node.node ]
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

        elif node.node == "WHILE":
            code = []
            code += [
                "{ // WHILE",
                "expr:",
            ]

            if node.expr.node == "NUMERIC" and node.expr.value == 1:
                code += ["suite:"]
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

        elif node.node == "IF":
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

        elif node.node == "=":
            # a,b = 1,2 (calculate right side)
            nodes = []
            for rnode in node.right:
                nodes += self.compile_expr(rnode)

            for lnode in node.left[::-1]:
                names = lnode.ns.get_names() if "ns" in lnode else self.names
                if lnode.node == "IDENT":
                    var_def = names[lnode.value]
                    name = "%s.%s" % (names.name, var_def.name) if "ns" in lnode else var_def.name
                    if var_def.type == "BYTE":
                        nodes += [
                            "    // assign to byte ident",
                            "    pla",
                            "    sta %s" % name,
                        ]
                    elif var_def.type == "WORD":
                        nodes += [
                            "    // assign to word ident",
                            "    pla",
                            "    sta %s" % name,
                            "    pla",
                            "    sta %s+1" % name,
                        ]
                    elif var_def.type == "STRING":
                        nodes += [
                            "    // assign string",
                            "    lda #%d" % var_def.capacity,
                            "    STR_PULL_TO(%s)" % name,
                        ]
                    else:
                        raise NotImplementedError()

                elif lnode.node == "SUBSCRIPTION":
                    var_def = names[lnode.left.value]
                    name = "%s.%s" % (names.name, var_def.name) if "ns" in lnode else var_def.name
                    if var_def.type == "BYTE" and var_def.index_type == "BYTE":
                        nodes += self.compile_expr(lnode.right)
                        nodes += [
                            "    // byte[byte]=byte",
                            "    pla",
                            "    tax",
                            "    pla",
                            "    sta %s,x" % name,
                        ]
                    elif var_def.type == "BYTE" and var_def.index_type == "WORD":
                        nodes += self.compile_expr(lnode.right)
                        nodes += [
                            "    // byte[word]=byte",
                            "    pla",
                            "    clc",
                            "    adc #<%s" % name,
                            "    sta ZP_W0",

                            "    pla",
                            "    adc #>%s" % name,
                            "    sta ZP_W0+1",

                            "    pla",
                            "    ldy #0",
                            "    sta (ZP_W0),y"
                        ]
                    elif var_def.type == "WORD" and var_def.index_type == "BYTE":
                        nodes += self.compile_expr(lnode.right)
                        nodes += [
                            "    // word[byte]=word",
                            "    pla",
                            "    tax",
                            "    pla",
                            "    sta %s_lo,x" % name,
                            "    pla",
                            "    sta %s_hi,x" % name,
                        ]
                    elif var_def.type == "WORD" and var_def.index_type == "WORD":
                        nodes += self.compile_expr(lnode.right)
                        nodes += [
                            "    // word[word]=word",
                            "    pla",
                            "    clc",
                            "    adc #<%s_lo" % name,
                            "    sta ZP_W0",

                            "    pla",
                            "    adc #>%s_lo" % name,
                            "    sta ZP_W0+1",

                            "    pla",
                            "    ldy #0",
                            "    sta (ZP_W0),y",

                            "    lda #%d" % ((var_def.size >> 8) & 0xff),
                            # "    clc",
                            "    adc ZP_W0+1",
                            "    sta ZP_W0+1",

                            "    pla",
                            "    ldy #%d" % (var_def.size & 0xff),
                            "    sta (ZP_W0),y"
                        ]
            return nodes

    @staticmethod
    def compile_literal(node):
        assert node.node == "LITERAL"

        return (
            ["%s:" % node.md5]+
            ["    .byte %d" % len(node.value)] +
            ["    .text \"%s\"" % node.value]
        )

    @staticmethod
    def compile_variable(node):
        assert node.node == "VARIABLE"

        if node.type == "BYTE":
            # byte a
            return list(format_data(node.name, node.initializer))
        elif node.type == "WORD":
            # word a
            return (
                ["%s:" % node.name] +
                format_data("%s_lo" % node.name, map(lambda x: (x & 0xff), node.initializer)) +
                format_data("%s_hi" % node.name, map(lambda x: (x >> 8) & 0xff, node.initializer))
            )
        elif node.type == "STRING":
            result = ["%s:" % node.name]
            for element in node.initializer:
                result += ["    .byte %d" % element[0]]
                result += ["    .text \"%s\"" % element[1]]
            return result
        else:
            raise NotImplementedError()

    def compile_expr(self, node):
        if node.node == "NUMERIC":
            if node.type == "BYTE":
                return [
                    "    lda #%d" % (node.value & 0xff),
                    "    pha",
                ]
            elif node.type == "WORD":
                # 256
                return [
                    "    // word literal %d to stack" % node.value,
                    "    lda #%d" % ((node.value >> 8) & 0xff),
                    "    pha",
                    "    lda #%d" % (node.value & 0xff),
                    "    pha",
                ]
            else:
                raise SyntaxError("Invalid type: %s" + node.type)

        elif node.node == "LITERAL":
            return [
                "    STR_PUSH_FROM(%s)" % node.md5,
            ]

        elif node.node == "IDENT":
            if "ns" in node:
                ident = node.ns.get_names()[node.value]
                name = "%s.%s" % (node.ns.name, ident.name)
            else:
                ident = self.names[node.value]
                name = ident.name

            if ident.type == "BYTE":
                return [
                    "    lda %s" % name,
                    "    pha",
                ]
            elif ident.type == "WORD":
                return [
                    "    lda %s+1" % name,
                    "    pha",
                    "    lda %s" % name,
                    "    pha",
                ]
            elif ident.type == "STRING":
                if ident.size == 1:
                    return [
                        "    STR_PUSH_FROM(%s)" % name,
                    ]
                else:
                    pass
            else:
                raise NotImplementedError()

        elif node.node == "CSTRING":
            if node.value.type == "BYTE":
                return (
                    self.compile_expr(node.value) +
                    [
                        "    lda #0",
                        "    sta ZP_W0+1",
                        "    pla",
                        "    sta ZP_W0",
                        "    lda #0",
                        "    sta ZP_B0",
                        "    jsr LIB_CSTRING_WORD",
                    ]
                )
            if node.value.type == "WORD":
                return (
                    self.compile_expr(node.value) +
                    [
                        "    pla",
                        "    sta ZP_W0",
                        "    pla",
                        "    sta ZP_W0+1",
                        "    lda #0",
                        "    sta ZP_B0",
                        "    jsr LIB_CSTRING_WORD",
                    ]
                )
            else:
                raise NotImplementedError()

        elif node.node == "CWORD":
            if node.value.type == "BYTE":
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

        elif node.node == "CBYTE":
            if node.value.type == "WORD":
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

        elif node.node == "CALL":
            names = node.ns.get_names() if "ns" in node else self.names
            def_args = names[node.name].args
            assert len(def_args) == len(node.args), "%s expects %d arguments, got %d" % (node.name, len(def_args), len(node.args))

            code = []
            for call_arg, def_arg in zip(node.args, def_args):
                if "ns" in node:
                    name = "%s.%s.%s" % (node.ns.name, node.name, def_arg.name)
                else:
                    name = "%s.%s" % (node.name, def_arg.name)

                code += self.compile_expr(call_arg)
                if call_arg.type == "BYTE":
                    code += [
                        "    pla",
                        "    sta %s" % name,
                    ]
                elif call_arg.type == "WORD":
                    code += [
                        "    pla",
                        "    sta %s" % name,
                        "    pla",
                        "    sta %s+1" % name,
                    ]
                elif call_arg.type == "STRING":
                    code += [
                        "    lda #%d" % def_arg.capacity,
                        "    STR_PULL_TO(%s)" % name,
                    ]
                else:
                    raise NotImplementedError()

            name = "%s.%s" % (node.ns.name, node.name) if "ns" in node else node.name
            code += [
                "    jsr %s" % name
            ]

            if node.type == "BYTE":
                code += [
                    "    lda %s.%s" %(name, node.name),
                    "    pha",
                ]
            elif node.type == "WORD":
                code += [
                    "    lda %s.%s+1" %(name, node.name),
                    "    pha",
                    "    lda %s.%s" %(name, node.name),
                    "    pha",
                ]
            elif self.names[node.name].type == "STRING":
                raise NotImplementedError()
            else:
                raise NotImplementedError()

            # if node.type == "BYTE":
            #     code += [
            #         "    lda %s._RETURN_" % node.name,
            #         "    pha"
            #     ]
            # elif node.type == "WORD":
            #     code += [
            #         "    lda %s._RETURN_+1" % node.name,
            #         "    pha"
            #         "    lda %s._RETURN_" % node.name,
            #         "    pha"
            #     ]
            # elif node.type == "STRING":
            #     self.library["LIB_STRING_PUSH"] = LIBRARY["LIB_STRING_PUSH"]
            #     code += [
            #         "    LOAD(%s._RETURN_, ZP_W0)" % node.name,
            #         "    jsr LIB_STRING_PUSH",
            #     ]
            # else:
            #     raise NotImplementedError()

            return code

        elif node.node == "SUBSCRIPTION":
            names = node.ns.get_names() if "ns" in node else self.names
            if node.left.node == "IDENT":
                var_def = names[node.left.value]

                if var_def.type == "STRING":
                    if var_def.size == 1:
                        node.left.ns = node.ns

                        code = ["    // STRING %s" % node.node]
                        code += self.compile_expr(node.left)
                        code += self.compile_expr(node.right)
                        code += [
                            "    pla",
                            "    jsr LIB_STRING_SUBSCRIPTION",
                        ]
                        return code

                    else:
                        # STRING ARRAY INDEX IS ALWAYS WORD
                        name = "%s.%s" % (node.ns.name, var_def.name) if "ns" in node else var_def.name
                        code = ["    // STRING[] %s" % node.node]
                        code += self.compile_expr(node.right)
                        code += [
                            "    pla",
                            "    sta ZP_W0",
                            "    pla",
                            "    sta ZP_W0+1",

                            "    lda #0     // capacity as 16 bit number",
                            "    sta ZP_W1+1",
                            "    lda #%d" % var_def.capacity,
                            "    sta ZP_W1",
                            "    inc ZP_W1  // one byte for size",
                            "    jsr LIB_MUL16",
                            "    clc",
                            "    lda #<%s" % name,
                            "    adc ZP_DW",
                            "    sta ZP_W0",
                            "    lda #>%s" % name,
                            "    adc ZP_DW+1",
                            "    sta ZP_W0+1",
                            "    jsr LIB_STRING_PUSH",
                        ]
                        return code

                if var_def.type == "BYTE" and var_def.index_type == "BYTE":
                    name = "%s.%s" % (node.ns.name, var_def.name) if "ns" in node else var_def.name
                    return (
                        self.compile_expr(node.right) +
                        [
                            "    pla",
                            "    tay",
                            "    lda %s,y" % name,
                            "    pha"
                        ]
                    )
                if var_def.type == "BYTE" and var_def.index_type == "WORD":
                    name = "%s.%s" % (node.ns.name, var_def.name) if "ns" in node else var_def.name
                    return (
                        self.compile_expr(node.right) +
                        [
                            "    pla",
                            "    clc",
                            "    adc #<%s_lo" % name,
                            "    sta ZP_W0",

                            "    pla",
                            "    adc #>%s_lo" % name,
                            "    sta ZP_W0+1",

                            "    ldy #0",
                            "    lda (ZP_W0),y",
                            "    pha",
                        ]
                    )
                if var_def.type == "WORD" and var_def.index_type == "BYTE":
                    name = "%s.%s" % (node.ns.name, var_def.name) if "ns" in node else var_def.name
                    return (
                        self.compile_expr(node.right) +
                        [
                            "    pla",
                            "    tay",

                            "    lda %s_hi,y" % name,
                            "    pha",

                            "    lda %s_lo,y" % name,
                            "    pha",
                        ]
                    )
                if var_def.type == "WORD" and var_def.index_type == "WORD":
                    name = "%s.%s" % (node.ns.name, var_def.name) if "ns" in node else var_def.name
                    return (
                        self.compile_expr(node.right) +
                        [
                            "    pla",
                            "    clc",
                            "    adc #<%s_lo" % name,
                            "    sta ZP_W0",

                            "    pla",
                            "    adc #>%s_lo" % name,
                            "    sta ZP_W0+1",

                            "    ldy #0",
                            "    lda (ZP_W0),y",
                            "    tax",

                            "    lda #%d" % ((var_def.size >> 8) & 0xff),
                            "    clc",
                            "    adc ZP_W0+1",
                            "    sta ZP_W0+1",
                            "    ldy #%d" % (var_def.size & 0xff),
                            "    lda (ZP_W0),y",
                            "    pha",
                            "    txa",
                            "    pha",
                        ]
                    )
            elif node.left.type == "STRING":
                code = ["    // %s %s" % (node.type, node.node)]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    jsr LIB_STRING_SUBSCRIPTION",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.node == "AND_LIST":
            if len(node.value) == 1:
                return self.compile_expr(node.value[0])
            else:
                raise NotImplemented()

        elif node.node == "<":
            if node.left.type == "BYTE":
                code = ["    // %s %s" % (node.type, node.node)]
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
                ]
                return code
            else:
                raise NotImplemented()

        elif node.node == "AND":
            if node.type == "BYTE":
                code = [ "{   // %s %s" % (node.type, node.node) ]
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

        elif node.node == "OR":
            if node.type == "BYTE":
                code = [ "{  // %s %s" % (node.type, node.node) ]
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

        elif node.node == "NOT":
            if node.type == "BYTE":
                code = [ "{  // %s %s" % (node.type, node.node) ]
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

        elif node.node == "+":
            if node.type == "BYTE":
                code = ["    // %s %s" % (node.type, node.node)]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_W0",
                    "    pla",
                    "    clc",
                    "    adc ZP_W0",
                    "    pha",
                ]
                return code

            elif node.type == "WORD":
                code = ["    // %s %s" % (node.type, node.node)]
                code += self.compile_expr(node.left)
                code += self.compile_expr(node.right)
                code += [
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
                    ]
                return code

            elif node.type == "STRING":
                code = ["    // %s %s" % (node.type, node.node)]
                code += self.compile_expr(node.right)
                code += self.compile_expr(node.left)
                code += [
                    "    jsr LIB_STRING_MERGE",
                ]
                return code
            else:
                raise NotImplemented()

        elif node.node == "-":
            if node.type == "BYTE":
                code = ["    // %s %s" % (node.type, node.node)]
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_W0",
                ]
                code += self.compile_expr(node.left)
                code += [
                    "    pla",
                    "    sec",
                    "    sbc ZP_W0",
                    "    pha",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.node == "*":
            if node.type == "BYTE":
                code = ["    // %s %s" % (node.type, node.node)]
                code += self.compile_expr(node.left)
                code += [
                    "    pla",
                    "    sta ZP_B0",
                ]
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_B1",

                    "    jsr LIB_MUL8",

                    "    lda ZP_W0",
                    "    pha",
                ]
                return code
            if node.type == "WORD":
                code = ["    // %s %s" % (node.type, node.node)]
                code += self.compile_expr(node.left)
                code += [
                    "    pla",
                    "    sta ZP_W0",
                    "    pla",
                    "    sta ZP_W0+1",
                ]
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_W1",
                    "    pla",
                    "    sta ZP_W1+1",

                    "    jsr LIB_MUL16",

                    "    lda ZP_DW",
                    "    pha",
                    "    lda ZP_DW+1",
                    "    pha",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.node == "/":
            if node.type == "BYTE":
                code = ["    // %s %s" % (node.type, node.node)]
                code += self.compile_expr(node.left)
                code += [
                    "    //  ZP_B0 / ZP_B1",
                    "    pla",
                    "    sta ZP_B0",
                ]
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_B1",

                    "    jsr LIB_DIV8",

                    "    lda ZP_B0",
                    "    pha",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.node == "%":
            if node.type == "BYTE":
                code = ["    // %s %s" % (node.type, node.node)]
                code += self.compile_expr(node.left)
                code += [
                    "    //  ZP_B0 / ZP_B1",
                    "    pla",
                    "    sta ZP_B0",
                ]
                code += self.compile_expr(node.right)
                code += [
                    "    pla",
                    "    sta ZP_B1",

                    "    jsr LIB_DIV8",

                    "    lda ZP_B1",
                    "    pha",
                ]
                return code
            else:
                raise NotImplementedError()

        elif node.node == "&":
            if node.type == "BYTE":
                code = ["{ // BYTE %s" % node.node]
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

        elif node.node == "|":
            if node.type == "BYTE":
                code = ["{ // BYTE %s" % node.node]
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

        elif node.node == "^":
            if node.type == "BYTE":
                code = [ "{ // BYTE %s" % node.node ]
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

        elif node.node == "~":
            if node.type == "BYTE":
                code = [ "{ // BYTE %s" % node.node ]
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

        elif node.node == "<<":
            if node.type == "BYTE":
                code = [ "{ // BYTE %s" % node.node ]
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

        elif node.node == ">>":
            if node.type == "BYTE":
                code = [ "{ // BYTE %s" % node.node ]
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

        elif node.node == ">>>":
            if node.type == "BYTE":
                code = [ "{ // BYTE %s" % node.node ]
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

        elif node.node == ">><":
            if node.type == "BYTE":
                code = [ "{ // BYTE %s" % node.node ]
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

        elif node.node == "<<>":
            if node.type == "BYTE":
                code = [ "{ // BYTE %s" % node.node ]
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
