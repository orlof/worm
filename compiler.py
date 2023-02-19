

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
        self.library = {}
        self.macro = {}

    def include_library(self, name):
        if name not in self.library:
            self.library[name] = LIBRARY[name]
            self.include_macros(LIBRARY[name])
            self.include_libraries(LIBRARY[name])

    def include_macro(self, name):
        if name not in self.macro:
            self.macro[name] = MACRO[name]
            self.include_macros(MACRO[name])
            self.include_libraries(MACRO[name])

    def include_macros(self, lines):
        for line in lines:
            for macro in sorted(MACRO.keys(), key=len, reverse=True):
                if ("    %s" % macro) in line:
                    self.include_macro(macro)

    def include_libraries(self, lines):
        for line in lines:
            token = line.split()
            if len(token) >= 2:
                if token[0] == "jsr":
                    if token[1].startswith("LIB_"):
                        self.include_library(token[1])

    @classmethod
    def compile_dataset(cls, dataset):
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

    def compile(self, func):
        self.func_name = func.name
        self.names.update(func.vars)
        self.names.update(func.data)

        code = []
        code += ["// %s" % func.name]
        for node in func.code:
            asm = self.compile_code(node)
            code += asm

        # COMPILE DATA
        code += [""]
        code += ["// DATA"]
        for data in func.data:
            code += self.compile_dataset(data)

        # COMPILE VARIABLES
        code += [""]
        code += ["// VARIABLES"]
        for var in func.vars.values():
            code += self.compile_variable(var)

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
        if node.node == "LIST":
            assert False
            nodes = []
            for n in node:
                nodes += self.compile_code(n)
            return nodes

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
            def_args = self.shared[node.name].args
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

            code += [
                "    jsr %s" % node.name
            ]
            if self.shared[node.name].type == "STRING":
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

        elif node.node == "START":
            return [
                "// START OF MODULE"
            ]

        elif node.node == "END":
            return [
                "// END OF MODULE"
            ]

        elif node.node == "WHILE":
            code = []
            code += [
                "{ // WHILE",
                "expr:",
            ]

            if node.expr.node == "NUMERIC" and node.expr.value == 1:
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
                if lnode.node == "IDENT":
                    cvar = self.names[lnode.value]
                    if cvar.type == "BYTE":
                        nodes += [
                            "    // assign to byte ident",
                            "    pla",
                            "    sta %s" % cvar.name,
                        ]
                    elif cvar.type == "WORD":
                        nodes += [
                            "    // assign to word ident",
                            "    pla",
                            "    sta %s" % cvar.name,
                            "    pla",
                            "    sta %s+1" % cvar.name,
                        ]
                    elif cvar.type == "STRING":
                        nodes += [
                            "    // assign string",
                            "    lda #%d" % cvar.capacity,
                            "    STR_PULL_TO(%s)" % cvar.name,
                        ]
                    else:
                        raise NotImplementedError()

                elif lnode.node == "SUBSCRIPTION":
                    cvar = self.names[lnode.left.value]
                    if cvar.type == "BYTE" and cvar.index_type == "BYTE":
                        nodes += self.compile_expr(lnode.right)
                        nodes += [
                            "    // byte[byte]=byte",
                            "    pla",
                            "    tax",
                            "    pla",
                            "    sta %s,x" % cvar.name,
                        ]
                    elif cvar.type == "BYTE" and cvar.index_type == "WORD":
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
                    elif cvar.type == "WORD" and cvar.index_type == "BYTE":
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
        assert node.node == "LITERAL"

        return (
            ["%s:" % node.md5]+
            ["    .byte %d" % len(node.value)] +
            ["    .text \"%s\"" % node.value]
        )

    @classmethod
    def compile_variable(cls, node):
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
                raise SyntaxError("Invalid type: %s" + out_type)

        elif node.node == "LITERAL":
            return [
                "    STR_PUSH_FROM(%s)" % node.md5,
            ]

        elif node.node == "IDENT":
            ident = self.names[node.value]

            if ident.type == "BYTE":
                # color
                return [
                    "    // push byte value of %s to stack" % ident.name,
                    "    lda %s" % ident.name,
                    "    pha",
                ]
            elif ident.type == "WORD":
                return [
                    "    // cword to stack",
                    "    lda %s+1" % ident.name,
                    "    pha",
                    "    lda %s" % ident.name,
                    "    pha",
                ]
            elif ident.type == "STRING":
                if ident.size == 1:
                    return [
                        "    STR_PUSH_FROM(%s)" % ident.name,
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

        elif node.node == "CALL":
            def_args = self.shared[node.name].args
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

            code += [
                "    jsr %s" % node.name
            ]
            if node.type == "BYTE":
                code += [
                    "    lda %s.%s" %(node.name, node.name),
                    "    pha",
                ]
            elif self.shared[node.name].type == "WORD":
                code += [
                    "    lda %s.%s+1" %(node.name, node.name),
                    "    pha",
                    "    lda %s.%s" %(node.name, node.name),
                    "    pha",
                ]
            elif self.shared[node.name].type == "STRING":
                pass
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
            if node.left.node == "IDENT":
                cvar = self.names[node.left.value]

                if cvar.type == "STRING":
                    if cvar.size == 1:
                        code = [ "    // STRING %s" % node.node ]
                        code += self.compile_expr(node.left)
                        code += self.compile_expr(node.right)
                        code += [
                            "    pla",
                            "    jsr LIB_STRING_SUBSCRIPTION",
                        ]
                        return code
                    else:
                        # STRING ARRAY INDEX IS ALWAYS WORD
                        code = [ "    // STRING[] %s" % node.node ]
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

                if cvar.type == "BYTE" and cvar.index_type == "BYTE":
                    return (
                        self.compile_expr(node.right) +
                        [
                            "    pla",
                            "    tay",
                            "    lda %s,y" % cvar.name,
                            "    pha"
                        ]
                    )
                if cvar.type == "BYTE" and cvar.index_type == "WORD":
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
                if cvar.type == "WORD" and cvar.index_type == "BYTE":
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
                if cvar.type == "WORD" and cvar.index_type == "WORD":
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
            elif node.right.type == "STRING":
                code = [ "    // %s %s" % (node.type, node.node) ]
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
                code = [ "    // %s %s" % (node.type, node.node) ]
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
                code = [ "    // %s %s" % (node.type, node.node) ]
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

            elif node.type == "WORD":
                code = [ "    // %s %s" % (node.type, node.node) ]
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
                code = [ "    // %s %s" % (node.type, node.node) ]
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
                code = [ "    // %s %s" % (node.type, node.node) ]
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
                code = [ "    // %s %s" % (node.type, node.node) ]
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
                code = [ "    // %s %s" % (node.type, node.node) ]
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
                code = [ "    // %s %s" % (node.type, node.node) ]
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
                code = [ "    // %s %s" % (node.type, node.node) ]
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
                code = [ "{ // BYTE %s" % node.node ]
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
                code = [ "{ // BYTE %s" % node.node ]
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

MACRO = {
"LOAD":
"""
.macro LOAD(Addr, ZP) {
    lda #<Addr
    sta ZP
    lda #>Addr
    sta ZP+1
}
""".split("\n"),

"INC16":
"""
.macro INC16(Addr) {
    inc Addr
    bne !+
    inc Addr+1
!:
}
""".split("\n"),

"ADD16_BYTE":
"""
.macro ADD16_BYTE(wAddr, bAddr) {
    clc
    lda bAddr
    adc wAddr
    sta wAddr
    bcc exit
    inc wAddr+1
exit:
}
""".split("\n"),

"SUB16_BYTE":
"""
.macro SUB16_BYTE(wAddr, bAddr) {
    sec
    lda wAddr
    sbc bAddr
    sta wAddr
    bcs exit
    dec wAddr+1
exit:
}
""".split("\n"),

"DEC16":
"""
.macro DEC16(Addr) {
    lda Addr
    bne !+
    dec Addr+1
!:
    dec Addr
}
""".split("\n"),

"STR_PUSH_FROM":
"""
.macro STR_PUSH_FROM(Addr) {
    LOAD(Addr, ZP_W0)
    jsr LIB_STRING_PUSH
}
""".split("\n"),

"STR_PULL_TO":
"""
.macro STR_PULL_TO(Addr) {
    // capacity in a
    LOAD(Addr, ZP_W0)
    jsr LIB_STRING_PULL
}
""".split("\n")
}

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

"LIB_DIV8":
"""
LIB_DIV8 {
    // ZP_B0 divident, ZP_B1 divisor
    lda #0
    ldx #8
    asl ZP_B0
l1:
    rol
    cmp ZP_B1
    bcc l2
    sbc ZP_B1
l2:
    rol ZP_B0
    dex
    bne l1
    // ZP_B0 quotient, ZP_B1 remainder
    rts
}
""".split("\n"),

"LIB_MUL8":
"""
LIB_MUL8: {
    lda #0          // Initialize ZP_W0 to 0
    ldx #8          // There are 8 bits in ZP_B1
L1:
    lsr ZP_B1       // Get low bit of ZP_B1
    bcc L2          // 0 or 1?
    clc             // If 1, add ZP_B0
    adc ZP_B0
L2:
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

"LIB_CSTRING_WORD":
"""
/*
---------------------------
Print 16-bit decimal number
---------------------------
On entry, ZP_W0=number to print
          ZP_B0=0 or pad character (eg '0' or ' ')
On entry at PrDec16Lp1,
          Y=(number of digits)*2-2, eg 8 for 5 digits
On exit,  A,X,Y,ZP_W0,ZP_B0 corrupted
Size      69 bytes
-----------------------------------------------------------------
*/
LIB_CSTRING_WORD: {
PrDec16:
    lda #1                              // String length
    sta ZP_B1
    ldy #8                              // Offset to powers of ten

PrDec16Lp1:
    ldx #$ff
    sec                                 // Start with digit=-1
PrDec16Lp2:
    lda ZP_W0+0
    sbc PrDec16Tens+0,y
    sta ZP_W0+0                         // Subtract current tens
    lda ZP_W0+1
    sbc PrDec16Tens+1,y
    sta ZP_W0+1
    inx
    bcs PrDec16Lp2                      // Loop until <0
    lda ZP_W0+0
    adc PrDec16Tens+0,Y
    sta ZP_W0+0                         // Add current tens back in
    lda ZP_W0+1
    adc PrDec16Tens+1,y
    sta ZP_W0+1
    txa
    bne PrDec16Digit                    // Not zero, print it
    lda ZP_B0
    bne PrDec16Print
    beq PrDec16Next                     // pad<>0, use it
PrDec16Digit:
    ldx #$30
    stx ZP_B0                           // No more zero padding
    ora #$30                            // Print this digit
PrDec16Print:
    inc ZP_B1
    pha
PrDec16Next:
    dey
    dey
    bpl PrDec16Lp1                      // Loop for next digit

    SUB16_BYTE(STACK, ZP_B1)
    ldy ZP_B1
!:
    pla
    sta (STACK),y
    dey
    cpy #1
    bne !-

    ldx ZP_B1
    dex
    txa
    sta (STACK),y

    rts

PrDec16Tens:
    .word 1
    .word 10
    .word 100
    .word 1000
    .word 10000
}
""".split("\n"),

"LIB_STRING_PULL":
"""
LIB_STRING_PULL: {
    INC16(STACK)

    ldy #0
    cmp (STACK),y   //
    bcc !+          // capacity < size
    lda (STACK),y
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
