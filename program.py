import hashlib

from lexer import Scanner
from nodes import *
from parse_util import process_variables

from pathlib import Path
from compiler import Compiler


class Module:
    def __init__(self, filename):
        self.node = "MODULE"
        self.filename = filename
        self.name = Path(filename).stem

        self.shared = AstNode(
            imports=AstNode(),
            literals=AstNode(),
            consts=AstNode(),
            vars=AstNode(),
            data=AstNode(),
        )

        self.module = AstNode(
            # node = "MODULE", name = "MAIN", type = "NA",
            consts=AstNode(),
            imports=AstNode(),
            funcs=AstNode(),
        )

        root_scope = AstNode(
            node="SCOPE", name="__MAIN__", type="NA",
            vars=AstNode(),
            data=AstNode(),
            code=AstList(),
        )
        self.scopes = AstList([root_scope])
        self.module.funcs["__MAIN__"] = root_scope

        self.scanner = Scanner()
        self.scanner.scan_file(self.filename)

        self.import_module()

        self.module.vars = root_scope.vars
        self.module.data = root_scope.data
        root_scope.vars = AstNode()
        root_scope.data = AstNode()

    @property
    def token(self):
        return self.scanner.token

    @property
    def next_token(self):
        return self.scanner.next_token

    @property
    def value(self):
        return self.scanner.value

    def advance(self):
        return self.scanner.advance()

    @property
    def scope(self):
        return self.scopes[-1]

    def get_names(self):
        names = AstNode()
        names.update(self.shared.vars)
        names.update(self.shared.data)
        names.update(self.module.imports)
        names.update(self.module.funcs)
        names.update(self.module.vars)
        names.update(self.module.data)
        return names

    def compile(self):
        code = [
            "%s: {" % self.name,
        ]

        compiler = Compiler(self.get_names())
        code += compiler.compile_function(self.module.funcs["__MAIN__"])

        for func in (f for f in self.module.funcs.values() if f.name != "__MAIN__"):
            compiler = Compiler(self.get_names())

            code += [
                "",
                "%s: {" % func.name,
            ]
            code += compiler.compile_function(func)
            code += [
                "}   // %s" % func.name,
            ]

        # COMPILE DATA
        if self.module.data:
            code += [""]
            code += ["    // DATA"]
            for data in self.module.data.values():
                code += Compiler.compile_dataset(data)

        # COMPILE VARIABLES
        if self.module.vars:
            code += [""]
            code += ["    // VARIABLES"]
            for var in self.module.vars.values():
                code += Compiler.compile_variable(var)

        code += [
            "}",
            "",
        ]

        return code

    def substitute_constants(self, constants):
        self.shared.substitute_constants(constants)
        self.module.substitute_constants(constants)
        #self.scopes.substitute_constants(constants)

    def fold_constants(self):
        self.shared.fold_constants()
        self.module.fold_constants()
        #self.scopes.fold_constants()

    def fix_type_propagation(self):
        names = AstNode()
        names.update(self.shared.vars)
        names.update(self.shared.data)
        names.update(self.module.imports)
        names.update(self.module.funcs)
        names.update(self.module.vars)
        names.update(self.module.data)

        for func in self.module.funcs.values():
            names.update(func.vars)
            names.update(func.data)
            names["__SELF__"] = func
            for node in func.code:
                node.fix_type_propagation(names)

    def process_variables(self):
        assert len(self.scopes) == 1
        process_variables(self.module.vars)
        process_variables(self.shared.vars)

        for func in self.module.funcs.values():
            process_variables(func.vars)
            # func.capacity = func.capacity.value

    def import_module(self):
        while True:
            if self.token == "START":
                self.advance()
                self.scope.code.append(AstNode(node="START"))

            elif self.token == "END":
                self.scope.code.append(AstNode(node="END"))
                break

            else:
                stmt = self.stmt()
                if stmt is not None:
                    self.scope.code.append(stmt)

    def set_imports(self, imports):
        # replace name->AstNode with name->Module in module.imports
        for key, value in self.module.imports.items():
            self.module.imports[key] = imports[value.filename]

    def stmt(self):
        if self.token == "IMPORT":
            self.advance()
            assert self.token == "LITERAL"
            filename = self.value
            self.advance()
            assert self.token == "AS"
            self.advance()
            name = self.value
            self.shared.imports[filename] = self.module.imports[name] = \
                AstNode(node="IMPORT", name=name, filename=filename)
            self.advance()
            return None

        elif self.token == "SHARED":
            self.advance()

            if self.token == "CONST":
                self.advance()
                self.shared.consts.update(self.constants())
                return None

            elif self.token == "DATA":
                self.advance()

                if self.token == "IDENT":
                    name = self.value
                    self.advance()
                    data = self.parse_data()
                    data.name = name
                    self.shared.data[name] = data
                else:
                    data = self.parse_data()
                    self.shared.code.append(data)

                return None

            elif self.token in ("BYTE", "WORD", "STRING"):
                var_type = self.token
                self.advance()

                capacity = AstNode({"node": "NUMERIC", "value": 0})

                if var_type == "STRING" and self.token == "[":
                    self.advance()
                    capacity = self.expr()
                    assert self.token == "]"
                    self.advance()

                for name, variable in self.variable_definitions().items():
                    variable.type = var_type
                    variable.capacity = capacity
                    self.shared.vars[name] = variable

                return None

        elif self.token == "CONST":
            self.advance()
            self.module.consts.update(self.constants())
            return None

        elif self.token in ("BYTE", "WORD", "STRING"):
            var_type = self.token
            self.advance()

            capacity = AstNode({"node": "NUMERIC", "value": 0})

            if var_type == "STRING" and self.token == "[":
                self.advance()
                capacity = self.expr()
                assert self.token == "]"
                self.advance()

            if self.token == "IDENT" and self.next_token == "(":
                # FUNCTION DEFINITION
                name = self.value
                self.advance()

                self.advance()

                # CREATE NEW SCOPE
                func = AstNode(
                    node="FUNCTION", name=name, type=var_type,
                    capacity=capacity, size=1,
                    args=self.func_arg_list(),
                    const=AstNode(), data=AstNode()
                )
                self.scopes.append(func)

                assert self.token == ")"
                self.advance()

                # ADD ARGS TO LOCALS
                func.vars = AstNode({node.name: node for node in func.args})
                func.vars[name] = AstNode(
                    node="FUNCTION_ARGUMENT", name=name, type=var_type, capacity=capacity, size=1,
                )

                func.code = self.suite()
                self.module.funcs[func.name] = self.scopes.pop()
                return None

            else:
                # VARIABLE DEFINITION
                for name, variable in self.variable_definitions().items():
                    variable.type = var_type
                    variable.capacity = capacity
                    self.scope.vars[name] = variable

                return None

        elif self.token == "DATA":
            self.advance()

            if self.token == "IDENT":
                name = self.value
                self.advance()
                data = self.parse_data()
                data.name = name
                self.scope.data[name] = data
            else:
                data = self.parse_data()
                self.scope.code.append(data)

            return None

        elif self.token == "IDENT" and self.next_token == ":":
            # LABEL
            name = self.value
            self.advance()
            self.advance()

            return AstNode(node="LABEL", name=name)

        elif self.token == "GOTO":
            self.advance()
            assert self.token == "IDENT"
            value = self.value
            self.advance()

            return AstNode(node="GOTO", value=value)

        elif self.token == "ORIGIN":
            self.advance()
            assert self.token in ("NUMERIC", "IDENT")
            value = self.value
            self.advance()

            return AstNode(node="ORIGIN", value=value)

        elif self.token == "ASM":
            self.advance()
            assert self.token == "ASM_BLOCK"
            value = self.value
            self.advance()
            assert self.token == "END" and self.next_token == "ASM"
            self.advance()
            self.advance()
            return AstNode(node="ASM", value=value)

        elif self.token == "POKE":
            self.advance()
            addr = self.expr()
            assert self.token == ","
            self.advance()
            value = self.expr()
            return AstNode(node="POKE", addr=addr, value=value)

        elif self.token == "DEBUG":
            self.advance()
            return AstNode(node="DEBUG", value=self.expr())

        elif self.token == "PEEK":
            self.advance()
            assert self.token == "("
            self.advance()
            node = AstNode(node="PEEK", addr=self.expr())
            assert self.token == ")"
            self.advance()
            return node

        elif self.token == "RETURN":
            self.advance()
            return AstNode(node="RETURN", value=self.expr())

        elif self.token == "WHILE":
            self.advance()
            return AstNode(node="WHILE", expr=self.expr(), suite=self.suite())

        elif self.token == "IF":
            self.advance()
            node = AstNode(node="IF", branches=AstList())

            while True:
                node.branches.append(AstNode(node="ELSE_IF", expr=self.expr(), body=self.suite()))
                if self.token == "ELSE" and self.next_token == "IF":
                    self.advance()
                    self.advance()
                    continue
                break
            if self.token == "ELSE":
                self.advance()
                node._else = AstNode(node="ELSE", body=self.suite())

            return node

        else:
            return self.assignment()

    def constants(self):
        constants = AstNode()
        if self.token == "INDENT":
            self.advance()
            while self.token != "DEDENT":
                constant = self.constant()
                constants[constant.name] = constant
            self.advance()
            return constants
        else:
            constant = self.constant()
            constants[constant.name] = constant
        return constants

    def constant(self):
        assert self.token == "IDENT"
        name = self.value
        self.advance()
        assert self.token == "="
        self.advance()
        return AstNode({"node": "CONSTANT", "name": name, "value": self.expr()})

    def variable_definitions(self):
        variables = AstNode()
        if self.token == "INDENT":
            self.advance()
            while self.token != "DEDENT":
                variables.update(self.line_of_variables())
            self.advance()
        else:
            variables.update(self.line_of_variables())
        return variables

    def line_of_variables(self):
        nodes = AstNode()
        for var in self.simple_assignment_list():
            name = var.value if var.node == "IDENT" else var.left.value
            nodes[name] = AstNode(node="VARIABLE", name=name, ast=var)
        return nodes

    def func_arg_list(self):
        arg_list = AstList()
        while self.token in ("BYTE", "WORD", "STRING"):
            arg_type = self.token
            self.advance()

            capacity = AstNode(node="NUMERIC", value=0)
            if arg_type == "STRING" and self.token == "[":
                self.advance()
                capacity = self.expr()
                self.advance()

            assert self.token == "IDENT"

            name = self.value
            self.advance()

            arg_list.append(
                AstNode(node="FUNCTION_ARGUMENT", name=name, type=arg_type, capacity=capacity, size=1)
            )

            if self.token == ",":
                self.advance()

        return arg_list

    def suite(self):
        assert self.token == "INDENT"
        self.advance()
        suite = AstList()
        while self.token != "DEDENT":
            stmt = self.stmt()
            if stmt is not None:
                suite.append(stmt)

        self.advance()
        return suite

    def parse_data(self):
        node = AstNode(node="DATASET", value=AstList())
        if self.token == "INDENT":
            node.value.extend(self.parse_data_indent())
        else:
            node.value.append(self.parse_data_type())
        return node

    def parse_data_indent(self):
        self.advance()
        nodes = AstList()
        while self.token != "DEDENT":
            nodes.append(self.parse_data_type())
        self.advance()
        return nodes

    def parse_data_type(self):
        assert self.token in ("BYTE", "WORD", "STRING")
        data_type = self.token
        self.advance()

        capacity = AstNode({"node": "NUMERIC", "value": 0})

        if data_type == "STRING" and self.token == "[":
            self.advance()
            capacity = self.expr()
            assert self.token == "]"
            self.advance()

        if self.token == "INDENT":
            self.advance()
            value = AstList()
            while self.token != "DEDENT":
                value.extend(self.expr_list())
            self.advance()
            return AstNode(node="DATA", type=data_type, capacity=capacity, value=value)
        else:
            return AstNode(node="DATA", type=data_type, capacity=capacity, value=self.expr_list())

    def simple_assignment_list(self):
        # a=1,b=2
        nodes = AstList()
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
            left = AstNode(node="=", left=left, right=right)

        return left

    def assignment(self):
        # a,b=1,2
        left = self.expr_list()

        if self.token in [
            "+=", "-=", "*=", "/=", "&=", "|=", "^=", "!=",
            "<<=", ">>=", ">>>=", ">><=", "<<>="
        ]:
            self.advance()

            right = self.expr_list(len(left))
            left = AstNode(node="=", left=left, right=right)

            return left

        elif self.token == "=":
            while self.token == "=":
                self.advance()

                right = self.expr_list(len(left))
                left = AstNode(node="=", left=left, right=right)

            return left

        return left

    def expr_list(self, length=None):
        nodes = AstList()
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
            left = AstNode(node="OR", left=left, right=right)

        return left

    def comp_and(self):
        left = self.comp_not()

        while self.token == "AND":
            self.advance()

            right = self.comp_not()
            left = AstNode(node="AND", left=left, right=right)

        return left

    def comp_not(self):
        if self.token in ("NOT", "!"):
            self.advance()
            return AstNode(node="NOT", value=self.expr())

        return self.comp()

    def comp(self):
        nodes = AstList()
        left = self.bitwise_or()

        while self.token in ("==", "<", ">", ">=", "<=", "!=", "<>"):
            op = self.token
            self.advance()

            right = self.bitwise_or()

            nodes.append(AstNode(node=op, left=left, right=right))

            left = right

        if not nodes:
            return left
        elif len(nodes) == 1:
            return nodes[0]
        else:
            return AstNode(node="AND_LIST", value=nodes)

    def bitwise_or(self):
        left = self.bitwise_xor()

        while self.token == "|":
            op = self.token
            self.advance()
            right = self.bitwise_xor()

            left = AstNode(node=op, left=left, right=right)

        return left

    def bitwise_xor(self):
        left = self.bitwise_and()

        while self.token == "^":
            op = self.token
            self.advance()
            right = self.bitwise_and()

            left = AstNode(node=op, left=left, right=right)

        return left

    def bitwise_and(self):
        left = self.shift()

        while self.token == "&":
            op = self.token
            self.advance()
            right = self.shift()

            left = AstNode(node=op, left=left, right=right)

        return left

    def shift(self):
        left = self.addsub()

        while self.token in ("<<", ">>", ">>>", ">><", "<<>"):
            op = self.token
            self.advance()
            right = self.addsub()

            left = AstNode(node=op, left=left, right=right)

        return left

    def addsub(self):
        left = self.muldiv()

        while self.token in ("+", "-"):
            op = self.token
            self.advance()
            right = self.muldiv()

            left = AstNode(node=op, left=left, right=right)

        return left

    def muldiv(self):
        left = self.unary_minusplusnot()

        while self.token in ("*", "/"):
            op = self.token
            self.advance()
            right = self.unary_minusplusnot()

            left = AstNode(node=op, left=left, right=right)

        return left

    def unary_minusplusnot(self):
        if self.token == "-":
            self.advance()
            return AstNode(node="UMINUS", value=self.expr())
        if self.token == "+":
            self.advance()
            return self.expr()
        if self.token == "~":
            self.advance()
            return AstNode(node="~", value=self.expr())

        return self.exp()

    def exp(self):
        left = self.subscription_or_call()

        while self.token == "**":
            self.advance()
            right = self.exp()
            left = AstNode(node="**", left=left, right=right)

        return left

    def subscription_or_call(self):
        left = self.atom()

        while self.token in ("[", "(", "."):
            op = self.token
            self.advance()

            if op == "[":
                right = self.expr()
                left = AstNode(node="SUBSCRIPTION", left=left, right=right)

                if self.token != "]":
                    raise SyntaxError("invalid syntax")

                self.advance()

            if op == "(":
                if self.token == ")":
                    left = AstNode(node="CALL", name=left.value, args=AstList())
                else:
                    left = AstNode(node="CALL", name=left.value, args=self.expr_list())

                assert self.token == ")"
                self.advance()

            if op == ".":
                assert self.token == "IDENT"
                left = AstNode(node="REFERENCE", left=left, right=self.expr())

        return left

    def atom(self):
        if self.token == "(":
            self.advance()
            node = self.expr()

            assert self.token == ")"
            self.advance()

            return node

        if self.token == "[":
            self.advance()
            if self.token == "]":
                node = AstList()
            else:
                node = self.expr_list()

            assert self.token == "]"
            self.advance()
            return node

        if self.token in ("NUMERIC", "LITERAL", "IDENT"):
            node = AstNode(node=self.token, value=self.value)
            if self.token == "LITERAL":
                node.md5 = "STR_%s" % hashlib.md5(self.value.encode('utf-8')).hexdigest()
                self.shared.literals[node.md5] = node
            self.advance()
            return node

        raise SyntaxError("invalid syntax")


class Program:
    def __init__(self, filename):
        self.filename = filename
        #self.main_module = None
        self.shared = AstNode()
        self.modules = None

    def import_modules(self):
        modules = AstNode()

        import_queue = [self.filename]
        while import_queue:
            filename = import_queue.pop(0)
            if filename in modules:
                continue

            module = Module(filename)
            modules[filename] = module

            import_queue.extend(module.shared.imports.keys())

        return modules

    def collect_shared_components(self):
        shared = AstNode()
        for module in self.modules.values():
            for key in module.shared:
                if key not in shared:
                    shared[key] = module.shared[key]
                else:
                    shared[key].update(module.shared[key])

        return shared

    def compile_literal(self, node):
        assert node.type == "LITERAL"

        return (
            ["%s:" % node.md5]+
            ["    .byte %d" % len(node.value)] +
            ["    .text \"%s\"" % node.value]
        )

    def do_it(self):
        # import modules
        self.modules = self.import_modules()
        for module in self.modules.values():
            module.set_imports(self.modules)

        # get shared constants
        self.shared = self.collect_shared_components()

        # substitute constants
        for module in self.modules.values():
            constants = self.shared.consts.copy()
            constants.update(module.module.consts)
            module.substitute_constants(constants)

        # fold constant values
        for module in self.modules.values():
            module.fold_constants()

        # initialize variables
        for module in self.modules.values():
            module.process_variables()

        # recollect shared values to get folded variables and data
        self.shared = self.collect_shared_components()

        for module in self.modules.values():
            # distribute shared values to modules
            module.shared.vars = self.shared.vars
            module.shared.data = self.shared.data

            # add implicit type conversions to ast
            module.fix_type_propagation()

        code = [
            "// CONSTANTS",
            ".const ZP_DW = $02",
            ".const ZP_W0 = $06",
            ".const ZP_W1 = $08",
            ".const ZP_B0 = $10",
            ".const ZP_B1 = $11",
            ".const STACK = $12",
            ".const KERNEL_CHROUT = $ffd2",
            "",
        ]
        code += [".const %s = %s" % (name, value) for name, value in self.shared.consts.items()]
        code += [
            "*=2048",
            ".byte 0,11,8,10,0,158,50,48,54,49,0,0,0 // SYS 2061",
            "*=2061",
            "    sei",
            "    dec 1",
            "    cli",
            "    LOAD($cfff, STACK)",
            "",
            "    jsr %s.__MAIN__" % self.modules[self.filename].name,
            "",
            "    sei",
            "    inc 1",
            "    cli",
            "    rts",
            "    // END OF PROGRAM",
            "",
        ]

        for module in self.modules.values():
            code += module.compile()

        # COMPILE LITERALS
        if self.shared.literals:
            code += ["// LITERALS"]
            code += [".encoding \"petscii_upper\""]

            for node in self.shared.literals.values():
                code += self.compile_literal(node)

        # COMPILE DATA
        if self.shared.data:
            code += ["// SHARED DATA"]
            for data in self.shared.data.values():
                code += Compiler.compile_dataset(data)

        # COMPILE VARIABLES
        if self.shared.vars:
            code += [""]
            code += ["// SHARED VARIABLES"]
            for var in self.shared.vars.values():
                code += Compiler.compile_variable(var)

        with open("test.asm", "w") as f:
            for line in code:
                # print("%4d %s" % (line.nr, line.src))
                f.write("%s\n" % line)

        print("SENTINEL")


if __name__ == "__main__":
    prg = Program("examples.worm")
    prg.do_it()

