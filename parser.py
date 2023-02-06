
import hashlib
from dotwiz import DotWiz

from lexer import Lexer, operators_arithmetic, operators_comparison
from nodes import *
from parse_util import *


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

        #self.constants = AstDict()
        self.literals = AstDict()

        self.shared = AstDict()
        self.scope = AstList()
        self.scope.append(AstNode(
            type="DEF_FUN", name="MAIN",
            return_type="NA",
            local=AstDict(), ast=AstList(),
        ))
        #self.ast = AstList()

    @property
    def ast(self):
        return self.scope[-1].ast

    @ast.setter
    def ast(self, value):
        self.scope[-1].ast = value

    @property
    def local(self):
        return self.scope[-1].local

    @property
    def func(self):
        return self.scope[-1]

    @property
    def token(self):
        return self.tokens[self.pos][0]

    @property
    def next_token(self):
        return self.tokens[self.pos + 1][0]

    @property
    def value(self):
        return self.tokens[self.pos][1]

    def advance(self, accepted=None):
        self.pos +=  1
        assert accepted==None or self.token == accepted

    def parse(self):
        while True:
            if self.token == "START":
                self.advance()
                self.ast.append(AstNode(type="START"))
                continue
            if self.token == "END":
                self.ast.append(AstNode(type="END"))
                break

            elif self.token == "CONST":
                assert False
                self.advance()
                if self.token == "IDENT":
                    name = self.value
                    self.advance()
                    if self.token != "=":
                        raise SyntaxError("Malformed CONST")
                    self.advance()
                    self.constants[name] = AstNode(type="CONST", name=name, value=self.expr())
                else:
                    raise SyntaxError()
            else:
                stmt = self.stmt()
                if stmt:
                    self.ast.append(stmt)

        # OPTIMIZE CONSTANTS
        # for node in self.constants.values():
        #     node.optimize_constants(self.constants)

        self.shared.optimize_constants()
        self.local.optimize_constants()
        self.ast.optimize_constants()

        # INITIALIZE VARIABLES
        assert len(self.scope) == 1
        process_variables(self.local)
        process_variables(self.shared)

        # verify code trees
        names = self.shared.copy()
        names.update(self.local)
        propagate_type(self.ast, names)

        for node in self.shared.values():
            if node.type == "DEF_FUN":
                names = self.shared.copy()
                names.update(node.local)
                names["__SELF__"] = node
                propagate_type(node.ast, names)

        self.literals = collect_literals(self.ast)
        for node in self.shared.values():
            if node.type == "DEF_FUN":
                self.literals.update(collect_literals(node.ast))

        return self.shared, self.literals, self.ast, self.local

    def suite(self):
        if self.token != "INDENT":
            raise SyntaxError("No suite")
        self.advance()
        suite = AstList()
        while self.token != "DEDENT":
            suite.append(self.stmt())

        self.advance()
        return suite

    def stmt(self):
        if self.token in ("BYTE", "WORD", "STRING"):
            token = self.token
            self.advance()

            capacity = AstNode(type="NUMERIC", value=0)
            if token == "STRING" and self.token == "[":
                self.advance()
                capacity = self.expr()
                assert self.token == "]"
                self.advance()

            MARK = self.pos

            assert self.token == "IDENT"

            name = self.value
            self.advance()

            if self.token == "(":
                # FUNCTION DEFINITION
                self.advance()

                # CREATE NEW SCOPE
                func = AstNode(type="DEF_FUN", name=name,
                    return_type=token,
                    capacity=capacity,
                    size=1,
                    args = self.ast_arg_list(),
                    local=AstDict(), ast=AstList()
                )
                self.scope.append(func)

                assert self.token == ")"
                self.advance()

                # ADD ARGS TO LOCALS
                self.local.update({node.name: node for node in func.args})

                self.ast += self.suite()

                self.shared[func.name] = self.scope.pop()
                return None

            else:
                # VARIABLE DEFINITION
                self.pos = MARK

                for node in self.simple_assignment_list():
                    name = node.value if node.type=="IDENT" else node.left.value
                    self.local[name] = AstNode(type="AST_VAR",
                        name=name,
                        return_type=token,
                        capacity=capacity,
                        ast=node,
                    )
                return None

        elif self.token == "SHARED":
            self.advance()
            if self.token in ("BYTE", "WORD", "STRING"):
                token = self.token
                self.advance()

                for node in self.simple_assignment_list():
                    self.shared[node.name] = AstNode(type="AST_VAR",
                        name=node.name.value if node.type=="IDENT" else node.left.value,
                        return_type=token,
                        capacity=capacity,
                        ast=node,
                    )
                return None
            else:
                raise NotImplemented()

        elif self.token == "ASM":
            self.advance()
            assert self.token == "ASM_BLOCK"
            value = self.value
            self.advance()
            assert self.token == "END"
            self.advance()
            assert self.token == "ASM"
            self.advance()
            return AstNode(type="ASM", value=value)

        elif self.token == "POKE":
            self.advance()
            addr = self.expr()
            if self.token != ",":
                raise SyntaxError("No comma in POKE")
            self.advance()
            value = self.expr()
            return AstNode(type="POKE", addr=addr, value=value)

        elif self.token == "DEBUG":
            self.advance()
            value = self.expr()
            return AstNode(type="DEBUG", value=value)

        elif self.token == "PEEK":
            self.advance()
            if self.token != "(":
                raise SyntaxError()
            self.advance()
            addr = self.expr()
            if self.token != ")":
                raise SyntaxError()
            self.advance()

            return AstNode(type="PEEK", addr=addr)

        elif self.token == "RETURN":
            self.advance()
            return AstNode(type="RETURN", value=self.expr())

        elif self.token == "WHILE":
            self.advance()

            expr = self.expr()
            # if self.token != ":":
            #     raise SyntaxError("No : after while expression")
            # self.advance()
            #if self.token == "INDENT":
            return AstNode(type="WHILE", expr=expr, suite=self.suite())
            #else:
            #    return AstNode(type="WHILE", expr_type=expr_type, expr=expr, suite=self.stmt())

        elif self.token == "IF":
            self.advance()
            node = AstNode(type="IF", branches=AstList())

            while True:
                node.branches.append(AstNode(type="ELSE_IF", expr=self.expr(), body=self.suite()))
                if self.token == "ELSE" and self.next_token == "IF":
                    self.advance()
                    self.advance()
                    continue
                break
            if self.token == "ELSE":
                self.advance()
                node._else = AstNode(type="ELSE", body=self.suite())

            return node

        else:
            return self.assignment()
            #print("Not processed: %s, %s" % (self.token, self.value))
            #self.advance()

    def ast_arg_list(self):
        arg_list = AstList()
        while self.token in ("BYTE", "WORD", "STRING"):
            typedef = self.token
            self.advance()

            capacity = AstNode(type="NUMERIC", value=0)
            if typedef == "STRING" and self.token == "[":
                self.advance()
                capacity = self.expr()
                self.advance()

            assert self.token == "IDENT"

            name = self.value
            self.advance()

            arg_list.append(
                AstNode(type="AST_ARG", name=name,
                    return_type=typedef,
                    capacity=capacity,
                    size=1,
                )
            )

            if self.token == ",":
                self.advance()

        return arg_list

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

        elif self.token == "=":
            while self.token == "=":
                self.advance()

                right = self.expr_list(len(left))
                left = AstNode(type="=", left=left, right=right)

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
        if self.token in ("NOT", "!"):
            self.advance()
            return AstNode(type="NOT", value=self.expr())

        return self.comp()

    def comp(self):
        nodes = AstList()
        left = self.bitwise_or()

        while self.token in ("==", "<", ">", ">=", "<=", "!=", "<>"):
            op = self.token
            self.advance()

            right = self.bitwise_or()

            nodes.append(AstNode(type=op, left=left, right=right))

            left = right

        if not nodes:
            return left
        elif len(nodes) == 1:
            return nodes[0]
        else:
            return AstNode(type="AND_LIST", value=nodes)

    def bitwise_or(self):
        left = self.bitwise_xor()

        while self.token == "|":
            op = self.token
            self.advance()
            right = self.bitwise_xor()

            left = AstNode(type=op, left=left, right=right)

        return left

    def bitwise_xor(self):
        left = self.bitwise_and()

        while self.token == "^":
            op = self.token
            self.advance()
            right = self.bitwise_and()

            left = AstNode(type=op, left=left, right=right)

        return left

    def bitwise_and(self):
        left = self.shift()

        while self.token == "&":
            op = self.token
            self.advance()
            right = self.shift()

            left = AstNode(type=op, left=left, right=right)

        return left

    def shift(self):
        left = self.addsub()

        while self.token in ("<<", ">>", ">>>", ">><", "<<>"):
            op = self.token
            self.advance()
            right = self.addsub()

            left = AstNode(type=op, left=left, right=right)

        return left

    def addsub(self):
        left = self.muldiv()

        while self.token in ("+", "-"):
            op = self.token
            self.advance()
            right = self.muldiv()

            left = AstNode(type=op, left=left, right=right)

        return left

    def muldiv(self):
        left = self.unary_minusplusnot()

        while self.token in ("*", "/"):
            op = self.token
            self.advance()
            right = self.unary_minusplusnot()

            left = AstNode(type=op, left=left, right=right)

        return left

    def unary_minusplusnot(self):
        if self.token == "-":
            self.advance()
            return AstNode(type="UMINUS", value=self.expr())
        if self.token == "+":
            self.advance()
            return self.expr()
        if self.token == "~":
            self.advance()
            return AstNode(type="~", value=self.expr())

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
                    left = AstNode(type="CALL", name=left.value, args=AstList())
                else:
                    left = AstNode(type="CALL", name=left.value, args=self.expr_list())

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
                node = AstList()
            else:
                node = self.expr_list()

                if self.token != "]":
                    raise SyntaxError("invalid syntax")

            self.advance()
            return node

        if self.token in ("NUMERIC", "LITERAL", "IDENT"):
            node = AstNode(type=self.token, value=self.value)
            self.advance()
            return node

        raise SyntaxError("invalid syntax")
