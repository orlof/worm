
from dotwiz import DotWiz

from lexer import Lexer, operators_arithmetic, operators_comparison
from nodes import *
from parse_util import *


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

        self._shared = AstList()
        self._local = [AstList()]

        self.constants = AstDict()

        self.shared = AstDict()
        self.local = AstDict()
        self.ast = AstList()

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
        for k, v in self.constants.items():
            v.optimize_constants(self.constants)

        self._shared.optimize_constants(self.constants)
        self._local[0].optimize_constants(self.constants)

        self.ast.optimize_constants(self.constants)

        # CONVERT LOCAL AND SHARED LISTS TO DICTS
        for node in self._shared:
            assert node.type == "VARIABLE_DECLARATION"
            for def_var in process_variable_declaration(node):
                self.shared[def_var.name] = init_variable(def_var)

        for node in self._local[0]:
            assert node.type == "VARIABLE_DECLARATION":
            for def_var in process_variable_declaration(node):
                self.local[def_var.name] = init_variable(def_var)

        for node in self.shared.values():
            if node.type == "DEF_FUN":
                for var in node._local:
                    assert var == "DEF_VAR_TYPE"
                    for def_var in process_variable_declaration(var):
                        node.local[def_var.name] = def_var

        names = self.shared.copy()
        names.update(self.local)
        propagate_type(self.ast, names)

        for k, v in self.shared.items():
            if v.type == "DEF_FUN":
                names = self.shared.copy()
                names.update(v.local)
                propagate_type(v.body, names)

        return self.shared, self.ast, self.local

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

            MARK = self.pos

            capacity = 0
            if token == "STRING" and self.token == "[":
                self.advance("NUMERIC")
                capacity = self.value
                self.advance("]")
                self.advance()

            assert self.token == "IDENT"

            name = self.value
            self.advance()

            if self.token == "(":
                # FUNCTION DEFINITION
                self.advance()

                node = AstNode(type="DEF_FUN", name=name, 
                    return_type=token, 
                    capacity=capacity,
                    size=1, 
                    args=AstList(),
                    _local=AstList(),
                    local=AstDict()
                )

                while self.token != ")":
                    assert self.token in ("BYTE", "WORD", "STRING")

                    arg_type = self.token
                    self.advance()

                    arg_capacity = 0
                    if token == "STRING" and self.token == "[":
                        self.advance("NUMERIC")
                        arg_capacity = self.value
                        self.advance("]")
                        self.advance()

                    assert self.token == "IDENT"
                    arg_name = self.value

                    self.advance()
                    node.args.append(AstNode(type="DEF_VAR", 
                        return_type=arg_type, name=arg_name, 
                        capacity=arg_capacity, size=1,
                        index_type="BYTE"
                    ))
                    if self.token == ",":
                        self.advance()

                self.advance()

                # CREATE NEW SCOPE
                self._local.append(AstList())
                # ADD RETURN VALUE TO LOCALS
                self._local[-1].append(AstNode(type="DEF_VAR", 
                    return_type=node.return_type, name="_RETURN_", 
                    size=1
                ))
                # ADD ARGS TO LOCALS
                for n in node.args:
                    self._local[-1].append(n)

                node.body = self.suite()
                # STORE LOCALS TO FUNCTION and REMOVE SCOPE
                node._local = self._local.pop()
                
                self._shared.append(node)
                return None

            else:
                # VARIABLE DEFINITION
                self.pos = MARK
                self._local[-1].append(AstNode(type="DEF_VAR", 
                    return_type=token, 
                    value=self.simple_assignment_list()
                ))
                return None

        elif self.token == "SHARED":
            self.advance()
            if self.token in ("BYTE", "WORD"):
                token = self.token
                self.advance()
                #for n in create_variables(AstNode(return_type=token, value=self.simple_assignment_list())):
                #    self.shared[n.name] = n
                self._shared.append(AstNode(type="DEF_VAR", return_type=token, value=self.simple_assignment_list()))
                return None
            else:
                raise NotImplemented()

        elif self.token == "POKE":
            self.advance()
            addr = self.expr()
            if self.token != ",":
                raise SyntaxError("No comma in POKE")
            self.advance()
            value = self.expr()
            return AstNode(type="POKE", addr=addr, value=value)

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

        while self.token == "=":
            self.advance()

            right = self.expr_list(len(left))
            left = AstNode(type="=", left=left, right=right)

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
            token = self.token
            value = self.value
            self.advance()
            return AstNode(type=token, value=value)

        raise SyntaxError("invalid syntax")
