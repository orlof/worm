from dotwiz import DotWiz

from lexer import Lexer, operators_arithmetic, operators_comparison
from nodes import *


def type_conv(ast, shared, local):
    shared = {v.ident: v for v in shared}

class AstNode(Node):
    def add_type_conversion(self, ):
        if self.type == "IDENT":
            self.return_type = self.variables[self.value].type

        elif self.type == "NUMERIC":
            if 0 <= self.value <= 255:
                self.return_type = "BYTE"
            elif 0 <= self.value <= 65535:
                self.return_type = "WORD"
            elif -32768 <= self.value <= 32767:
                self.return_type = "INT"
            else:
                self.return_type = "LONG"

        elif self.type == "SUBSCRIPTION":
            self.left.add_type_conversion()
            self.right.add_type_conversion()
            if seself.left.value
            self.return_type = self.left.return_type

        elif self.type in ("+", "-", "*", "/", "%", "&", "|", "^"):
            left = self.type_out(self.left)
            right = self.type_out(self.right)
            if "LONG" in (left, right):
                if left != "LONG":
                    self.left = AstNode(type="CLONG", value=self.left)
                if right != "LONG":
                    self.right = AstNode(type="CLONG", value=self.right)
                return "LONG"
            if "INT" in (left, right) and "WORD" in (left, right):
                if left != "LONG":
                    self.left = AstNode(type="CLONG", value=self.left)
                if right != "LONG":
                    self.right = AstNode(type="CLONG", value=self.right)
                return "LONG"
            if "WORD" in (left, right):
                if left != "WORD":
                    self.left = AstNode(type="CWORD", value=self.left)
                if right != "WORD":
                    self.right = AstNode(type="CWORD", value=self.right)
                return "WORD"
            if "INT" in (left, right):
                if left != "INT":
                    self.left = AstNode(type="CINT", value=self.left)
                if right != "INT":
                    self.right = AstNode(type="CINT", value=self.right)
                return "INT"
            return "BYTE"

        elif self.type in ("**", "<<", ">>", ">>>", ">><", "<<>"):
            return self.type_out(self.left)

        elif self.type in ("UMINUS", "UNOT", "NOT"):
            return self.type_out(self.value)

        elif self.type in operators_comparison:
            return "BYTE"
        
        else:
            raise NotImplemented()

    def type_in(self):
        if self.type == "IDENT":
            return self.variables[self.value].type
        elif self.type == "SUBSCRIPTION":
            return self.type_in(self.left)
        else:
            raise SyntaxError("Unknown assignment target")

    def optimize_constants(self, constants, used_constants=set()):
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
            if isinstance(v, (AstNode, AstList, AstDict)):
                v.optimize_constants(constants, used_constants)
        self._optimize_constants()
    
    def _optimize_constants(self):
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
        if self["type"] in ("!", "NOT"):
            if self["value"]["type"] == "NUMERIC":
                value = not self["value"]["value"]
                self.clear()
                self["type"] = "NUMERIC"
                self.value = value
        if self["type"] == "~":
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

class AstList(NodeList):
    def optimize_constants(self, constants, used_constants=set()):
        for v in self:
            if isinstance(v, (AstNode, AstList, AstDict)):
                v.optimize_constants(constants, used_constants)

class AstDict(NodeDict):
    def optimize_constants(self, constants, used_constants=set()):
        for k, v in self.items():
            if isinstance(v, (AstNode, AstList, AstDict)):
                v.optimize_constants(constants, used_constants)

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

        self._constants = {}
        self._shared = AstList()
        self._local = [AstList()]
        self.ast = AstList()

        self.shared = {}
        self.local = {}

    @property
    def token(self):
        return self.tokens[self.pos][0]

    @property
    def value(self):
        return self.tokens[self.pos][1]

    def advance(self):
        self.pos +=  1

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
                    ident = self.value
                    self.advance()
                    if self.token != "=":
                        raise SyntaxError("Malformed CONST")
                    self.advance()
                    self.constants[ident] = AstNode(type="CONST", ident=ident, value=self.expr())
                else:
                    raise SyntaxError()
            else:
                self.ast.append(self.stmt())

        # OPTIMIZE CONSTANTS
        for k, v in self.constants.items():
            v.optimize_constants(self.constants)

        self.locals[0].optimize_constants(self.constants)

        self.ast.optimize_constants(self.constants)
        self.globals.optimize_constants(self.constants)

        # COMPILE SHARED NAMES
        for node in self._shared:
            if node.type == "DEF_VAR":
                for cnode in self.compile_variable(node):
                    self.shared[cnode.ident] = cnode

            if node.type == "DEF_FUN":
                self.shared[cnode.ident] = cnode

        # COMPILE MAIN'S NAMES
        self.names = self.shared.copy()

        for node in self._local:
            for cnode in self.compile_variable(node):
                self.names[cnode.ident] = cnode
            
        # COMPILE FUNCTIONS' NAMES
        for node in self.names:
            if node.type == "DEF_FUN":
                node.names = self.shared.copy()
                for inode in node.locals:
                    node.names[inode.ident] = inode
                

        return self.globals, self.ast, self.locals[0]

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
        if self.token in ("BYTE", "WORD"):
            token = self.token
            self.advance()
            if self.token != "IDENT":
                raise SyntaxError("funtion or variable without name")
            name = self.value
            self.advance()
            if self.token == "(":
                # function definition
                node = AstNode(type="DEF_FUN", return_type=token, name=name, args=AstList())

                self.advance()
                while self.token != ")":
                    if self.token not in ("BYTE", "WORD"):
                        raise SyntaxError("Unkown function argument type: %s" % self.token)
                    arg_type = self.token
                    self.advance()
                    if self.token != "IDENT":
                        raise SyntaxError("No function argument name")
                    arg_name = self.value
                    self.advance()
                    if self.token == "[":
                        self.advance()
                        arg_size = self.expr()
                        if self.token != "]":
                            raise SyntaxError("argument missing ]")
                        self.advance()
                    else:
                        arg_size = AstNode(type="NUMERIC", value=1)
                    node.args.append(AstNode(type=arg_type, name=arg_name, size=arg_size))
                    if self.token == ",":
                        self.advance()

                self.locals.append(AstList())
                node.body = self.suite()
                node.locals = self.locals.pop()
                
                self.globals.append(node)
                return AstNode(type="PASS")

            else:
                # variable definition
                self.pos -= 1
                self.scope[-1].append(AstNode(type="DEF_VAR", return_type=token, value=self.simple_assignment_list()))
                return AstNode(type="PASS")

        elif self.token == "SHARED":
            self.advance()
            if self.token in ("BYTE", "WORD"):
                token = self.token
                self.advance()
                self.globals.append(AstNode(type="DEF_VAR", return_type=token, value=self.simple_assignment_list()))
                return AstNode(type="PASS")
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

        elif self.token == "WHILE":
            self.advance()

            expr = self.expr()
            if self.token != ":":
                raise SyntaxError("No : after while expression")
            self.advance()
            if self.token == "INDENT":
                return AstNode(type="WHILE", expr=expr, suite=self.suite())
            else:
                return AstNode(type="WHILE", expr_type=expr_type, expr=expr, suite=self.stmt())

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
        left = self.unary_minusplusnot()

        while self.token in ("*", "/"):
            op = self.token
            self.advance()
            right = self.unary_minusplus()

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
                    left = AstNode(type="CALL", ident=left, args=AstList())
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
                node = AstNode(type="ARRAY", value=AstList())
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
