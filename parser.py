from dotwiz import DotWiz

from lexer import Lexer, operators_arithmetic, operators_comparison
from nodes import *


class AstNode(Node):
    def type_in(self):
        if self.type == "IDENT":
            return self.variables[self.value].type
        elif self.type == "SUBSCRIPTION":
            return self.type_in(self.left)
        else:
            raise SyntaxError("Unknown assignment target")

    def optimize_constants(self, constants, used_constants=set()):
        if self.type == "CONST":
            if self.name in used_constants:
                raise SyntaxError("Circular CONST definition: %s" % self.name)
            used_constants = set(list(used_constants) + [self.name])
        
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

def propagate_type(node, names):
    if isinstance(node, AstList):
        for n in node:
            propagate_type(n, names)
        node.return_type = "???"

    else:
        for k, v in node.items():
            if isinstance(v, (AstNode, AstList, AstDict)):
                propagate_type(v, names)

        if node.type == "IDENT":
            node.return_type = names[node.value].return_type
        
        elif node.type == "NUMERIC":
            if 0 <= node.value <= 255:
                node.return_type = "BYTE"
            elif 0 <= node.value <= 65535:
                node.return_type = "WORD"
            elif -32768 <= node.value <= 32767:
                node.return_type = "INT"
            else:
                node.return_type = "LONG"

        elif node.type == "SUBSCRIPTION":
            node.return_type = propagate_type(node.left, names)
            node.index_type = propagate_type(node.right, names)

        elif node.type in ("+", "-", "*", "/", "%", "&", "|", "^"):
            left = propagate_type(node.left, names)
            right = propagate_type(node.right, names)

            if "LONG" in (left, right):
                if left != "LONG":
                    node.left = AstNode(type="CLONG", return_type="LONG", value=node.left)
                if right != "LONG":
                    node.right = AstNode(type="CLONG", return_type="LONG", value=node.right)
                node.return_type = "LONG"
            elif "INT" in (left, right) and "WORD" in (left, right):
                if left != "LONG":
                    node.left = AstNode(type="CLONG", return_type="LONG", value=node.left)
                if right != "LONG":
                    node.right = AstNode(type="CLONG", return_type="LONG", value=node.right)
                node.return_type = "LONG"
            elif "WORD" in (left, right):
                if left != "WORD":
                    node.left = AstNode(type="CWORD", return_type="WORD", value=node.left)
                if right != "WORD":
                    node.right = AstNode(type="CWORD", return_type="WORD", value=node.right)
                node.return_type = "WORD"
            elif "INT" in (left, right):
                if left != "INT":
                    node.left = AstNode(type="CINT", return_type="INT", value=node.left)
                if right != "INT":
                    node.right = AstNode(type="CINT", return_type="INT", value=node.right)
                node.return_type = "INT"
            else:
                node.return_type = "BYTE"
        
        elif node.type in ("=", "+=", "-=", "*=", "/=", "&=", "|=", "^=", "!=", "<<=", ">>=", ">>>=", ">><=", "<<>="):
            node.return_type = "???"
            propagate_type(node.left, names)
            propagate_type(node.right, names)

        elif node.type in ("==", "<", ">", ">=", "<=", "!=", "<>"):
            node.return_type = "BYTE"
            left = propagate_type(node.left, names)
            right = propagate_type(node.right, names)

            if "LONG" in (left, right):
                node.child_type = "LONG"
                if left != "LONG":
                    node.left = AstNode(type="CLONG", return_type="LONG", value=node.left)
                if right != "LONG":
                    node.right = AstNode(type="CLONG", return_type="LONG", value=node.right)
            elif "INT" in (left, right) and "WORD" in (left, right):
                node.child_type = "LONG"
                if left != "LONG":
                    node.left = AstNode(type="CLONG", return_type="LONG", value=node.left)
                if right != "LONG":
                    node.right = AstNode(type="CLONG", return_type="LONG", value=node.right)
            elif "WORD" in (left, right):
                node.child_type = "WORD"
                if left != "WORD":
                    node.left = AstNode(type="CWORD", return_type="WORD", value=node.left)
                if right != "WORD":
                    node.right = AstNode(type="CWORD", return_type="WORD", value=node.right)
            elif "INT" in (left, right):
                node.child_type = "INT"
                if left != "INT":
                    node.left = AstNode(type="CINT", return_type="INT", value=node.left)
                if right != "INT":
                    node.right = AstNode(type="CINT", return_type="INT", value=node.right)
            else:
                node.child_type = "BYTE"
        
        elif node.type in ("**", "<<", ">>", ">>>", ">><", "<<>"):
            node.return_type = propagate_type(node.left, names)
            propagate_type(node.right, names)

        elif node.type in ("UMINUS", "UNOT"):
            node.return_type = propagate_type(node.value, names)

        elif node.type == "NOT":
            node.return_type = "BYTE"
            propagate_type(node.value, names)

        elif node.type in operators_comparison + ["AND", "OR"]:
            node.return_type = "BYTE"
            propagate_type(node.left, names)
            propagate_type(node.right, names)

        elif node.type == "CALL":
            node.return_type =  names[node.name].return_type
            for n in range(len(node.args)):
                arg_expr, arg_def = node.args[n], names[node.name].args[n]
                arg_type = propagate_type(arg_expr, names)
                if arg_type != arg_def.return_type:
                    node.args[n] = AstNode(type="C%s" % arg_def.return_type, return_type=arg_def.return_type, value=node.args[n])

        elif node.type == "RETURN":
            node.return_type = "???" # names["_RETURN_"].return_type
            expr_type = propagate_type(node.value, names)
            if expr_type != node.return_type:
                node.value = AstNode(type="C%s" % node.return_type, return_type=node.return_type, value=node.expr)
        
        elif node.type == "IF":
            node.return_type = "???"
            for index, branch in enumerate(node.branches.copy()):
                branch_type = propagate_type(branch, names)
                if branch_type != "BYTE":
                    node.branches[index] = AstNode(type="CBYTE", return_type="BYTE", value=node.branches[index])
            if "_else" in node:
                propagate_type(node._else, names)

        elif node.type == "ELSE_IF":
            node.return_type = "???"
            propagate_type(node.expr, names)
            propagate_type(node.body, names)

        elif node.type == "ELSE":
            node.return_type = "???"
            propagate_type(node.body, names)

        elif node.type == "PEEK":
            node.return_type = "BYTE"
            propagate_type(node.addr, names)

        elif node.type == "POKE":
            node.return_type = "???"
            propagate_type(node.addr, names)
            propagate_type(node.value, names)

        elif node.type == "WHILE":
            node.return_type = "???"
            propagate_type(node.expr, names)
            propagate_type(node.body, names)

        else:
            node.return_type = "???"

    return node.return_type

def create_variables(node):
    for i in node.value:
    #for i in node:
        if i.type == "IDENT":
            yield AstNode(type="DEF_VAR", name=i.value,
                return_type=node.return_type, 
                index_type="BYTE",
                size=1, 
                initializer=[0]
            )

        elif i.type == "SUBSCRIPTION":
            if i.left.type != "IDENT" or i.right.type != "NUMERIC":
                raise SyntaxError("Syntax error in definition")
            if not (0 <= i.right.value <= 65535):
                raise SyntaxError("Out of bounds")

            yield AstNode(type="DEF_VAR", name=i.value, size=i.right.value,
                return_type=node.return_type, 
                index_type="BYTE" if i.right.value <= 256 else "WORD", 
                initializer=[0] * i.right.value
            )

        elif i.type == "=":
            if i.left.type != "IDENT":
                raise SyntaxError("Syntax error in definition")

            if i.right.type == "NUMERIC":
                if node.return_type == "BYTE" and not (0 <= i.right.value <= 255):
                    raise SyntaxError("Out of bounds: %s" % (i.name))
                if node.return_type == "WORD" and not (0 <= i.right.value <= 65535):
                    raise SyntaxError("Out of bounds: %s" % (i.name))

                yield AstNode(type="DEF_VAR", name=i.left.value,
                    return_type=node.return_type, 
                    size=1, 
                    index_type="BYTE",
                    initializer=[i.right.value]
                )
            elif i.right.type == "LIST":
                values = [x.value for x in i.right.value]
                if node.return_type == "BYTE" and not all([0 <= v <= 255 for v in values]):
                    raise SyntaxError("Out of bounds: %s" % (i.left.value))
                if node.return_type == "WORD" and not all([0 <= v <= 65535 for v in values]):
                    raise SyntaxError("Out of bounds: %s" % (i.left.value))

                yield AstNode(type="DEF_VAR", name=i.left.value,
                    return_type=node.return_type,
                    size=len(values),
                    index_type="BYTE" if len(values) <= 256 else "WORD",
                    initializer=values,
                )
            else:
                raise SyntaxError("Unknown data type %s" % node.type)
        else:
            raise SyntaxError("Syntax error in definition")


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
                    name = self.value
                    self.advance()
                    if self.token != "=":
                        raise SyntaxError("Malformed CONST")
                    self.advance()
                    self.constants[name] = AstNode(type="CONST", name=name, value=self.expr())
                else:
                    raise SyntaxError()
            else:
                self.ast.append(self.stmt())

        # OPTIMIZE CONSTANTS
        for k, v in self.constants.items():
            v.optimize_constants(self.constants)

        self._shared.optimize_constants(self.constants)
        self._local[0].optimize_constants(self.constants)

        self.ast.optimize_constants(self.constants)

        # CONVERT LOCAL AND SHARED LISTS TO DICTS
        for node in self._shared:
            if node.type == "DEF_VAR":
                for var in create_variables(node):
                    self.shared[var.name] = var
            elif node.type == "DEF_FUN":
                self.shared[node.name] = node
                for var in node._local:
                    node.local[var.name] = var
        for node in self._local[0]:
            for var in create_variables(node):
                self.local[var.name] = var

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
        if self.token in ("BYTE", "WORD"):
            token = self.token
            self.advance()

            if self.next_token == "(":
                # function definition
                if self.token != "IDENT":
                    raise SyntaxError("funtion or variable without name")

                node = AstNode(type="DEF_FUN", name=self.value, 
                    return_type=token, 
                    size=1, 
                    args=AstList(),
                    _local=AstList(),
                    local=AstDict()
                )
                self.advance()
                # TODO array return values "byte print_char[10](byte c[2])"
                self.advance() # we already peek this "("

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
                        arg_size = 1
                    node.args.append(AstNode(type="DEF_VAR", 
                        return_type=arg_type, name=arg_name, size=arg_size,
                        index_type="BYTE", initializer=[0]
                    ))
                    if self.token == ",":
                        self.advance()

                self.advance() # )

                # CREATE NEW SCOPE
                self._local.append(AstList())
                # ADD RETURN VALUE TO LOCALS
                self._local[-1].append(AstNode(type="DEF_VAR", 
                    return_type=node.return_type, name="_RETURN_", 
                    size=node.size, initializer=[0]*node.size
                ))
                # ADD ARGS TO LOCALS
                for n in node.args:
                    self._local[-1].append(n)

                node.body = self.suite()
                # STORE LOCALS TO FUNCTION and REMOVE SCOPE
                node._local = self._local.pop()
                
                self._shared.append(node)
                return AstNode(type="PASS")

            else:
                # variable definition
                #for n in create_variables(AstNode(return_type=token, value=self.simple_assignment_list())):
                #    self.local[-1][n.name] = n
                self._local[-1].append(AstNode(type="DEF_VAR", return_type=token, value=self.simple_assignment_list()))
                return AstNode(type="PASS")

        elif self.token == "SHARED":
            self.advance()
            if self.token in ("BYTE", "WORD"):
                token = self.token
                self.advance()
                #for n in create_variables(AstNode(return_type=token, value=self.simple_assignment_list())):
                #    self.shared[n.name] = n
                self._shared.append(AstNode(type="DEF_VAR", return_type=token, value=self.simple_assignment_list()))
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
