from box import Box


class Node(Box):
    def __str__(self):
        result = []
        if "node" in self:
            result.append(self.node)
        if "name" in self:
            result.append(self.name)
        return ", ".join(result)

    def __repr__(self):
        return self.__str__()

    def clear(self):
        for key in list(self.keys()):
            self[key] = None
            del self[key]

    def tree(self, indent=0):
        main = " " * indent
        sub = " " * (indent + 2)

        subtree = []
        for key, value in self.items():
            if not key.startswith("_"):
                if isinstance(value, (Node, NodeList)):
                    subtree.append("%s%s:\n%s" % (sub, key, value.tree(indent + 2)))
                else:
                    subtree.append("%s%s: %s" % (sub, key, value))

        return "%s%s\n%s" % (sub, self.__class__.__name__, "\n".join(subtree))


class NodeList(list):
    node = "LIST"

    def __str__(self):
        return "%s[%s]" % (self.__class__.__name__, len(self))

    def __repr__(self):
        return self.__str__()

    def tree(self, indent=0):
        main = " " * indent
        sub = " " * (indent + 2)

        subtree = []
        for index, value in enumerate(self):
            if isinstance(value, (Node, NodeList)):
                subtree.append("%s%3d:\n%s" % (sub, index, value.tree(indent + 4)))
            else:
                subtree.append("%s%3d: %s" % (sub, index, value))

        return "%s%s\n%s" % (sub, self.__class__.__name__, "\n".join(subtree))


class AstNode(Node):
    def type_in(self):
        if self.type == "IDENT":
            return self.variables[self.value].type
        elif self.type == "SUBSCRIPTION":
            return self.left.type_in()
        else:
            raise SyntaxError("Unknown assignment target")

    def substitute_constants(self, constants):
        if "node" in self and self.node == "IDENT":
            if self.value in constants:
                value = self.value
                self.clear()
                self.update(constants[value].value)
                self.substitute_constants(constants)

        for value in self.values():
            if isinstance(value, (AstNode, AstList)):
                value.substitute_constants(constants)

    def fold_constants(self):
        for v in self.values():
            if isinstance(v, (AstNode, AstList)):
                v.fold_constants()
        self._fold_constants()

    def _fold_constants(self):
        if "node" not in self:
            return
        if self.node in ["+", "-", "*", "%", "**", "&", "|", "^", "<<", ">>", "==", "<", ">", ">=", "<=", "!=", "<>"]:
            op = self.node
            if self.left.node == "NUMERIC" and self.right.node == "NUMERIC":
                value = eval('self.left.value %s self.right.value' % op)
                self.clear()
                self.node = "NUMERIC"
                self.value = value
        if self.node == "/":
            if self.left.node == "NUMERIC" and self.right.node == "NUMERIC":
                value = self.left.value // self.right.value
                self.clear()
                self.node = "NUMERIC"
                self.value = value
        if self.node in ("!", "NOT"):
            if self.value.node == "NUMERIC":
                value = not self.value.value
                self.clear()
                self.node = "NUMERIC"
                self.value = value
        if self.node == "~":
            if self.value.node == "NUMERIC":
                value = ~self.value.value
                self.clear()
                self.node = "NUMERIC"
                self.value = value
        if self.node == "UMINUS":
            if self.value.node == "NUMERIC":
                value = -self.value.value
                self.clear()
                self.node = "NUMERIC"
                self.value = value
        # ">>>", ">><", "<<>" cannot be optimised without knowing the width

    def fix_type_propagation(self, names):
        if isinstance(self, AstList):
            for n in self:
                n.fix_type_propagation(names)
            return

#        for node in self.values():
#            if isinstance(node, (AstNode, AstList)):
#                node.fix_type_propagation(names)

        if self.node == "LITERAL":
            self.type = "STRING"

        elif self.node == "NUMERIC":
            if 0 <= self.value <= 255:
                self.type = "BYTE"
            elif 0 <= self.value <= 65535:
                self.type = "WORD"
            elif -32768 <= self.value <= 32767:
                self.type = "INT"
            else:
                self.type = "LONG"

        elif self.node == "REFERENCE":
            reference = self
            reference_module = None
            reference_names = names
            while reference.node == "REFERENCE":
                reference_module = reference_names[reference.left.value]
                assert reference_module.node == "MODULE", "Left side of reference must be a module"
                reference_names = reference_module.get_names()
                reference = reference.right

            self.clear()
            self.update(reference)
            self.ns = reference_module
            self.fix_type_propagation(names)

        elif self.node == "IDENT":
            left_names = self.ns.get_names() if "ns" in self else names
            assert self.value in left_names, "'%s' not found in %s" % (self.value, self.ns.filename)
            self.type = left_names[self.value].type

        elif self.node == "SUBSCRIPTION":
            left_names = self.ns.get_names() if "ns" in self else names
            self.type = self.left.fix_type_propagation(left_names)

            assert self.type in ("STRING", "BYTE", "WORD", "INT", "LONG")

            self.index_type = self.right.fix_type_propagation(names)
            if self.type == "STRING":
                if self.left.node == "IDENT":
                    cvar = left_names[self.left.value]
                    if cvar.size > 1:
                        # array of strings
                        if self.index_type != "WORD":
                            self.index_type = "WORD"
                            self.right = AstNode(node="CWORD", type="WORD", value=self.right)
                    else:
                        # string ident
                        if self.index_type != "BYTE":
                            self.index_type = "BYTE"
                            self.right = AstNode(node="CBYTE", type="BYTE", value=self.right)
                else:
                    # string, but not ident
                    if self.index_type != "BYTE":
                        self.right = AstNode(node="CBYTE", type="BYTE", value=self.right)

        elif self.node == "CALL":
            left_names = self.ns.get_names() if "ns" in self else names
            self.type = self.left.fix_type_propagation(left_names)

            self.type = left_names[self.name].type
            for n in range(len(self.args)):
                arg_expr, arg_def = self.args[n], left_names[self.name].args[n]
                arg_type = arg_expr.fix_type_propagation(names)
                if arg_type != arg_def.type:
                    self.args[n] = AstNode(node="C%s" % arg_def.type, type=arg_def.type, value=self.args[n])

        elif self.node in ("CSTRING", "CINT", "CBYTE", "CWORD", "CLONG"):
            assert self.type == self.node[1:]

        elif self.node in ("+", "-", "*", "/", "%", "&", "|", "^"):
            left = self.left.fix_type_propagation(names)
            right = self.right.fix_type_propagation(names)

            if "STRING" in (left, right):
                if left != "STRING":
                    self.left = AstNode(node="CSTRING", type="STRING", value=self.left)
                if right != "STRING":
                    self.right = AstNode(node="CSTRING", type="STRING", value=self.right)
                self.type = "STRING"
            elif "LONG" in (left, right):
                if left != "LONG":
                    self.left = AstNode(node="CLONG", type="LONG", value=self.left)
                if right != "LONG":
                    self.right = AstNode(node="CLONG", type="LONG", value=self.right)
                self.type = "LONG"
            elif "INT" in (left, right) and "WORD" in (left, right):
                if left != "LONG":
                    self.left = AstNode(node="CLONG", type="LONG", value=self.left)
                if right != "LONG":
                    self.right = AstNode(node="CLONG", type="LONG", value=self.right)
                self.type = "LONG"
            elif "WORD" in (left, right):
                if left != "WORD":
                    self.left = AstNode(node="CWORD", type="WORD", value=self.left)
                if right != "WORD":
                    self.right = AstNode(node="CWORD", type="WORD", value=self.right)
                self.type = "WORD"
            elif "INT" in (left, right):
                if left != "INT":
                    self.left = AstNode(node="CINT", type="INT", value=self.left)
                if right != "INT":
                    self.right = AstNode(node="CINT", type="INT", value=self.right)
                self.type = "INT"
            else:
                self.type = "BYTE"

        elif self.node in ("=", "+=", "-=", "*=", "/=", "&=", "|=", "^=", "!=", "<<=", ">>=", ">>>=", ">><=", "<<>="):
            self.type = "NA"
            self.left.fix_type_propagation(names)
            self.right.fix_type_propagation(names)

        elif self.node in ("==", "<", ">", ">=", "<=", "!=", "<>"):
            self.type = "BYTE"
            left = self.left.fix_type_propagation(names)
            right = self.right.fix_type_propagation(names)

            if "LONG" in (left, right):
                self.child_type = "LONG"
                if left != "LONG":
                    self.left = AstNode(node="CLONG", type="LONG", value=self.left)
                if right != "LONG":
                    self.right = AstNode(node="CLONG", type="LONG", value=self.right)
            elif "INT" in (left, right) and "WORD" in (left, right):
                self.child_type = "LONG"
                if left != "LONG":
                    self.left = AstNode(node="CLONG", type="LONG", value=self.left)
                if right != "LONG":
                    self.right = AstNode(node="CLONG", type="LONG", value=self.right)
            elif "WORD" in (left, right):
                self.child_type = "WORD"
                if left != "WORD":
                    self.left = AstNode(node="CWORD", type="WORD", value=self.left)
                if right != "WORD":
                    self.right = AstNode(node="CWORD", type="WORD", value=self.right)
            elif "INT" in (left, right):
                self.child_type = "INT"
                if left != "INT":
                    self.left = AstNode(node="CINT", type="INT", value=self.left)
                if right != "INT":
                    self.right = AstNode(node="CINT", type="INT", value=self.right)
            else:
                self.child_type = "BYTE"

        elif self.node in ("**", "<<", ">>", ">>>", ">><", "<<>"):
            self.type = self.left.fix_type_propagation(names)
            self.right.fix_type_propagation(names)

        elif self.node in ("UMINUS", "UNOT"):
            self.type = self.value.fix_type_propagation(names)

        elif self.node == "NOT":
            self.type = "BYTE"
            self.value.fix_type_propagation(names)

        elif self.node in ["==", "<", ">", ">=", "<=", "!=", "<>", "AND", "OR"]:
            self.type = "BYTE"
            self.left.fix_type_propagation(names)
            self.right.fix_type_propagation(names)

        elif self.node == "RETURN":
            self.type = "NA"  # names["_RETURN_"].type
            expr_type = self.value.fix_type_propagation(names)
            func_type = names["__SELF__"].type
            if expr_type != func_type:
                self.value = AstNode(node="C%s" % func_type, type=func_type, value=self.value)

        elif self.node == "IF":
            self.type = "NA"
            for index, branch in enumerate(self.branches.copy()):
                branch_type = branch.fix_type_propagation(names)
                if branch_type != "BYTE":
                    self.branches[index] = AstNode(node="CBYTE", type="BYTE", value=self.branches[index])
            if "_else" in self:
                self._else.fix_type_propagation(names)

        elif self.node == "ELSE_IF":
            self.type = "NA"
            self.expr.fix_type_propagation(names)
            if self.value.type != "BYTE":
                self.value = AstNode(node="CBYTE", type="BYTE", value=self.addr)
            self.body.fix_type_propagation(names)

        elif self.node == "ELSE":
            self.type = "NA"
            self.body.fix_type_propagation(names)

        elif self.node == "ASM":
            self.type = "NA"

        elif self.node == "PEEK":
            self.type = "BYTE"
            self.addr.fix_type_propagation(names)
            if self.addr.type not in ("BYTE", "WORD"):
                self.addr = AstNode(node="CWORD", type="WORD", value=self.addr)

        elif self.node == "POKE":
            self.type = "NA"
            self.addr.fix_type_propagation(names)
            if self.addr.type not in ("BYTE", "WORD"):
                self.addr = AstNode(node="CWORD", type="WORD", value=self.addr)
            self.value.fix_type_propagation(names)
            if self.value.type != "BYTE":
                self.value = AstNode(node="CBYTE", type="BYTE", value=self.addr)

        elif self.node == "DEBUG":
            self.type = "NA"
            self.value.fix_type_propagation(names)
            if self.value.type != "STRING":
                self.value = AstNode(node="CSTRING", type="STRING", value=self.value)

        elif self.node == "WHILE":
            self.type = "NA"
            self.expr.fix_type_propagation(names)
            if self.expr.type != "BYTE":
                self.expr = AstNode(node="CBYTE", type="BYTE", value=self.expr)
            self.bodyfix_type_propagation(names)

        elif self.node == "DATASET":
            self.value.fix_type_propagation(names)

        elif self.node == "DATA":
            self.value.fix_type_propagation(names)

        elif self.node in ("START", "END", "LABEL", "GOTO", "ORIGIN"):
            self.type = "NA"

        else:
            raise NotImplementedError()

        return self.type


class AstList(NodeList):
    def fold_constants(self):
        for v in self:
            if isinstance(v, (AstNode, AstList)):
                v.fold_constants()

    def substitute_constants(self, constants):
        for v in self:
            if isinstance(v, (AstNode, AstList)):
                v.substitute_constants(constants)

    def fix_type_propagation(self, names):
        for v in self:
            if isinstance(v, (AstNode, AstList)):
                v.fix_type_propagation(names)

    def init_variables(self):
        for v in self:
            if isinstance(v, (AstNode, AstList)):
                v.init_variables()
