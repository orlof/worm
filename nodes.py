from dotwiz import DotWiz

class Node(DotWiz):
    def __str__(self):
        return "%s[%s]" % (self.__class__.__name__, len(self))

    def __repr__(self):
        return "%s[%s]" % (self.__class__.__name__, len(self))

    def tree(self, indent=0):
        main = " " * indent
        sub = " " * (indent + 2)

        subtree = []
        for key, value in self.items():
            if not key.startswith("_"):
                if isinstance(value, (Node, NodeList, NodeDict)):
                    subtree.append("%s%s:\n%s" % (sub, key, value.tree(indent + 2)))
                else:
                    subtree.append("%s%s: %s" % (sub, key, value))

        return "%s%s\n%s" % (sub, self.__class__.__name__, "\n".join(subtree))

class NodeList(list):
    type = "LIST"

    def __str__(self):
        return "%s[%s]" % (self.__class__.__name__, len(self))

    def __repr__(self):
        return "%s[%s]" % (self.__class__.__name__, len(self))

    def tree(self, indent=0):
        main = " " * indent
        sub = " " * (indent + 2)

        subtree = []
        for index, value in enumerate(self):
            if isinstance(value, (Node, NodeList, NodeDict)):
                subtree.append("%s%3d:\n%s" % (sub, index, value.tree(indent + 4)))
            else:
                subtree.append("%s%3d: %s" % (sub, index, value))

        return "%s%s\n%s" % (sub, self.__class__.__name__, "\n".join(subtree))

class NodeDict(DotWiz):
    type = "DICT"

    def __str__(self):
        return "%s[%s]" % (self.__class__.__name__, len(self))

    def __repr__(self):
        return "%s[%s]" % (self.__class__.__name__, len(self))

    def tree(self, indent=0):
        main = " " * indent
        sub = " " * (indent + 2)

        subtree = []
        for key, value in self.items():
            if isinstance(value, (Node, NodeList, NodeDict)):
                subtree.append("%s%s:\n%s" % (sub, key, value.tree(indent + 4)))
            else:
                subtree.append("%s%s: %s" % (sub, key, value))

        return "%s%s\n%s" % (sub, self.__class__.__name__, "\n".join(subtree))

class AstNode(Node):
    def type_in(self):
        if self.type == "IDENT":
            return self.variables[self.value].type
        elif self.type == "SUBSCRIPTION":
            return self.type_in(self.left)
        else:
            raise SyntaxError("Unknown assignment target")

    def optimize_constants(self):
        for k, v in self.copy().items():
            if isinstance(v, (AstNode, AstList, AstDict)):
                v.optimize_constants()
        self._optimize_constants()

    def _optimize_constants(self):
        if self["type"] in ["+", "-", "*", "%", "**", "&", "|", "^", "<<", ">>", "==", "<", ">", ">=", "<=", "!=", "<>"]:
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
    def optimize_constants(self):
        for v in self:
            if isinstance(v, (AstNode, AstList, AstDict)):
                v.optimize_constants()

    def init_variables(self):
        for v in self:
            if isinstance(v, (AstNode, AstList, AstDict)):
                v.init_variables()

class AstDict(NodeDict):
    def optimize_constants(self):
        for k, v in self.items():
            if isinstance(v, (AstNode, AstList, AstDict)):
                v.optimize_constants()

    def init_variables(self):
        for k, v in self.items():
            if isinstance(v, (AstNode, AstList, AstDict)):
                v.init_variables()


