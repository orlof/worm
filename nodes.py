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
            if isinstance(value, (Node, NodeList)):
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
            if isinstance(value, (Node, NodeList)):
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
                subtree.append("%s%3d:\n%s" % (sub, key, value.tree(indent + 4)))
            else:
                subtree.append("%s%3d: %s" % (sub, key, value))

        return "%s%s\n%s" % (sub, self.__class__.__name__, "\n".join(subtree))
