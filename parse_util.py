import hashlib
from nodes import *

DEFAULT_STRING_CAPACITY = 32


def process_variables(namespace):
    for var in namespace.values():
        if var.node == "FUNCTION_ARGUMENT":
            process_function_argument(var)
        elif var.node == "VARIABLE":
            process_variable(var)


def process_variable(node):
    ast = node.ast
    if ast.node == "=":
        left, right = ast.left, ast.right

        assert left.node == "IDENT"

        if node.type == "STRING":
            if right.node == "LIST":
                # string s=["abc, "foo"] or string[2] s=["abc, "foo"]
                capacity = node.capacity.value or max((len(text.value) for text in right))

                node.node = "VARIABLE"
                node.size = len(right)
                node.index_type = "BYTE" if node.size <= 255 else "WORD"
                node.capacity = capacity
                node.initializer = []
                for literal in right:
                    length = min(capacity, len(literal.value))
                    data = (length, literal.value[:length].ljust(capacity, " "))
                    node.initializer.append(data)

            elif right.node == "LITERAL":
                # string s="foo" or string[2] s="foo"
                capacity = node.capacity.value or len(right.value)

                node.node = "VARIABLE"
                node.size = 1
                node.index_type = "BYTE" if node.size <= 255 else "WORD"
                node.capacity = capacity
                length = min(capacity, len(right.value))
                node.initializer = [(length, right.value[:length].ljust(capacity, " "))]

            else:
                raise NotImplementedError()

        elif node.type == "BYTE":
            if right.node == "LIST":
                # byte b=[1, 2]

                node.node = "VARIABLE"
                node.size = len(right)
                node.index_type = "BYTE" if node.size <= 255 else "WORD"
                node.capacity = 0
                node.initializer = [data.value for data in right]

            elif right.node == "NUMERIC":
                # byte b=2

                # node.node = "VARIABLE"
                node.size = 1
                node.index_type = "NA"
                node.capacity = 0
                node.initializer = [right.value]

            else:
                raise NotImplementedError()

    elif ast.node == "SUBSCRIPTION":
        left, right = ast.left, ast.right

        assert left.node == "IDENT"

        if node.type == "STRING":
            # string s[5] or string[4] s[5]
            size = right.value
            capacity = node.capacity.value or DEFAULT_STRING_CAPACITY

            node.node = "VARIABLE"
            node.size = size
            node.index_type = "BYTE" if node.size <= 255 else "WORD"
            node.capacity = capacity
            node.initializer = [(0, " " * node.capacity) for _ in range(size)]

            del node.ast

        elif node.type == "BYTE":
            # byte b[5]
            size = right.value

            node.node = "VARIABLE"
            node.size = size
            node.index_type = "BYTE" if node.size <= 255 else "WORD"
            node.capacity = 0
            node.initializer = [0] * size

            del node.ast

        else:
            raise NotImplementedError()

    elif ast.node == "IDENT":
        if node.type == "STRING":
            # string s or string[4] s
            capacity = node.capacity.value or DEFAULT_STRING_CAPACITY

            node.node = "VARIABLE"
            node.size = 1
            node.index_type = "BYTE"
            node.capacity = capacity
            node.initializer = [(0, " " * node.capacity)]

            del node.ast

        elif node.type == "BYTE":
            # byte b

            node.node = "VARIABLE"
            node.size = 1
            node.index_type = "BYTE"
            node.capacity = 0
            node.initializer = [0]

            del node.ast

        else:
            raise NotImplementedError()
    else:
        raise NotImplementedError()


def process_function_argument(node):
    if node.type == "STRING":
        # string s or string[3] s

        capacity = node.capacity.value or DEFAULT_STRING_CAPACITY

        node.node = "VARIABLE"
        node.capacity = capacity
        node.initializer = [(0, " " * node.capacity)]

    elif node.type == "BYTE":
        # byte b

        node.node = "VARIABLE"
        node.size = 1
        node.index_type = "NA"
        node.capacity = 0
        node.initializer = [0]

    elif node.type == "WORD":
        # word w

        node.node = "VARIABLE"
        node.size = 1
        node.index_type = "NA"
        node.capacity = 0
        node.initializer = [0]

    else:
        raise NotImplementedError()
