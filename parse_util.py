DEFAULT_STRING_CAPACITY = 32
from nodes import *

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

        elif node.type in ["==", "<", ">", ">=", "<=", "!=", "<>", "AND", "OR"]:
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
            node.return_type = names["_RETURN_"].return_type
            expr_type = propagate_type(node.value, names)
            if expr_type != node.return_type:
                node.value = AstNode(type="C%s" % node.return_type, return_type=node.return_type, value=node.value)
        
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
        if i.type == "IDENT":
            if node.return_type == "STRING":
                # string[5] s
                node.capacity = node.capacity or DEFAULT_STRING_CAPACITY
                yield AstNode(type="DEF_VAR", name=i.value,
                    return_type=node.return_type, 
                    index_type="BYTE",
                    size=1, 
                    capacity=node.capacity,
                    initializer=[(0, " " * node.capacity)]
                )
            elif node.return_type == "BYTE":
                # byte b
                yield AstNode(type="DEF_VAR", name=i.value,
                    return_type=node.return_type, 
                    index_type="BYTE",
                    size=1, 
                    initializer=[0]
                )
            else:
                raise NotImplementedError()

        elif i.type == "SUBSCRIPTION":
            if i.left.type != "IDENT" or i.right.type != "NUMERIC":
                raise SyntaxError("Syntax error in definition")
            if not (0 <= i.right.value <= 65535):
                raise SyntaxError("Out of bounds")

            if node.return_type == "STRING":
                # string[5] s[3]
                node.capacity = node.capacity or DEFAULT_STRING_CAPACITY
                yield AstNode(type="DEF_VAR", name=i.left.value,
                    return_type=node.return_type, 
                    index_type="BYTE" if i.right.value <= 256 else "WORD", 
                    size=i.right.value,
                    capacity=node.capacity,
                    initializer=[(0, " " * node.capacity) for _ in range(i.right.value)]
                )
            elif node.return_type == "BYTE":
                # byte b[3]
                yield AstNode(type="DEF_VAR", name=i.left.value, 
                    return_type=node.return_type, 
                    index_type="BYTE" if i.right.value <= 256 else "WORD", 
                    size=i.right.value,
                    initializer=[0] * i.right.value
                )
            else:
                raise NotImplementedError()

        elif i.type == "=":
            if i.left.type != "IDENT":
                raise SyntaxError("Syntax error in definition")

            if node.return_type == "STRING":
                if i.right.type == "LITERAL":
                    # string s="a"
                    node.capacity = node.capacity or len(i.right.value)

                    value = i.right.value
                    length = min(len(value), node.capacity)
                    initializer = [(length, value[:length].ljust(node.capacity, " "))]
                    yield AstNode(type="DEF_VAR", name=i.left.value,
                        return_type=node.return_type, 
                        index_type="BYTE",
                        size=1, 
                        capacity=node.capacity,
                        initializer=initializer
                    )
                elif i.right.type == "LIST":
                    # string[5] s=["a", "b"]
                    for val in i.right:
                        if val.type != "LITERAL":
                            raise SyntaxError("only string literals allowed")


                    node.capacity = node.capacity or max([len(val.value) for val in i.right])

                    initializers = []
                    for val in i.right:
                        length = min(len(val.value), node.capacity)
                        initializer = [(length, val.value[:length].ljust(node.capacity, " "))]
                        initializers += initializer

                    yield AstNode(type="DEF_VAR", name=i.left.value,
                        return_type=node.return_type, 
                        index_type="BYTE" if len(i.right) <= 255 else "WORD",
                        size=len(i.right), 
                        capacity=node.capacity,
                        initializer=initializers
                    )
                else:
                    raise NotImplementedError()

            elif node.return_type == "BYTE":
                # byte b=1
                if i.right.type == "NUMERIC":
                    if not (0 <= i.right.value <= 255):
                        raise SyntaxError("Out of bounds: %s" % (i.name))

                    yield AstNode(type="DEF_VAR", name=i.left.value,
                        return_type=node.return_type, 
                        index_type="BYTE",
                        size=1, 
                        initializer=[i.right.value]
                    )
                elif i.right.type == "LIST":
                    # byte b=[1,2,3]
                    values = [x.value for x in i.right]
                    if not all([0 <= v <= 255 for v in values]):
                        raise SyntaxError("Out of bounds: %s" % (i.left.value))

                    yield AstNode(type="DEF_VAR", name=i.left.value,
                        return_type=node.return_type,
                        index_type="BYTE" if len(values) <= 256 else "WORD",
                        size=len(values),
                        initializer=values,
                    )
            else:
                raise SyntaxError("Unknown data type %s" % node.type)
        else:
            raise SyntaxError("Syntax error in definition")
