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

def process_variable_declaration(dec_node):
    assert dec_node.type == "VARIABLE_DECLARATION"
    
    for var_node in dec_node.value:
        if var_node.type == "IDENT":
            if dec_node.return_type == "STRING":
                # string[5] s
                dec_node.capacity = dec_node.capacity or DEFAULT_STRING_CAPACITY
                yield AstNode(type="DEF_VAR", name=var_node.value,
                    return_type=dec_node.return_type, 
                    index_type="BYTE",
                    size=1, 
                    capacity=dec_node.capacity,
                    initializer=[(0, " " * dec_node.capacity)]
                )
            elif dec_node.return_type == "BYTE":
                # byte b
                yield AstNode(type="DEF_VAR", name=var_node.value,
                    return_type=dec_node.return_type, 
                    index_type="BYTE",
                    size=1, 
                    initializer=[0]
                )
            else:
                raise NotImplementedError()

        elif var_node.type == "SUBSCRIPTION":
            assert var_node.left.type == "IDENT" and var_node.right.type == "NUMERIC":
            assert 0 <= var_node.right.value <= 65535

            if dec_node.return_type == "STRING":
                # string[5] s[3]
                dec_node.capacity = dec_node.capacity or DEFAULT_STRING_CAPACITY
                yield AstNode(type="DEF_VAR", name=var_node.left.value,
                    return_type=dec_node.return_type, 
                    index_type="BYTE" if var_node.right.value <= 256 else "WORD", 
                    size=var_node.right.value,
                    capacity=dec_node.capacity,
                    initializer=[(0, " " * dec_node.capacity) for _ in range(var_node.right.value)]
                )
            elif dec_node.return_type == "BYTE":
                # byte b[3]
                yield AstNode(type="DEF_VAR", name=var_node.left.value, 
                    return_type=dec_node.return_type, 
                    index_type="BYTE" if var_node.right.value <= 256 else "WORD", 
                    size=var_node.right.value,
                    initializer=[0] * var_node.right.value
                )
            else:
                raise NotImplementedError()

        elif var_node.type == "=":
            assert var_node.left.type == "IDENT"

            if dec_node.return_type == "STRING":
                if var_node.right.type == "LITERAL":
                    # string s="a"
                    dec_node.capacity = dec_node.capacity or len(var_node.right.value)

                    value = var_node.right.value
                    length = min(len(value), dec_node.capacity)
                    initializer = [(length, value[:length].ljust(dec_node.capacity, " "))]
                    yield AstNode(type="DEF_VAR", name=var_node.left.value,
                        return_type=dec_node.return_type, 
                        index_type="BYTE",
                        size=1, 
                        capacity=dec_node.capacity,
                        initializer=initializer
                    )
                elif var_node.right.type == "LIST":
                    # string[5] s=["a", "b"]
                    assert all([n.type == "LITERAL" for n in vr_node.right])

                    dec_node.capacity = dec_node.capacity or max([len(val.value) for val in var_node.right])

                    initializers = []
                    for val in var_node.right:
                        length = min(len(val.value), dec_node.capacity)
                        initializer = [(length, val.value[:length].ljust(dec_node.capacity, " "))]
                        initializers += initializer

                    yield AstNode(type="DEF_VAR", name=var_node.left.value,
                        return_type=dec_node.return_type, 
                        index_type="BYTE" if len(var_node.right) <= 255 else "WORD",
                        size=len(var_node.right), 
                        capacity=dec_node.capacity,
                        initializer=initializers
                    )
                else:
                    raise NotImplementedError()

            elif dec_node.return_type == "BYTE":
                # byte b=1
                if var_node.right.type == "NUMERIC":
                    assert 0 <= var_node.right.value <= 255

                    yield AstNode(type="DEF_VAR", name=var_node.left.value,
                        return_type=dec_node.return_type, 
                        index_type="BYTE",
                        size=1, 
                        initializer=[var_node.right.value]
                    )
                elif var_node.right.type == "LIST":
                    # byte b=[1,2,3]
                    values = [x.value for x in var_node.right]
                    assert all([0 <= v <= 255 for v in values])

                    yield AstNode(type="DEF_VAR", name=var_node.left.value,
                        return_type=dec_node.return_type,
                        index_type="BYTE" if len(values) <= 256 else "WORD",
                        size=len(values),
                        initializer=values,
                    )
            else:
                raise SyntaxError("Unknown data type %s" % dec_node.type)
        else:
            raise SyntaxError("Syntax error in definition")

