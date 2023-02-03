DEFAULT_STRING_CAPACITY = 32
from nodes import *

def propagate_type(node, names):
    if isinstance(node, AstList):
        for n in node:
            propagate_type(n, names)
        node.return_type = "NA"

    else:
        for k, v in node.items():
            if isinstance(v, (AstNode, AstList, AstDict)):
                propagate_type(v, names)

        if node.type == "IDENT":
            node.return_type = names[node.value].return_type
        
        elif node.type == "LITERAL":
            node.return_type = "STRING"

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
            node.return_type = "NA"
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
            node.return_type = "NA" # names["_RETURN_"].return_type
            expr_type = propagate_type(node.value, names)
            if expr_type != node.return_type:
                node.value = AstNode(type="C%s" % node.return_type, return_type=node.return_type, value=node.value)
        
        elif node.type == "IF":
            node.return_type = "NA"
            for index, branch in enumerate(node.branches.copy()):
                branch_type = propagate_type(branch, names)
                if branch_type != "BYTE":
                    node.branches[index] = AstNode(type="CBYTE", return_type="BYTE", value=node.branches[index])
            if "_else" in node:
                propagate_type(node._else, names)

        elif node.type == "ELSE_IF":
            node.return_type = "NA"
            propagate_type(node.expr, names)
            propagate_type(node.body, names)

        elif node.type == "ELSE":
            node.return_type = "NA"
            propagate_type(node.body, names)

        elif node.type == "PEEK":
            node.return_type = "BYTE"
            propagate_type(node.addr, names)

        elif node.type == "POKE":
            node.return_type = "NA"
            propagate_type(node.addr, names)
            propagate_type(node.value, names)

        elif node.type == "WHILE":
            node.return_type = "NA"
            propagate_type(node.expr, names)
            propagate_type(node.body, names)

        elif node.type in ("START", "END"):
            node.return_type = "NA"

        else:
            raise NotImplementedError()
            node.return_type = "???"

    return node.return_type

def process_variables(namespace):
    for node in namespace.values():
        if node.type == "AST_ARG":
            process_ast_arg(node)
        if node.type == "AST_VAR":
            process_ast_var(node)
        if node.type == "DEF_FUN":
            process_variables(node.local)
            node.capacity = node.capacity.value

def process_ast_var(node):
    ast = node.ast    
    if ast.type == "=":
        left, right = ast.left, ast.right

        assert left.type == "IDENT"

        if node.return_type == "STRING":
            if right.type == "LIST":
                # string s=["abc, "foo"] or string[2] s=["abc, "foo"]
                capacity = node.capacity.value or max((len(text.value) for text in right))

                node.type = "DEF_VAR"
                node.size = len(right)
                node.index_type = "BYTE" if node.size <= 255 else "WORD"
                node.capacity = capacity
                node.initializer = []
                for literal in right:
                    length = min(capacity, len(literal.value))
                    data = (length, literal.value[:length].ljust(capacity, " "))
                    node.initializer.append(data)
                del node.ast

            elif right.type == "LITERAL":
                # string s="foo" or string[2] s="foo"
                capacity = node.capacity.value or len(right.value)

                node.type = "DEF_VAR"
                node.size = 1
                node.index_type = "BYTE" if node.size <= 255 else "WORD"
                node.capacity = capacity
                length = min(capacity, len(right.value))
                node.initializer = [(length, right.value[:length].ljust(capacity, " "))]

                del node.ast

            else:
                raise NotImplementedError()

        elif node.return_type == "BYTE":
            if right.type == "LIST":
                # byte b=[1, 2]

                node.type = "DEF_VAR"
                node.size = len(right)
                node.index_type = "BYTE" if node.size <= 255 else "WORD"
                node.capacity = 0
                node.initializer = [data.value for data in right]

                del node.ast

            elif right.type == "NUMERIC":
                # byte b=2

                node.type = "DEF_VAR"
                node.size = 1
                node.index_type = "NA"
                node.capacity = 0
                node.initializer = [right.value]

                del node.ast

            else:
                raise NotImplementedError()

    elif ast.type == "SUBSCRIPTION":
        left, right = ast.left, ast.right

        assert left.type == "IDENT"

        if node.return_type == "STRING":
            # string s[5] or string[4] s[5]
            size = right.value
            capacity = node.capacity.value or DEFAULT_STRING_CAPACITY

            node.type = "DEF_VAR"
            node.size = size
            node.index_type = "BYTE" if node.size <= 255 else "WORD"
            node.capacity = capacity
            node.initializer = [(0, " " * node.capacity) for _ in range(size)]

            del node.ast

        elif node.return_type == "BYTE":
            # byte b[5]
            size = right.value
            
            node.type = "DEF_VAR"
            node.size = size
            node.index_type = "BYTE" if node.size <= 255 else "WORD"
            node.capacity = 0
            node.initializer = [0] * size

            del node.ast

        else:
            raise NotImplementedError()

    elif ast.type == "IDENT":
        if node.return_type == "STRING":
            # string s or string[4] s
            capacity = node.capacity.value or DEFAULT_STRING_CAPACITY

            node.type = "DEF_VAR"
            node.size = 1
            node.index_type = "BYTE"
            node.capacity = capacity
            node.initializer = [(0, " " * node.capacity)]

            del node.ast

        elif node.return_type == "BYTE":
            # byte b
            
            node.type = "DEF_VAR"
            node.size = 1
            node.index_type = "BYTE"
            node.capacity = 0
            node.initializer = [0]

            del node.ast

        else:
            raise NotImplementedError()
    else:
        raise NotImplementedError()

def process_ast_arg(node):    
    if node.return_type == "STRING":
        # string s or string[3] s

        capacity = node.capacity.value or DEFAULT_STRING_CAPACITY

        node.type = "DEF_VAR"
        node.capacity = capacity
        node.initializer = [(0, " " * node.capacity)]

    elif node.return_type == "BYTE":
        # byte b

        node.type = "DEF_VAR"
        node.size = 1
        node.index_type = "NA"
        node.capacity = 0
        node.initializer = [0]

    else:
        raise NotImplementedError()
