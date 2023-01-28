from dotwiz import DotWiz

from lexer import Lexer, operators_arithmetic, operators_comparison
from nodes import *
from parser import Parser, AstNode, AstList
from compiler import Compiler, CodeNode, CodeList

class Optimizer:
    def __init__(self, nodes):
        self.nodes = nodes

    def optimize(self):
        code_lines = list(self.line_iterator())

        # optimise lda #, pha, pla
        prev_line = ""
        for block in self.block_iterator(code_lines):
            stack = []
            for line in block:
                if line.src.startswith("    pha"):
                    stack.append((prev_line, line))
                if line.src.startswith("    pla") and stack:
                    lda_line, pha_line = stack.pop()

                    if lda_line.src.startswith("    lda #"):
                        line.src = "%s // optimized" % lda_line.src
                        lda_line.src = "    // %s" % lda_line.src[4:]
                        pha_line.src = "    // %s" % pha_line.src[4:]

                if not line.src.startswith("    //"):
                    prev_line = line

        # optimise pha, pla
        for block in self.block_iterator(code_lines):
            for line in block:
                for next_line in code_lines[line.nr + 1:]:
                    if next_line.src.startswith("    //"):
                        continue
                    break

                if line.src.startswith("    pha") and next_line.src.startswith("    pla"):
                    line.src = "    // %s" % line.src[4:]
                    next_line.src = "    // %s" % next_line.src[4:]

        return code_lines

    def line_iterator(self):
        line_nr = 0
        for node in self.nodes:
            for line in node.src:
                yield DotWiz(nr=line_nr, src=line)
                line_nr += 1

    def block_iterator(self, lines):
        block = []
        for line in lines:
            if line.src.startswith("    "):
                block.append(line)
            elif "{" in line.src or "}" in line.src:
                block.append(line)
            else:
                yield block
                block = [line]
        if block:
            yield block



if __name__ == "__main__":
    lexer = Lexer("examples.worm")
    tokens = lexer.scan()

    parser = Parser(tokens)
    shared, ast, local = parser.parse()

    print(ast.tree())

    compiler = Compiler(shared, ast, local)
    variables, nodes = compiler.compile()

    optimizer = Optimizer(nodes)
    code = optimizer.optimize()
    
    print("=== AST ===")
    for v in ast:
        print(v.tree())
    
    print("=== COMPILED VARIABLES ===")
    for k, v in variables.items():
        print(v.tree())

    print("===SOURCE CODE===")
    with open("test.asm", "w") as f:
        f.write("// code segment\n")
        for line in code:
            # print("%4d %s" % (line.nr, line.src))
            f.write("%s\n" % line.src)

        f.write("// variable segment\n")
        for ident, node in variables.items():
            f.write("\n".join(node.src))
