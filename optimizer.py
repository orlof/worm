from dotwiz import DotWiz

from lexer import Lexer, operators_arithmetic, operators_comparison
from nodes import *
from parser import Parser, AstNode, AstList
from compiler import Compiler

class Optimizer:
    def __init__(self, code):
        self.code = code

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
            prev_line = DotWiz(src="")
            for line in block:
                if line.src.startswith("    pla") and prev_line.src.startswith("    pha"):
                    line.src = "    // %s" % line.src[4:]
                    prev_line.src = "    // %s" % prev_line.src[4:]
                prev_line = line

        # optimise sta, lda
        for block in self.block_iterator(code_lines):
            prev_line = DotWiz(src="")
            for line in block:
                if line.src.replace("lda", "sta") == prev_line.src:
                    line.src = "    // %s" % line.src[4:]
                prev_line = line


        return code_lines

    def line_iterator(self):
        for line_nr, line in enumerate(self.code):
            if not line.strip().startswith("//"):
                yield DotWiz(nr=line_nr, src=line)

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

    print("=== SHARED ===")
    print(shared.tree())

    print("=== LOCAL ===")
    print(local.tree())
    
    print("=== AST ===")
    print(ast.tree())

    compiler = Compiler(shared, ast, local)
    code = compiler.compile()

    optimizer = Optimizer(code)
    code = optimizer.optimize()
    
    print("===SOURCE CODE===")
    with open("test.asm", "w") as f:
        for line in code:
            # print("%4d %s" % (line.nr, line.src))
            f.write("%s\n" % line.src)
