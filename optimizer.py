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

                    if lda_line.src.startswith("    lda"):
                        self.code[line.nr] = "%s // optimized" % lda_line.src
                        self.code[lda_line.nr] = "    // %s" % lda_line.src[4:]
                        self.code[pha_line.nr] = "    // %s" % pha_line.src[4:]

                prev_line = line

        # optimise pha, pla
        code_lines = list(self.line_iterator())
        for block in self.block_iterator(code_lines):
            prev_line = DotWiz(src="")
            for line in block:
                if line.src.startswith("    pla") and prev_line.src.startswith("    pha"):
                    self.code[line.nr] = "    // %s" % line.src[4:]
                    self.code[prev_line.nr] = "    // %s" % prev_line.src[4:]
                prev_line = line

        # optimise sta, lda
        code_lines = list(self.line_iterator())
        for block in self.block_iterator(code_lines):
            prev_line = DotWiz(src="")
            for line in block:
                if "lda" in line.src and line.src.replace("lda", "sta") == prev_line.src:
                    self.code[line.nr] = "    // %s" % line.src[4:]
                prev_line = line

        # optimise rts, rts
        code_lines = list(self.line_iterator())
        for block in self.block_iterator(code_lines):
            prev_line = DotWiz(src="")
            for line in block:
                if "rts" in line.src and "rts" in prev_line.src:
                    self.code[line.nr] = "    // %s" % line.src[4:]
                prev_line = line

        # optimise jmp to next instruction
        code_lines = list(self.line_iterator2())
        prev_line = DotWiz(src="")
        for line in code_lines:
            tokens = prev_line.src.strip().split()
            if len(tokens) > 1 and tokens[0] in ("jmp", "bcc", "bcs", "beq", "bne", "bmi", "bpl", "bvc", "bvs") and line.src.strip().startswith(tokens[1]+":"):
                self.code[prev_line.nr] = self.code[prev_line.nr].replace("jmp", "// jmp")
            prev_line = line

        return self.code

    def line_iterator(self):
        for line_nr, line in enumerate(self.code):
            if line and not line.strip().startswith("{") and not line.strip().startswith("}") and (not line.strip().startswith("//")):
                yield DotWiz(nr=line_nr, src=line)

    def line_iterator2(self):
        for line_nr, line in enumerate(self.code):
            if line and not line.strip().startswith("//"):
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
    shared, literals, ast, local = parser.parse()

    print("=== SHARED ===")
    print(shared.tree())

    print("=== LITERALS ===")
    print(literals.tree())

    print("=== LOCAL ===")
    print(local.tree())
    
    print("=== AST ===")
    print(ast.tree())

    compiler = Compiler(shared, literals, ast, local)
    code = compiler.compile()

    optimizer = Optimizer(code)
    code = optimizer.optimize()
    
    print("===SOURCE CODE===")
    with open("test.asm", "w") as f:
        for line in code:
            # print("%4d %s" % (line.nr, line.src))
            f.write("%s\n" % line)
