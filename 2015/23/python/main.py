# --- Constants --
INPUT_FILE = "../input/input.txt"

# --- Classes ---
class CPU:
    def __init__(self):
        self.a = 0
        self.b = 0

    def set(self, reg, value):
        if reg == 'a': self.a = value
        elif reg == 'b': self.b = value
        else: raise Exception("Invalid registry")

    def get(self, reg):
        if reg == 'a': return self.a
        if reg == 'b': return self.b
        raise Exception("Invalid registry")

    def exec(self, code):
        i = 0;
        while i < len(code):
            s = code[i].split(" ");
            s[1] = s[1].replace(",", "")
            op = s[0]
            if   op == "hlf": self.set(s[1], self.get(s[1]) / 2); i += 1
            elif op == "tpl": self.set(s[1], self.get(s[1]) * 3); i += 1
            elif op == "inc": self.set(s[1], self.get(s[1]) + 1); i += 1
            elif op == "jmp": i += int(s[1])
            elif op == "jie": i += int(s[2]) if self.get(s[1]) % 2 == 0 else 1
            elif op == "jio": i += int(s[2]) if self.get(s[1]) == 1 else 1
            else: raise Exception("Invlaid op")

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
text = file.read().strip()
code = text.split("\n")

# --- Puzzle 1 ---
cpu = CPU()
cpu.exec(code)
print(f"1. Register b: {cpu.b:,d}")

# --- Puzzle 2 ---
cpu = CPU()
cpu.a = 1
cpu.exec(code)
print(f"2. Register b: {cpu.b:,d}")
