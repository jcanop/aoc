from enum import Enum

# --- Constants ---
INPUT_FILE = "../input/input.txt"

# --- Classes ---
class Operation(Enum):
    NOOP = "noop"
    ADDX = "addx"

    @classmethod
    def from_value(cls, value):
        for e in list(cls):
            if e.value == value: return e
        raise Exception("Invalid argument: " + value)

class CPU:
    def __init__(self, program):
        self.program = program
        self.registry = 1
        self.command = None

    def tick(self):
        if self.command is None:
            cmd = self.program.pop(0)
            if cmd[0] == Operation.NOOP: return len(self.program) > 0
            if cmd[0] == Operation.ADDX:
                self.command = cmd
                return True
            raise Exception("Unsupported command: " + cmd[0])
        self.registry += self.command[1]
        self.command = None
        return len(self.program) > 0

# --- Variables ---
program = []
cycle = 1
mark  = 20
total = 0
buffer = ""

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    cmd = Operation.from_value(line[0:4])
    val = 0
    if cmd == Operation.ADDX:
        val = int(line[5:])
    program.append((cmd, val))

cpu = CPU(program)
while True:
    # --- Puzzle 1 ---
    if cycle == mark:
        total += cpu.registry * mark
        mark += 40

    # --- Puzzle 2 ---
    p = (cycle - 1) % 40
    c = " "
    if p >= cpu.registry - 1 and p <= cpu.registry + 1: c = "#"
    buffer += c;
    if cycle % 40 == 0: buffer += "\n"

    # --- Both ---
    cycle += 1
    if cpu.tick() == False: break

# --- Puzzle 1 ---
print(f"1. The sum of the signal strenghts is: {total:,d}")

# --- Puzzle 2 ---
print(f"2. Image on the CRT")
print(buffer)
