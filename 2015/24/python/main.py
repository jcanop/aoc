from functools import reduce
from itertools import combinations

# --- Constants --
INPUT_FILE = "../input/input.txt"

# --- Functions ---
def quantum_entanglement(ls, groups):
    target = sum(ls) / groups
    _min = float("inf")

    for i in range(1, len(ls)):
        for c in combinations(ls, i):
            if sum(c) == target:
                _min = min(_min, reduce(lambda a, b: a * b, c))
        if _min != float("inf"): return _min

    raise Exception("Not found!")

# --- Variables ---
ls = []

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    ls.append(int(line))
file.close()

# --- Puzzle 1 ---
qe = quantum_entanglement(ls, 3)
print(f"1. Quantum entanglement: {qe:,d}")

# --- Puzzle 2 ---
qe = quantum_entanglement(ls, 4)
print(f"2. Quantum entanglement: {qe:,d}")
