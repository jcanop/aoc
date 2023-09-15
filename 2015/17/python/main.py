from itertools import combinations

# --- Constants --
INPUT_FILE = "../input/input.txt"
LITERS = 150

# --- Variables ---
ls = []
count = 0
_map = {}

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    ls.append(int(line))
file.close()

# --- Combinations ---
for i in range(1, len(ls) + 1):
    for c in combinations(ls, i):
        total = sum(c)
        if total == LITERS:
            count += 1
            if len(c) not in _map: _map[len(c)] = 0
            _map[len(c)] += 1

# --- Puzzle 1 ---
print(f"1. Combinations of containers: {count:,d}")

# --- Puzzle 2 ---
num = _map[min(_map.keys())]
print(f"2. Number of different ways: {num:,d}")
