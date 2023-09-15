import random
import re

# --- Constants --
INPUT_FILE = "../input/input.txt"
REGEX = r"^([a-zA-Z]+) => ([a-zA-Z]+)$"

# --- Variables ---
_map = {}
pairs = []
molecule = ""
first = True

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    if len(line) == 0:
        first = False
    else:
        if first:
            cap = re.search(REGEX, line)
            if cap[1] not in _map: _map[cap[1]] = []
            _map[cap[1]].append(cap[2])
            pairs.append((cap[1], cap[2]))
        else:
            molecule = line
file.close()

# --- Puzzle 1 ---
regex = "(" + "|".join(_map.keys()) + ")"
_set = set()
pattern = re.compile(regex)
for match in pattern.finditer(molecule):
    i = match.start()
    j = match.end()
    v = match[0]
    for x in _map[v]:
        s = molecule[0:i] + x + molecule[j:]
        _set.add(molecule[0:i] + x + molecule[j:])
print(f"1. Distinct molecules can be created: {len(_set):,d}")

# --- Puzzle 2 ---
target = molecule[:]
steps = 0
while target != "e":
    change = False
    for p in pairs:
        if p[1] in target:
            target = target.replace(p[1], p[0], 1)
            change = True
            break

    if not change:
        random.shuffle(pairs)
        target = molecule[:]
        steps = 0
        continue;

    steps += 1
print(f"2. Fewest number of steps: {steps:,d}")
