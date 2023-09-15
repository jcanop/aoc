from itertools import permutations
import re

# --- Constants --
INPUT_FILE = "../input/input.txt"
REGEX = r"(.+)? would (gain|lose) (\d+) happiness units by sitting next to (.+)\."

# --- Functions ---
def calculate_happiness(persons, happiness):
    _max = float("-inf")
    for p in permutations(persons):
        h = 0
        for i in range(0, len(p)):
            n1 = p[i]
            n2 = p[(i + 1) % len(p)]
            h += happiness[n1 + "-" + n2]
            h += happiness[n2 + "-" + n1]
        _max = max(_max, h)
    return _max

# --- Variables ---
persons = set()
happiness = {}

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    cap = re.search(REGEX, line)
    persons.add(cap[1])
    persons.add(cap[4])
    h = int(cap[3])
    if cap[2] == "lose": h *= -1
    happiness[cap[1] + "-" + cap[4]] = h
file.close()
persons = list(persons)

# --- Puzzle 1 ---
_max = calculate_happiness(persons, happiness)
print(f"1. Max change in happiness: {_max:,d}")

# --- Puzzle 2 ---
persons.append("me")
for p in persons:
    happiness[p + "-me"] = 0
    happiness["me-" + p] = 0
_max = calculate_happiness(persons, happiness)
print(f"2. Max change in happiness: {_max:,d}")
