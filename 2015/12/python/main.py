import json
import re

# --- Constants --
INPUT_FILE = "../input/input.txt"
REGEX = r"(-?\d+)"

# --- Functions ---
def sum(js):
    t = type(js)
    if t == int:
        return js
    elif t == list:
        total = 0
        for v in js: total += sum(v)
        return total
    elif t == dict:
        values = list(js.values())
        try:
            values.index("red")
            return 0
        except ValueError:
            total = 0
            for v in values: total += sum(v)
            return total
    return 0

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
text = file.read().strip()
file.close()

# --- Puzzle 1 ---
total = 0
for cap in re.findall(REGEX, text): total += int(cap)
print(f"1. Sum of all numbers: {total:,d}")

# --- Puzzle 2 ---
total = sum(json.loads(text))
print(f"2. Sum of all numbers: {total:,d}")
