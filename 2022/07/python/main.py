from functools import reduce
import re

# --- Constants ---
INPUT_FILE = "../input/input.txt"
LIMIT=100000
DISK_SIZE=70000000
UPDATE_SIZE=30000000

# --- Read the input file ---
data = {}
dirs = []
level = 0
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    if line == "$ cd ..":
        level -= 1
        dirs.pop()
    elif line.startswith("$ cd"):
        dirs.append(line[5:])
        path = "/".join(dirs)
        data[path] = 0
        level += 1
    elif re.match(r"^\d+.+$", line):
        i = line.index(" ")
        size = int(line[0:i])
        ls = list(dirs)
        while len(ls) > 0:
            path = "/".join(ls)
            ls.pop()
            data[path] += size

# --- Puzzle 1 ---
sizes = data.values()
total = reduce(lambda acc, val: acc + val, filter(lambda n: n <= LIMIT, sizes))
print(f"1. The sum of the total size of the directories under {LIMIT:,d} is {total:,d}")

# --- Puzzle 2 ---
need = UPDATE_SIZE - DISK_SIZE + data["/"]
total = sorted(list(filter(lambda n: n >= need, sizes)))[0]
print(f"2. The total size of the smalles directory needed is {total:,d}")
