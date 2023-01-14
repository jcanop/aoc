import re

# --- Constants ---
INPUT_FILE = "../input/input.txt"
REGEX = r"move (\d+) from (\d+) to (\d+)"

# --- Read and parse input file ---
file = open(INPUT_FILE)
text = file.read()
n = text.index("\n\n")

lines = text[0:n].split("\n")
lines.pop()
count = len(lines[0]) // 4 + 1
queues_1 = [""] * count
queues_2 = [""] * count

for line in lines:
    for i in range(count):
        p = i * 4 + 1
        c = line[p:p + 1]
        if c != " ":
            queues_1[i] += c
            queues_2[i] += c

for line in text.strip()[n + 2:].split("\n"):
    cap = re.search(REGEX, line)
    cn = int(cap.group(1))
    fm = int(cap.group(2)) - 1
    to = int(cap.group(3)) - 1

    # --- Puzzle 1 ---
    for i in range(cn):
        c = queues_1[fm][0:1]
        queues_1[fm] = queues_1[fm][1:]
        queues_1[to] = c + queues_1[to]

    # --- Puzzle 2 ---
    c = queues_2[fm][0:cn]
    queues_2[fm] = queues_2[fm][cn:]
    queues_2[to] = c + queues_2[to]

# --- Print resutls ---
top = "".join([ s[0] for s in queues_1 ])
print(f"1. Crates on the top of each stack: {top}")

top = "".join([ s[0] for s in queues_2 ])
print(f"2. Crates on the top of each stack: {top}")
