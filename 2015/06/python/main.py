import re

# --- Constants --
INPUT_FILE = "../input/input.txt"
LEN = 1000
REGEX = r"(toggle|turn on|turn off) (\d+),(\d+) through (\d+),(\d+)"

# --- Variables ---
map1 = [[False for i in range(LEN)] for j in range(LEN)]
map2 = [[0 for i in range(LEN)] for j in range(LEN)]

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    cap = re.search(REGEX, line)
    op = cap[1]
    x1 = int(cap[2])
    y1 = int(cap[3])
    x2 = int(cap[4])
    y2 = int(cap[5])

    for y in range(y1, y2 + 1):
        for x in range(x1, x2 + 1):
            if op == "toggle": map1[x][y] = not map1[x][y]; map2[x][y] += 2
            elif op == "turn on": map1[x][y] = True; map2[x][y] += 1
            elif op == "turn off": map1[x][y] = False; map2[x][y] = max(map2[x][y] - 1, 0)
            else: raise Exception("Unsupported op")

file.close()


# --- Results ---
count1 = 0
count2 = 0
for x in range(0, LEN):
    for y in range(0, LEN):
        if map1[x][y]: count1 += 1
        count2 += map2[x][y]

print(f"1. Lights lit: {count1:,d}")
print(f"2. Total brightness: {count2:,d}")
