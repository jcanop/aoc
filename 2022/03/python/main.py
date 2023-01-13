# --- Constants ---
INPUT_FILE = "../input/input.txt"

# --- Find the duplciated character ---
def find_duplicate(a, b, c = None):
    for ac in a:
        for bc in b:
            if ac == bc:
                if c == None: return ac
                for cc in c:
                    if bc == cc:
                        return cc

#--- Get the prioirty value of a character ---
def get_priority(c):
    if c >= "a" and c <= "z": return ord(c) - ord("a") + 1
    if c >= "A" and c <= "Z": return ord(c) - ord("A") + 27
    return None

# --- Variables ---
totals = [0, 0]
lines = ["", "", ""]
index = 0

# --- Read the input file ---
file = open(INPUT_FILE)
while True:
    line = file.readline()
    if not line:
        break

    line = line.strip()

    # --- Puzzle 1 ---
    n = int(len(line) / 2)
    s1 = line[0:n]
    s2 = line[n:]
    c = find_duplicate(s1, s2)
    totals[0] += get_priority(c)

    # --- Puzzle 2 ---
    lines[index] = line
    index += 1
    if index == 3:
        c = find_duplicate(lines[0], lines[1], lines[2])
        totals[1] += get_priority(c)
        index = 0

print(f"1. Total sum of the priorities: {totals[0]:,d}")
print(f"1. Total sum of the item types: {totals[1]:,d}")
