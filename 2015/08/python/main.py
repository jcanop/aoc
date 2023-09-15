# --- Constants --
INPUT_FILE = "../input/input.txt"

# --- Functions ---
def count_mem(s):
    total = len(s) - 2;
    ls = [*s]
    i = 1
    while i < len(ls) - 2:
        if ls[i] == "\\":
            if ls[i + 1] == "\\" or ls[i + 1] == '"':
                total -= 1
                i += 1
            elif ls[i + 1] == "x":
                total -= 3
                i += 3
        i += 1
    return total

def encode(s):
    ls = ['"']
    for c in [*s]:
        if c == '"' or c == '\\': ls.append('\\')
        ls.append(c)
    ls.append('"')
    return "".join(ls)

# --- Variables ---
total1 = 0
total2 = 0

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    # --- Puzzle 1 ---
    total1 += len(line) - count_mem(line)

    # --- Puzzle 2 ---
    enc = encode(line)
    total2 += len(enc) - count_mem(enc)
file.close()

# --- Results ---
print(f"1. Number of characters in memory: {total1:,d}")
print(f"2. Number of characters in memory: {total2:,d}")
