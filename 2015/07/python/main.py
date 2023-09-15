# --- Constants --
INPUT_FILE = "../input/input.txt"

# --- Functions ---
def contains(a, v):
    try:
        a.index(v)
        return True
    except ValueError:
        pass
    return False

def solve(op, wires, cache):
    if op.isdigit(): return int(op)
    if op in cache: return cache[op]

    ls = wires[op].split()
    r = None
    if contains(ls, "AND"):      r = solve(ls[0], wires, cache) &  solve(ls[2], wires, cache)
    elif contains(ls, "OR"):     r = solve(ls[0], wires, cache) |  solve(ls[2], wires, cache)
    elif contains(ls, "LSHIFT"): r = solve(ls[0], wires, cache) << solve(ls[2], wires, cache)
    elif contains(ls, "RSHIFT"): r = solve(ls[0], wires, cache) >> solve(ls[2], wires, cache)
    elif contains(ls, "NOT"):    r = ~ solve(ls[1], wires, cache)
    else: r = solve(ls[0], wires, cache)
    cache[op] = r
    return r

# --- Variables ---
wires = {}
cache = {}

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    ls = line.split(" -> ")
    wires[ls[1].strip()] = ls[0].strip()
file.close()

# --- Puzzle 1 ---
r = solve("a", wires, cache)
print(f"1. Wire a: {r:,d}")

# --- Puzzle 2 ---
wires["b"] = str(r)
cache.clear()
r = solve("a", wires, cache)
print(f"2. Wire a: {r:,d}")
