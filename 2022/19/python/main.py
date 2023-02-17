# --- This solution is pure Python implementation and can take a while ---
# --- to get the answer (20 min. in my testing machine).               ---
from math import prod
from lib import Blueprint, get_max

# --- Constants ---
INPUT_FILE = "../input/input.txt"

def task(bps, results, idx):
    results[idx] = get_max(bps[idx], 32)
    print("> ", results[idx])

# --- Read and parse the input file ---
bps = Blueprint.load(INPUT_FILE)

# --- Puzzle 1 ---
total = sum([ bp.id * get_max(bp, 24) for bp in bps ])
print(f"1. Total sum of quality levels: {total:,d}");

# --- Puzzle 2 ---
total = prod([ get_max(bp, 32) for bp in bps[0:3] ])
print(f"2. Total of multiply the first 3 blueprints max values: {total:,d}");
