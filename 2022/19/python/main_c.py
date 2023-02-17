# --- This solution uses a C library to burst the performance; please ---
# --- refer to the README.md file for instructions on compiling and   ---
# --- running this version of the solution.                           ---
from math import prod
from ctypes import *
from lib import ORE, Blueprint as BP
libc = CDLL("libc.so")

# --- Constants ---
INPUT_FILE = "../input/input.txt"

# --- C Lang Structure ---
class Blueprint(Structure):
    _fields_ = [("id", c_int),
                ("costs", c_int * 3 * 4),
                ("max_ore", c_int)]

# --- Read and parse the input file ---
bps = []
for b in BP.load(INPUT_FILE):
    # --- Transforms each Python struct into a C struct ---
    bp = Blueprint()
    bp.id = b.id
    for i in range(4):
        for j in range(3):
            bp.costs[i][j] = b.costs[i][j]
    bp.max_ore = b.max_costs[ORE]
    bps.append(bp)

# --- Puzzle 1 ---
total = sum([ bp.id * libc.get_max(byref(bp), 24) for bp in bps ])
print(f"1. Total sum of quality levels: {total:,d}");

# --- Puzzle 2 ---
total = prod([ libc.get_max(byref(bp), 32) for bp in bps[0:3] ])
print(f"2. Total of multiply the first 3 blueprints max values: {total:,d}");
