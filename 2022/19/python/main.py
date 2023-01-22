# --- This solution needs to be completed; in theory, the principal   ---
# --- function works but runs so slow (45 min for one Blueprint). So  ---
# --- I am saving this for the moment.                                ---
from lib import Blueprint, get_max

# --- Constants ---
INPUT_FILE = "../input/input.test"

# --- Read and parse the input file ---
bps = Blueprint.load(INPUT_FILE)

# --- Puzzle 1 ---
# map(lambda bp: bp.id * get_max(bp, 24))
print(get_max(bps[0], 24))
