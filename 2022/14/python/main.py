from enum import Enum
from lib import Map

# --- Constants --
INPUT_FILE = "../input/input.txt"

# --- Read and parse the input file ---
_map = Map(INPUT_FILE)

# --- Puzzle 1 ---
_map.sim(1)
total = _map.count_sand()
print(f"1. Total units of sand: {total:,d}")

# --- Puzzle 2 ---
_map.sim(2)
total = _map.count_sand()
print(f"2. Total units of sand: {total:,d}")
