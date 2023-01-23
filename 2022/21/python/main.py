from lib import Resolver

# --- Constants ---
INPUT_FILE = "../input/input.txt"
ROOT = "root"
ME = "humn"

# --- Puzzle 1 ---
resolver = Resolver(INPUT_FILE)
result = resolver.solve_for(ROOT)
print(f"1. Number that will the monkey root yell: {result:,d}")

# --- Puzzle 2 ---
resolver = Resolver(INPUT_FILE)
resolver.update_for_puzzle2(ROOT, ME)
result = resolver.solve_for(ME)
print(f"2. Number that I yell to pass root's equality test: {result:,d}")
