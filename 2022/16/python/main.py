from lib import Layout

# --- Constants ---
INPUT_FILE = "../input/input.txt"

layout = Layout(INPUT_FILE)

# ---  Puzzle 1 ---
total = layout.dfs1()
print(f"1. Total pressuere released: {total:,d}")

# --- Puzzle 2 ---
total = total = layout.dfs2()
print(f"2. Total pressure released with an elephant: {total:,d}")
