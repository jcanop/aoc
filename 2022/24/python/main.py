from layout import Layout

# --- Constants ---
INPUT_FILE = "../input/input.txt"

# --- Read and parse the input file ---
layout = Layout(INPUT_FILE)
start = (1, 0)
end = (layout.width - 2, layout.height - 1)

# --- Puzzle 1 ---
time = layout.search(start, end, 0)
print(f"1. Minutes to reach the goal: {time:,d}")

# --- Puzzle 2 ---
time = layout.search(end, start, time)
time = layout.search(start, end, time)
print(f"2. Minutes to reach the goal, go back to the start, then reach the goal again: {time:,d}")
