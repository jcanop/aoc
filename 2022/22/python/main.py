from lib import Layout, move_grid, move_cube

# --- Constants ---
INPUT_FILE = "../input/input.txt"

# --- Read the input file ---
with open(INPUT_FILE, "r") as file:
    text = file.read().rstrip()
    i = text.index("\n\n")
    grid = text[0:i]
    path = text[i + 2:]

# --- Puzzle 1 ---
layout = Layout(grid)
layout.path(path, move_grid)
password = layout.get_password()
print(f"1. Password: {password}")

# --- Puzzle 2 ---
layout = Layout(grid)
layout.path(path, move_cube)
password = layout.get_password()
print(f"2. Password: {password}")
