# --- Constants ---
INPUT_FILE = "../input/input.txt"

# --- Checks if a tree is visible from outside ---
def is_visible(grid, x, y):
    for i in range(y + 1, len(grid)):
        if grid[i][x] >= grid[y][x]: break
        if i == len(grid) - 1: return True
    for i in reversed(range(y)):
        if grid[i][x] >= grid[y][x]: break
        if i == 0: return True
    for i in range(x + 1, len(grid[0])):
        if grid[y][i] >= grid[y][x]: break
        if i == len(grid[9]) - 1: return True
    for i in reversed(range(x)):
        if grid[y][i] >= grid[y][x]: return False
        if i == 0: return True
    raise Exception("Unreachable!")

# --- Calculates the scenic score ---
def scenic_score(grid, x, y):
    a = 1
    for i in range(y + 1, len(grid) - 1):
        if grid[i][x] >= grid[y][x]: break
        a += 1
    b = 1
    for i in reversed(range(1, y)):
        if grid[i][x] >= grid[y][x]: break
        b += 1
    c = 1
    for i in range(x + 1, len(grid[0]) - 1):
        if grid[y][i] >= grid[y][x]: break
        c += 1
    d = 1
    for i in reversed(range(1, x)):
        if grid[y][i] >= grid[y][x]: return False
        d += 1
    return a * b * c * d

# --- Read the input file ---
grid = [];

file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    grid.append([ int(x) for x in line ])

# ---- Puzzle 1 ---
total = (len(grid) + len(grid[0]) - 2) * 2
for y in range(1, len(grid) - 1):
    for x in range(1, len(grid[0]) - 1):
        if is_visible(grid, x, y): total += 1
print(f"1. Trees that are visible from outside the grid: {total:,d}")

# --- Puzzle 2 ---
score = 0
for y in range(1, len(grid) - 1):
    for x in range(1, len(grid[0]) - 1):
        score = max(score, scenic_score(grid, x, y))
print(f"2. The highest scenic score is: {score:,d}")
