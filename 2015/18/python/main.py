# --- Constants --
INPUT_FILE = "../input/input.txt"
STEPS = 100
ON = '#'
OFF = '.'

# --- Functions ---
def count_on_neighbors(grid, x, y):
    count = 0
    for j in range(-1, 2):
        if j == -1 and y == 0: continue
        if j == 1  and y == len(grid) - 1: continue
        ny = y + j
        for i in range(-1, 2):
            if i == 0 and j == 0: continue
            if i == -1 and x == 0: continue
            if i == 1 and x == len(grid[0]) - 1: continue
            nx = x + i
            if grid[ny][nx] == ON: count += 1
    return count

def animation(data, puzzle):
    # --- Init the two grids ---
    grid1 = [r[:] for r in data]
    grid2 = [r[:] for r in data]
    xlen = len(data[0])
    ylen = len(data)

    # --- Run the animation ---
    grid = grid1
    buffer = grid2
    if puzzle == 2:
        grid[0][0] = ON
        grid[0][xlen - 1] = ON
        grid[ylen - 1][0] = ON
        grid[ylen - 1][xlen - 1] = ON
    for step in range(STEPS):
        for y in range(ylen):
            for x in range(xlen):
                if puzzle == 2 and (
                    (x == 0 and y == 0) or
                    (x == xlen - 1 and y == 0) or
                    (x == 0 and y == ylen -1) or
                    (x == xlen - 1 and y == ylen - 1)
                ): buffer[y][x] = ON; continue
                c = count_on_neighbors(grid, x, y)
                if grid[y][x] == ON:
                    buffer[y][x] = ON if c == 2 or c == 3 else OFF
                else:
                    buffer[y][x] = ON if c == 3 else OFF
        if step % 2 == 0:
            grid = grid2
            buffer = grid1
        else:
            grid = grid1
            buffer = grid2

    # --- Return lights on ---
    count = 0
    for y in range(ylen):
        for x in range(xlen):
            if grid[y][x] == ON: count += 1
    return count

# --- Variables ---
data = []

# --- Read and Parse the input file --
file = open(INPUT_FILE, "r")
while True:
    line = file.readline()
    if not line: break
    line = line.strip()

    data.append([*line])
file.close()

# --- Puzzle 1 ---
count = animation(data, 1)
print(f"1. Number of lights on: {count:,d}")

# --- Puzzle 2 ---
count = animation(data, 2)
print(f"2. Number of lights on: {count:,d}")
